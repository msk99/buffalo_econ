# herd_cohort.R -- monthly cohort herd projection with a steady-state policy.
#
# Policy (owner decision, Aug 2026, see docs/DECISIONS_2026-08_build.md #1):
# the adult herd is held FIXED at the target size. Culling is the balancing
# variable: all retained heifers that reach first calving join the herd, and
# any excess over the target is culled (sold at salvage). Before the home-bred
# pipeline matures, deaths and culls are replaced by purchasing adults, so the
# herd is genuinely fixed rather than sagging.
#
# The projection is an expected value over a population: animal counts are
# fractional throughout and rounded only for display (plan 3.3).
#
# Structure per month:
#   adult cohorts   (entry month, count)   -> in-milk timing per cohort:
#                   freshly calved on entry, in milk for lactation_days, dry to
#                   the end of the first calving interval, steady-state
#                   fraction thereafter (cohorts past one interval are merged
#                   into a single settled pool)
#   parity groups   (1, 2-3, 4, 5+)        -> yield multiplier; culls are drawn
#                   from parity 4+ only (voluntary culling targets older stock)
#   female pipeline (ages 0..AFC-1 months) -> calf phase then heifer phase,
#                   with class-specific mortality; matures at first calving
#   male pipeline   (ages 0..calf phase)   -> sold at the end of the calf phase

library(data.table)

MONTHS_PER_YEAR <- 12L


#' Project the herd month by month
#'
#' @param p buffalo_params from scale_parameters(): includes herd_size
#' @param years projection horizon in years (5 or 10)
#' @return data.table, one row per month
project_herd <- function(p, years) {

  n_months <- years * MONTHS_PER_YEAR
  N        <- p$herd_size

  ci_m   <- p$calving_interval_days / DAYS_PER_MONTH   # calving interval, months
  lact_m <- p$lactation_days        / DAYS_PER_MONTH   # lactation, months
  f_ss   <- p$fraction_in_milk                          # steady-state in-milk share

  # monthly exit rates from annual ones
  m_rate <- function(annual) 1 - (1 - annual)^(1 / 12)
  mort_a_m <- m_rate(p$mortality_rate_adult)
  mort_c_m <- m_rate(p$mortality_rate_calf)
  mort_h_m <- m_rate(p$mortality_rate_heifer)
  cull_m   <- m_rate(p$culling_rate)

  calf_months <- max(1L, as.integer(round(p$calf_feeding_days / DAYS_PER_MONTH)))  # 6
  afc_months  <- as.integer(round(p$age_first_calving_months))                     # 36

  # purchase plan: equal batches, interval months apart, starting month 1
  n_batches <- max(1L, as.integer(p$staggered_purchase_batches))
  interval  <- max(1L, as.integer(p$staggered_purchase_interval_months))
  batch_months <- 1L + (seq_len(n_batches) - 1L) * interval
  batch_size   <- N / n_batches

  # expected in-milk share of a cohort during the month [e, e+1) after entry:
  # in milk for lact_m months from entry (fresh calver), dry to the end of the
  # first calving interval, steady-state fraction thereafter
  cohort_in_milk <- function(e) {
    if (e >= ci_m) return(f_ss)
    frac_settled <- max(0, (e + 1) - ci_m)              # part-month past the interval
    max(0, min(e + 1, lact_m) - e) + frac_settled * f_ss
  }

  # ---- state ---------------------------------------------------------------
  cohorts <- data.table(entry = integer(0), n = numeric(0))
  settled <- 0                        # adults past their first calving interval
  parity  <- c(p1 = 0, p23 = 0, p4 = 0, p5p = 0)
  fem     <- numeric(afc_months)      # females by age month 0..afc-1
  male    <- numeric(calf_months)     # males by age month 0..calf_months-1
  purchased_pool <- 0                 # purchased adults still in the herd

  mults <- c(p$parity_yield_mult_1, p$parity_yield_mult_2_3,
             p$parity_yield_mult_4, p$parity_yield_mult_5_plus)

  rows <- vector("list", n_months)

  for (m in seq_len(n_months)) {

    target <- batch_size * sum(batch_months <= m)       # ramps N/2 -> N

    # ---- 1. initial purchase batches ---------------------------------------
    batch_buy <- if (m %in% batch_months) batch_size else 0
    if (batch_buy > 0) {
      cohorts <- rbind(cohorts, data.table(entry = m, n = batch_buy))
      parity["p23"]  <- parity[["p23"]] + batch_buy
      purchased_pool <- purchased_pool + batch_buy
    }

    A <- sum(cohorts$n) + settled

    # ---- 2. adult deaths ------------------------------------------------------
    deaths_a <- A * mort_a_m

    # ---- 3. pipeline aging and young-stock exits ----------------------------
    fem_next   <- numeric(afc_months)
    male_next  <- numeric(calf_months)
    deaths_y   <- 0
    maturities <- 0
    for (a in seq_len(afc_months) - 1L) {               # ages 0..afc-1
      surv <- if (a < calf_months) 1 - mort_c_m else 1 - mort_h_m
      kept <- fem[a + 1L] * surv
      deaths_y <- deaths_y + fem[a + 1L] * (1 - surv)
      if (a + 1L < afc_months) fem_next[a + 2L] <- kept
      else maturities <- kept                           # reaches first calving
    }
    male_sold <- 0
    for (a in seq_len(calf_months) - 1L) {
      kept <- male[a + 1L] * (1 - mort_c_m)
      deaths_y <- deaths_y + male[a + 1L] * mort_c_m
      if (a + 1L < calf_months) male_next[a + 2L] <- kept
      else male_sold <- kept                            # sold at end of calf phase
    }
    # non-retained females are sold at the same age as the male calves
    fem_sold <- 0
    if (calf_months < afc_months) {
      at_gate <- fem_next[calf_months + 1L]
      fem_sold <- at_gate * (1 - p$heifer_retention_pct)
      fem_next[calf_months + 1L] <- at_gate - fem_sold
    }

    # ---- 4. steady-state balance: culling is the balancing variable ---------
    # Voluntary culling (workbook culling_rate, parity 4+ only) never cuts so
    # deep that a replacement purchase is needed; extra culls then remove any
    # surplus over the target. Purchases only cover what the pipeline cannot.
    available   <- A - deaths_a + maturities
    eligible    <- parity[["p4"]] + parity[["p5p"]]
    culls_base  <- min(A * cull_m, eligible, max(0, available - target))
    culls_extra <- max(0, available - culls_base - target)
    culls       <- culls_base + culls_extra
    repl_buy    <- max(0, target - (available - culls))
    exits       <- deaths_a + culls

    # ---- 5. apply to cohorts, joiners, purchases ----------------------------
    if (A > 0) {
      keep <- 1 - exits / A
      cohorts[, n := n * keep]
      settled <- settled * keep
      deaths_purchased <- deaths_a * purchased_pool / A
      purchased_pool   <- max(0, purchased_pool * keep)
    } else deaths_purchased <- 0
    if (maturities > 0) cohorts <- rbind(cohorts, data.table(entry = m, n = maturities))
    if (repl_buy   > 0) {
      cohorts <- rbind(cohorts, data.table(entry = m, n = repl_buy))
      purchased_pool <- purchased_pool + repl_buy
    }

    # ---- 6. parity compartments ---------------------------------------------
    adv    <- 1 / ci_m                                  # calvings per adult per month
    move1  <- parity[["p1"]]  * adv
    move23 <- parity[["p23"]] * adv / 2                 # group spans two parities
    move4  <- parity[["p4"]]  * adv
    parity["p1"]  <- parity[["p1"]]  - move1
    parity["p23"] <- parity[["p23"]] + move1 - move23
    parity["p4"]  <- parity[["p4"]]  + move23 - move4
    parity["p5p"] <- parity[["p5p"]] + move4
    # deaths proportional; culls from 5+ then 4 then 2-3 then 1
    parity <- parity * (1 - deaths_a / max(A, 1e-9))
    take <- culls
    for (g in c("p5p", "p4", "p23", "p1")) {
      t_g <- min(parity[[g]], take)
      parity[g] <- parity[[g]] - t_g
      take <- take - t_g
      if (take <= 1e-12) break
    }
    parity["p1"]  <- parity[["p1"]]  + maturities       # first calvers
    parity["p23"] <- parity[["p23"]] + repl_buy         # bought 2nd-3rd lactation

    # ---- 7. milk -------------------------------------------------------------
    in_milk <- settled * f_ss
    if (nrow(cohorts) > 0) {
      in_milk <- in_milk +
        sum(cohorts$n * vapply(m - cohorts$entry, cohort_in_milk, numeric(1)))
    }
    A_end <- sum(cohorts$n) + settled
    psum  <- sum(parity)
    if (psum > 0) parity <- parity * A_end / psum        # keep the two views consistent
    mult_avg <- if (A_end > 0) sum(parity * mults) / A_end else 0
    milk_litres <- in_milk * p$yield_peak_litres_per_day * mult_avg * DAYS_PER_MONTH

    # merge cohorts that have completed their first interval into the settled pool
    done <- cohorts[, (m - entry) >= ci_m]
    if (any(done)) {
      settled <- settled + sum(cohorts$n[done])
      cohorts <- cohorts[!done]
    }

    # ---- 8. births and pipeline advance ---------------------------------------
    births        <- A_end / ci_m
    fem_next[1L]  <- births * p$sex_ratio_female
    male_next[1L] <- births * (1 - p$sex_ratio_female)
    fem  <- fem_next
    male <- male_next

    heifers <- sum(fem[(calf_months + 1L):afc_months])
    calves  <- sum(fem[1:calf_months]) + sum(male)

    rows[[m]] <- data.table(
      month = m, year = ceiling(m / 12),
      adults = A_end, in_milk = in_milk, dry_adults = A_end - in_milk,
      heifers = heifers, calves = calves,
      adult_equivalents = A_end + p$heifer_adult_equivalent * heifers +
                          p$calf_adult_equivalent * calves,
      milk_litres = milk_litres, parity_mult = mult_avg,
      births = births,
      male_calves_sold = male_sold, female_calves_sold = fem_sold,
      heifers_matured = maturities,
      culls_base = culls_base, culls_extra = culls_extra, culls = culls,
      deaths_adult = deaths_a, deaths_young = deaths_y,
      deaths_purchased = deaths_purchased,
      batch_purchases = batch_buy, replacement_purchases = repl_buy,
      purchased_share = if (A_end > 0) purchased_pool / A_end else 0
    )
  }

  out <- rbindlist(rows)
  setattr(out, "herd_target", N)
  out[]
}


#' Annual roll-up of a monthly herd projection
#'
#' Stocks are averaged over the year, flows are summed.
herd_annual <- function(herd_m) {
  stock <- c("adults", "in_milk", "dry_adults", "heifers", "calves",
             "adult_equivalents", "parity_mult", "purchased_share")
  flow  <- c("milk_litres", "births", "male_calves_sold", "female_calves_sold",
             "heifers_matured", "culls_base", "culls_extra", "culls",
             "deaths_adult", "deaths_young", "deaths_purchased",
             "batch_purchases", "replacement_purchases")
  a <- herd_m[, lapply(.SD, mean), by = year, .SDcols = stock]
  f <- herd_m[, lapply(.SD, sum),  by = year, .SDcols = flow]
  a[f, on = "year"][]
}
