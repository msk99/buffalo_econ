# params_scale.R -- turn anchored parameters into values for one herd size.
#
# There are no small, medium or large categories anywhere in the model: each
# parameter moves between its two anchors according to its declared scale_mode,
# and is held flat outside them.
#
#   constant  same at every herd size
#   log       interpolate on log(herd size); the default for unit costs and
#             prices, which follow economies of scale
#   linear    interpolate on herd size directly
#   derived   computed from other parameters (see derive_parameters)
#   capacity  whole units, computed by equipment_schedule(), not interpolated
#   regime    a mode chosen by the user, not a function of herd size
#
# Nothing is ever extrapolated. Above the large anchor every interpolated
# parameter holds its large value; below the small anchor it holds its small
# value. Costs still rise with herd size, because per-head values are
# multiplied by the herd and the capacity model keeps adding units.

library(data.table)

DAYS_PER_MONTH <- 30.42


#' Interpolation position of a herd size between the anchors, clamped to [0, 1]
#' @return 0 at or below the small anchor, 1 at or above the large anchor
anchor_position <- function(herd_size, anchors, mode = "log") {
  lo <- anchors[["small"]]
  hi <- anchors[["large"]]
  t <- if (mode == "log") {
    (log(herd_size) - log(lo)) / (log(hi) - log(lo))
  } else {
    (herd_size - lo) / (hi - lo)
  }
  min(max(t, 0), 1)   # clamp: never extrapolate
}


#' Apply the rounding rule declared for a parameter
apply_rounding <- function(x, rounding) {
  if (is.na(x) || !is.numeric(x)) return(x)
  switch(rounding %||% "none",
         int  = round(x),
         ceil = ceiling(x),
         x)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a


#' Scale a single parameter to a herd size
scale_one <- function(small, large, mode, rounding, herd_size, anchors) {
  if (mode == "constant") return(apply_rounding(small, rounding))
  if (is.na(small) || is.na(large)) return(NA_real_)
  t <- anchor_position(herd_size, anchors, mode = mode)
  apply_rounding(small + (large - small) * t, rounding)
}


#' Scale every parameter in the workbook to one herd size
#'
#' @param raw object from load_parameters()
#' @param herd_size number of adult buffaloes
#' @param overrides named list of user overrides, applied last
#' @param automation "mechanised" or "manual"; see equipment_schedule()
#' @return named list of parameters, plus the ration, equipment and herd size
scale_parameters <- function(raw, herd_size, overrides = list(), automation = "mechanised") {

  stopifnot(inherits(raw, "buffalo_params_raw"))
  if (!is.numeric(herd_size) || length(herd_size) != 1L || herd_size <= 0) {
    stop("herd_size must be a single positive number.", call. = FALSE)
  }

  p       <- raw$params
  anchors <- raw$anchors
  out     <- list()

  for (i in seq_len(nrow(p))) {
    nm   <- p$parameter[i]
    mode <- p$scale_mode[i]

    if (mode == "derived") {
      next                                    # filled in by derive_parameters()
    } else if (mode == "regime") {
      # A mode, not a number: a user choice with a size-appropriate default.
      # Where the anchors differ (e.g. loan tenure 5 vs 7 years), the default
      # follows the nearer anchor, split at the geometric mean of the two.
      use_large <- herd_size >= sqrt(anchors[["small"]] * anchors[["large"]])
      v_num <- if (use_large) p$large[i]     else p$small[i]
      v_raw <- if (use_large) p$large_raw[i] else p$small_raw[i]
      out[[nm]] <- if (is.na(v_num)) v_raw else apply_rounding(v_num, p$rounding[i])
    } else if (mode == "capacity") {
      out[[nm]] <- p$small[i]                 # thresholds; used by the capacity models
    } else {
      out[[nm]] <- scale_one(p$small[i], p$large[i], mode, p$rounding[i], herd_size, anchors)
    }
  }

  out$herd_size    <- herd_size
  out$anchor_small <- anchors[["small"]]
  out$anchor_large <- anchors[["large"]]
  out$at_anchor_ceiling <- herd_size >= anchors[["large"]]

  out <- derive_parameters(out)
  out$feed_cost_multiplier <- 1        # sensitivity hook on the whole ration

  # Ration and equipment are structural, not scalar.
  out$automation <- automation
  out$ration     <- ration_costs(raw, herd_size)
  out$equipment  <- equipment_schedule(raw, herd_size, automation)

  # User overrides win, and are recorded so a report can show what was changed.
  if (length(overrides) > 0) {
    unknown <- setdiff(names(overrides), names(out))
    if (length(unknown) > 0) {
      warning(sprintf("Override(s) not recognised as model parameters: %s",
                      paste(unknown, collapse = ", ")), call. = FALSE)
    }
    for (nm in names(overrides)) out[[nm]] <- overrides[[nm]]
    out$.overrides <- overrides
    # re-flow derived quantities so an override of e.g. yield or calving
    # interval reaches peak yield, dry days and the in-milk fraction
    out <- derive_parameters(out)
  }

  structure(out, class = c("buffalo_params", "list"))
}


#' Compute parameters that are calculated rather than entered
#'
#' Calving interval is the primary reproductive clock. Dry days follow from it,
#' so lactation, dry period and calving interval can never disagree.
derive_parameters <- function(p) {

  ci_days <- p$calving_interval_months * DAYS_PER_MONTH
  p$calving_interval_days <- ci_days
  p$dry_days <- ci_days - p$lactation_days

  if (!is.na(p$dry_days) && p$dry_days <= 0) {
    stop(sprintf(
      "Derived dry period is %.0f days: lactation (%.0f) does not fit inside the calving interval (%.0f).",
      p$dry_days, p$lactation_days, ci_days), call. = FALSE)
  }

  # Share of the adult herd in milk at any time, and calvings per adult per year.
  p$fraction_in_milk       <- p$lactation_days / ci_days
  p$calvings_per_adult_year <- 365 / ci_days

  # Yield is entered as the herd average across parities. Peak-parity yield is
  # back-solved so that the parity mix reproduces the entered average.
  shares <- c(p$parity_share_1, p$parity_share_2_3, p$parity_share_4, p$parity_share_5_plus)
  mults  <- c(p$parity_yield_mult_1, p$parity_yield_mult_2_3,
              p$parity_yield_mult_4, p$parity_yield_mult_5_plus)
  p$parity_weighted_mult <- sum(shares * mults)
  p$yield_peak_litres_per_day <-
    p$yield_herd_avg_litres_per_day / p$parity_weighted_mult

  p
}


#' Feed cost per animal class per day at a given herd size
#'
#' Component rows are interpolated on kg/day and rate/kg separately, so the
#' ration stays visible rather than collapsing into a single cost.
ration_costs <- function(raw, herd_size) {
  r <- raw$ration[is_total == FALSE]
  t <- anchor_position(herd_size, raw$anchors, mode = "log")

  dt <- data.table(
    animal_class = r$animal_class,
    feedstuff    = r$feedstuff,
    kg_per_day   = r$kg_small   + (r$kg_large   - r$kg_small)   * t,
    rate_per_kg  = r$rate_small + (r$rate_large - r$rate_small) * t
  )
  dt[, cost_per_day := kg_per_day * rate_per_kg]

  totals <- dt[, .(cost_per_day = sum(cost_per_day, na.rm = TRUE)), by = animal_class]
  list(detail = dt[], by_class = totals[],
       lookup = setNames(totals$cost_per_day, totals$animal_class))
}


#' Equipment units and capital cost at a given herd size
#'
#' units = 0 below min_herd or above max_herd, else ceiling(herd / capacity).
#' This is real lumpiness and is kept. What matters is that the thresholds are
#' spread apart, so steps never stack on one herd size.
#'
#' @param automation "mechanised" (default) buys the full list; "manual" skips
#'   rows flagged needs_automation = 1 (milking machines and coolers).
equipment_schedule <- function(raw, herd_size, automation = "mechanised") {
  e <- copy(raw$equipment)
  if (identical(automation, "manual")) e <- e[needs_automation == 0]

  e[, active := herd_size >= min_herd & (is.na(max_herd) | herd_size <= max_herd)]
  e[, units := fifelse(active, ceiling(herd_size / capacity_head), 0)]
  e[, total_cost := units * cost_per_unit]

  out <- e[units > 0, .(item, units, cost_per_unit, total_cost, life_years)]
  setattr(out, "total_capital", sum(out$total_cost))
  setattr(out, "herd_size", herd_size)
  out[]
}


#' Front-page defaults for the app, resolved at a herd size
#'
#' These must always be resolved from the workbook, never hard-coded, so that
#' workbook edits always reach the app.
app_defaults <- function(raw, herd_size) {
  ad <- copy(raw$app_defaults)
  p  <- scale_parameters(raw, herd_size)

  ad[, resolved := as.character(default_value)]
  for (i in seq_len(nrow(ad))) {
    src <- ad$source_parameter[i]
    if (!is.na(src) && nzchar(src) && !grepl("\\s", src) && !is.null(p[[src]])) {
      ad$resolved[i] <- as.character(p[[src]])
    }
  }
  ad[, .(control, label, resolved, min, max, step, source_parameter)]
}


#' @export
print.buffalo_params <- function(x, ...) {
  cat("<buffalo model parameters>\n")
  cat(sprintf("  herd size          : %g adult buffaloes%s\n", x$herd_size,
              if (isTRUE(x$at_anchor_ceiling))
                sprintf(" (at or above the %d head anchor: values held flat)", x$anchor_large) else ""))
  cat(sprintf("  calving interval   : %.1f months (%.0f days)\n",
              x$calving_interval_months, x$calving_interval_days))
  cat(sprintf("  lactation / dry    : %.0f / %.0f days -> %.1f%% of adults in milk\n",
              x$lactation_days, x$dry_days, x$fraction_in_milk * 100))
  cat(sprintf("  yield              : %.2f L/day herd average (%.2f at peak parity)\n",
              x$yield_herd_avg_litres_per_day, x$yield_peak_litres_per_day))
  cat(sprintf("  milk price         : Rs %.2f/litre\n", x$price_per_litre))
  cat(sprintf("  feed, lactating    : Rs %.2f/day\n", x$ration$lookup[["lactating"]]))
  cat(sprintf("  purchase price     : Rs %s/animal\n",
              format(round(x$purchase_price), big.mark = ",")))
  cat(sprintf("  equipment          : %d item types, Rs %s capital\n",
              nrow(x$equipment),
              format(attr(x$equipment, "total_capital"), big.mark = ",")))
  invisible(x)
}
