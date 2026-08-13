# tax.R -- income tax by entity form, with the s.44AD presumptive scheme.
#
# Dairy income is taxable business income, not agricultural income (plan 3.5).
# Every rate and threshold lives in sheet 11_Tax of the workbook, never in
# this file. The model checks the 44AD turnover cap each year rather than
# trusting the toggle (decision record #7). Regular-computation losses carry
# forward; marginal relief on surcharge is not modelled (recorded limitation).

library(data.table)


#' Slab tax for a proprietor/HUF under the new regime, before cess
slab_tax <- function(income, p) {
  limits <- c(p$tax_slab_limit_1, p$tax_slab_limit_2, p$tax_slab_limit_3,
              p$tax_slab_limit_4, p$tax_slab_limit_5, p$tax_slab_limit_6, Inf)
  rates  <- c(p$tax_slab_rate_1, p$tax_slab_rate_2, p$tax_slab_rate_3,
              p$tax_slab_rate_4, p$tax_slab_rate_5, p$tax_slab_rate_6,
              p$tax_slab_rate_top)
  tax <- 0; lower <- 0
  for (i in seq_along(limits)) {
    if (income <= lower) break
    tax <- tax + (min(income, limits[i]) - lower) * rates[i]
    lower <- limits[i]
  }
  # s.87A rebate extinguishes tax up to the income limit
  if (income <= p$rebate_87a_income_limit) tax <- max(0, tax - p$rebate_87a_amount)
  # surcharge tiers (no marginal relief)
  if      (income > p$surcharge_limit_2) tax <- tax * (1 + p$surcharge_rate_2)
  else if (income > p$surcharge_limit_1) tax <- tax * (1 + p$surcharge_rate_1)
  tax * (1 + p$health_education_cess)
}


#' Tax on a given taxable income for the chosen entity form
entity_tax <- function(income, p) {
  if (income <= 0) return(0)
  switch(p$entity_type,
    proprietor  = slab_tax(income, p),
    partnership = income * p$tax_rate_partnership,     # rate is cess-inclusive
    company     = income * p$tax_rate_company_115baa,
    cooperative = income * p$tax_rate_cooperative_115bad,
    slab_tax(income, p))
}


#' Annual tax schedule
#'
#' @param fin data.table with year, turnover, taxable_regular (already after
#'   tax depreciation, s.36(1)(vi) write-off and interest)
#' @param p buffalo_params
#' @return data.table year, loss_carried, taxable, basis, tax
tax_schedule <- function(fin, p) {
  presumptive_ok <- isTRUE(p$use_presumptive_44ad == 1) &&
    p$entity_type %in% c("proprietor", "partnership")
  # s.44AD(4) bars re-entry for a number of years once an assessee who has used
  # the scheme stops declaring on that basis. presumptive_sticky_years = 0
  # (the default) allows the year-by-year choice; 5 applies the statutory bar.
  lock <- p$presumptive_sticky_years
  lock <- if (is.null(lock) || is.na(lock)) 0L else as.integer(lock)

  loss_bf <- 0
  used_before <- FALSE
  barred_until <- -Inf                      # 44AD unavailable in years before this
  rows <- vector("list", nrow(fin))
  for (i in seq_len(nrow(fin))) {
    y  <- fin$year[i]
    ti <- fin$taxable_regular[i] - loss_bf
    if (ti < 0) { loss_next <- -ti; ti_reg <- 0 } else { loss_next <- 0; ti_reg <- ti }
    tax_reg <- entity_tax(ti_reg, p)

    basis <- "regular"; tax <- tax_reg; taxable <- ti_reg
    available <- presumptive_ok &&
      fin$turnover[i] <= p$presumptive_44ad_turnover_limit &&
      y >= barred_until
    if (available) {
      deemed <- fin$turnover[i] * p$presumptive_rate_digital   # receipts assumed digital
      tax_44 <- entity_tax(deemed, p)
      if (tax_44 < tax_reg) { basis <- "44AD"; tax <- tax_44; taxable <- deemed }
    }
    # leaving the scheme, having once used it, starts the lock-out; the test on
    # barred_until stops an active bar from being extended year after year
    if (lock > 0L && used_before && basis == "regular" && y >= barred_until) {
      barred_until <- y + lock
    }
    if (basis == "44AD") used_before <- TRUE

    rows[[i]] <- data.table(year = y, loss_brought_forward = loss_bf,
                            taxable = taxable, basis = basis, tax = tax)
    # losses only accumulate/consume under the regular computation
    loss_bf <- if (basis == "regular") loss_next else max(0, loss_next)
  }
  rbindlist(rows)
}
