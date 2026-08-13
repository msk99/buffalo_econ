# pnl.R -- assemble the monthly operating statement and roll it up to years.
#
# This stops at EBITDA; depreciation, interest and tax are layered on by the
# orchestrator (run_model.R) because they need the capital and loan schedules.

library(data.table)


#' Monthly operating statement
#' @return data.table: revenue lines, cost lines, ebitda
operating_statement <- function(herd_m, p) {
  rev  <- revenues(herd_m, p)
  feed <- feed_costs(herd_m, p)
  lab  <- labour_costs(herd_m, p)
  hea  <- health_costs(herd_m, p)
  oth  <- other_costs(herd_m, p)

  dt <- herd_m[, .(month, year)]
  dt <- cbind(
    dt,
    rev [, .(rev_milk = milk, rev_manure = manure, rev_feed_bags = feed_bags,
             rev_male_calves = male_calves, rev_female_calves = female_calves,
             rev_culls = culls, revenue_total)],
    feed[, .(feed_total)],
    lab [, .(labour_family, labour_hired, labour_supervision, labour_total,
             fte_family, fte_hired, supervisors)],
    hea [, .(vet, insurance, breeding, mastitis_treatment, mastitis_milk_loss,
             health_total)],
    oth [, .(utilities, land_rent, replacements, other_total)]
  )
  dt[, opex_total := feed_total + labour_total + health_total + other_total]
  dt[, ebitda := revenue_total - opex_total]
  dt[]
}


#' Annual roll-up of the operating statement (all lines are flows: sum)
operating_annual <- function(op_m) {
  cols <- setdiff(names(op_m), c("month", "year"))
  means <- c("fte_family", "fte_hired", "supervisors")
  sums  <- setdiff(cols, means)
  a <- op_m[, lapply(.SD, sum),  by = year, .SDcols = sums]
  b <- op_m[, lapply(.SD, mean), by = year, .SDcols = means]
  a[b, on = "year"][]
}
