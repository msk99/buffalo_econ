# ration.R -- feed cost per month from the ration build-up.
#
# The per-class daily costs come from params_scale.R (kg/day x Rs/kg per
# feedstuff, interpolated at the herd size). Wastage is already inside the
# ration rates (plan 3.1). Feed prices escalate with annual_feed_escalation
# from Year 2 (decision record #8).

library(data.table)

esc_factor <- function(rate, year) (1 + rate)^(year - 1)


#' Monthly feed cost by animal class
#' @param herd_m monthly herd projection
#' @param p buffalo_params
#' @return data.table month, year, feed_lactating..feed_total (INR)
feed_costs <- function(herd_m, p) {
  lk <- p$ration$lookup
  mult <- p$feed_cost_multiplier %||% 1     # sensitivity hook, default 1
  out <- herd_m[, .(month, year)]
  e <- esc_factor(p$annual_feed_escalation, herd_m$year)
  out[, feed_lactating := herd_m$in_milk    * lk[["lactating"]] * DAYS_PER_MONTH * e * mult]
  out[, feed_dry       := herd_m$dry_adults * lk[["dry"]]       * DAYS_PER_MONTH * e * mult]
  out[, feed_heifer    := herd_m$heifers    * lk[["heifer"]]    * DAYS_PER_MONTH * e * mult]
  out[, feed_calf      := herd_m$calves     * lk[["calf"]]      * DAYS_PER_MONTH * e * mult]
  out[, feed_total := feed_lactating + feed_dry + feed_heifer + feed_calf]
  out[]
}
