# revenue.R -- milk, manure, calves, culls, feed bags.
#
# Milk escalates with annual_price_escalation; every non-milk line with
# annual_other_escalation (decision record #8). Surplus animals are culled
# adults sold at salvage -- the steady-state policy (decision record #1);
# there is no separate surplus-heifer sale line.

library(data.table)


#' Monthly revenue
revenues <- function(herd_m, p) {
  out <- herd_m[, .(month, year)]
  e_milk  <- esc_factor(p$annual_price_escalation, herd_m$year)
  e_other <- esc_factor(p$annual_other_escalation, herd_m$year)

  out[, milk        := herd_m$milk_litres * p$price_per_litre * e_milk]
  out[, manure      := herd_m$adults * p$manure_value_per_animal_year / 12 * e_other]
  out[, feed_bags   := herd_m$adults * p$feed_bag_sale_per_head_year / 12 * e_other]
  out[, male_calves := herd_m$male_calves_sold * p$male_calf_sale_price * e_other]
  # non-retained female calves leave at the same gate and price (decision #3)
  out[, female_calves := herd_m$female_calves_sold * p$male_calf_sale_price * e_other]
  out[, culls       := herd_m$culls * p$culling_salvage_per_head * e_other]
  out[, revenue_total := milk + manure + feed_bags + male_calves + female_calves + culls]
  out[]
}
