# cost_health.R -- veterinary, breeding, mastitis, insurance.
#
# Breeding is costed per conception: a blend of AI (services-per-conception x
# rate) and natural service. pregnancy_rate stays a cross-check only (decision
# record #5). Mastitis is explicit: incidence x treatment cost, plus the milk
# lost on affected animals valued at the milk price (plan 3.4).

library(data.table)


#' Monthly health and breeding cost
health_costs <- function(herd_m, p) {
  out <- herd_m[, .(month, year)]
  e_other <- esc_factor(p$annual_other_escalation, herd_m$year)
  e_milk  <- esc_factor(p$annual_price_escalation, herd_m$year)

  breeding_per_conception <-
    (1 - p$pct_bred_natural_service) * p$ai_per_conception * p$cost_per_ai +
    p$pct_bred_natural_service       * p$natural_service_cost_per_conception

  annual_milk_value_per_animal <-
    p$yield_herd_avg_litres_per_day * 365 * p$price_per_litre

  out[, vet       := herd_m$adults * p$vet_per_animal_year / 12 * e_other]
  out[, insurance := herd_m$adults * p$insurance_pct_animal_value * p$purchase_price / 12 * e_other]
  out[, breeding  := herd_m$births * breeding_per_conception * e_other]
  out[, mastitis_treatment := herd_m$adults * p$mastitis_incidence / 12 *
        p$mastitis_treatment_cost_per_case * e_other]
  out[, mastitis_milk_loss := herd_m$adults * p$mastitis_incidence / 12 *
        p$mastitis_milk_loss_pct * annual_milk_value_per_animal * e_milk]
  out[, health_total := vet + insurance + breeding + mastitis_treatment + mastitis_milk_loss]
  out[]
}


#' Monthly utilities, land rent and herd replacement purchases
other_costs <- function(herd_m, p) {
  out <- herd_m[, .(month, year)]
  e <- esc_factor(p$annual_other_escalation, herd_m$year)
  per_head_month <- p$electricity_per_head_month + p$water_per_head_month +
                    p$transport_per_head_month   + p$misc_per_head_month
  out[, utilities   := herd_m$adults * per_head_month * e]
  out[, land_rent   := herd_m$adults * p$land_rent_per_head_year / 12 * e]
  out[, replacements:= herd_m$replacement_purchases * p$purchase_price * e]
  out[, other_total := utilities + land_rent + replacements]
  out[]
}
