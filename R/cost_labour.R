# cost_labour.R -- family and hired labour, fractional FTE, adult equivalents.
#
# Workers are fractional (the projection is an expected value, plan 3.3); only
# supervisors are whole people (capacity mode). Family labour applies at herd
# sizes up to family_labour_max_herd, supplies up to family_labour_fte_cap FTE,
# is costed at the market wage and reported on its own line so it can be read
# as household self-employment income (plan 3.3, decision record #4).

library(data.table)


#' Monthly labour cost
#' @return data.table with FTE and cost split family / hired / supervision
labour_costs <- function(herd_m, p) {
  out <- herd_m[, .(month, year)]
  e <- esc_factor(p$annual_wage_escalation, herd_m$year)

  fte_req <- herd_m$adult_equivalents / p$animals_per_worker
  fam_cap <- if (p$herd_size <= p$family_labour_max_herd) p$family_labour_fte_cap else 0
  fte_fam <- pmin(fte_req, fam_cap)
  fte_hired <- fte_req - fte_fam

  # Supervision is fractional FTE at every herd size (a share of a manager's
  # time), matching the June 2026 workbook's own 0.1/0.4/1 entries. The
  # whole-person treatment produced a Rs 3.6 lakh cliff at the threshold --
  # caught by the NPV continuity test (decision record #4, revised Aug 2026).
  sup_units <- p$herd_size / p$supervisor_capacity_head

  out[, fte_required := fte_req]
  out[, fte_family   := fte_fam]
  out[, fte_hired    := fte_hired]
  out[, supervisors  := sup_units]
  out[, labour_family     := fte_fam   * p$wage_per_month * e]
  out[, labour_hired      := fte_hired * p$wage_per_month * e]
  out[, labour_supervision:= sup_units * p$supervisor_salary_month * e]
  out[, labour_total := labour_family + labour_hired + labour_supervision]
  out[]
}
