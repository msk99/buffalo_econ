# cost_capital.R -- project capital: animals, civil works, equipment, working
# capital, and the terminal value.
#
# Timeline convention (plan 7): all capital is committed at t = 0, operations
# run Years 1..n, terminal value falls in Year n. The second animal batch
# arrives at month 7 in the herd model, but its funds are committed at
# sanction, so financially it sits at t = 0 (decision record #9).

library(data.table)


#' Project capital cost breakdown at t = 0
#'
#' @param p buffalo_params
#' @param working_capital INR (computed from the first full operating year)
#' @return data.table item, class (animals/building/plant/working_capital), cost
capital_schedule <- function(p, working_capital = 0) {
  N <- p$herd_size
  shed_covered <- N * p$covered_sqft_per_adult * p$covered_cost_per_sqft
  shed_open    <- N * p$open_sqft_per_adult    * p$open_cost_per_sqft
  godown       <- p$godown_sqft * p$godown_cost_per_sqft

  items <- data.table(
    item = c("Milch animals", "Shed (covered)", "Shed (open paddock)",
             "Fodder godown", "Bore well and water supply", "Milk storage room",
             "Miscellaneous civil works", "Equipment", "Working capital"),
    class = c("animals", "building", "building", "building", "building",
              "building", "building", "plant", "working_capital"),
    cost = c(N * p$purchase_price, shed_covered, shed_open, godown,
             p$bore_well_cost, p$storage_room_cost, p$misc_civil_cost,
             attr(p$equipment, "total_capital"), working_capital)
  )
  items[]
}


#' Terminal value at the end of the horizon (Year n)
#'
#' Buildings and equipment at straight-line book value; animals per the
#' terminal_animals_at regime (salvage = conservative default, decision #6);
#' heifer inventory at heifer sale price; working capital recovered.
#' Terminal animal values escalate with the other-revenue rate (decision #8).
terminal_value <- function(p, capital, herd_a, years) {
  bv <- function(cost, life) cost * max(0, 1 - years / life)
  buildings <- capital[class == "building", sum(cost)]
  equip     <- capital[class == "plant",    sum(cost)]
  wc        <- capital[class == "working_capital", sum(cost)]

  adult_value <- switch(p$terminal_animals_at %||% "salvage",
    market = p$purchase_price,
    p$culling_salvage_per_head)

  e <- esc_factor(p$annual_other_escalation, years)
  end <- herd_a[year == years]

  data.table(
    item = c("Buildings at book value", "Equipment at book value",
             "Herd at terminal value", "Heifer inventory", "Working capital recovery"),
    value = c(bv(buildings, p$building_life_years),
              bv(equip, p$equipment_life_default_years),
              end$adults * adult_value * e,
              end$heifers * p$heifer_sale_price * e,
              wc)
  )
}
