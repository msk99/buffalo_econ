# depreciation.R -- two schedules, kept separate on purpose (plan 3.5):
#
#   book: straight line, shown in the P&L a bank reader expects. Animals
#         depreciate from purchase price down to salvage over
#         book_dep_animals_years.
#   tax : written-down-value blocks (buildings 10%, plant 15%). Livestock is
#         NOT depreciable (s. 43(3)); relief comes via s. 36(1)(vi) when a
#         purchased animal dies, at full purchase cost, carcass value nil
#         (decision record #6).

library(data.table)


#' Annual book depreciation
#' @return data.table year, dep_buildings, dep_plant, dep_animals, dep_total
book_depreciation <- function(p, capital, years) {
  buildings <- capital[class == "building", sum(cost)]
  plant     <- capital[class == "plant",    sum(cost)]
  animals_n <- p$herd_size
  animal_base <- animals_n * (p$purchase_price - p$culling_salvage_per_head)

  dt <- data.table(year = seq_len(years))
  dt[, dep_buildings := buildings / p$building_life_years]
  dt[, dep_plant     := plant / p$equipment_life_default_years]
  dt[, dep_animals   := pmax(0, animal_base) / p$book_dep_animals_years]
  dt[, dep_total := dep_buildings + dep_plant + dep_animals]
  dt[]
}


#' Annual tax depreciation (WDV blocks) plus the s.36(1)(vi) animal write-off
#' @param herd_a annual herd roll-up (for deaths of purchased animals)
tax_depreciation <- function(p, capital, herd_a, years) {
  b_open <- capital[class == "building", sum(cost)]
  p_open <- capital[class == "plant",    sum(cost)]
  rows <- vector("list", years)
  for (y in seq_len(years)) {
    b_dep <- b_open * p$tax_dep_building_wdv
    p_dep <- p_open * p$tax_dep_plant_wdv
    write_off <- herd_a[year == y, deaths_purchased] * p$purchase_price
    rows[[y]] <- data.table(year = y, tax_dep_buildings = b_dep,
                            tax_dep_plant = p_dep,
                            animal_write_off_36vi = write_off,
                            tax_dep_total = b_dep + p_dep + write_off)
    b_open <- b_open - b_dep
    p_open <- p_open - p_dep
  }
  rbindlist(rows)
}
