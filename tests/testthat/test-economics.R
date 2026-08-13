# Costs, revenue, capital, depreciation, operating statement.

library(testthat)
library(data.table)

WB   <- test_path("..", "..", "params", "parameters_v3_2026-08.xlsx")
raw  <- suppressMessages(load_parameters(WB))
p50  <- scale_parameters(raw, 50)
h50  <- project_herd(p50, 10)
op   <- operating_statement(h50, p50)
oa   <- operating_annual(op)

test_that("feed cost reproduces the ration at the anchors", {
  p10 <- scale_parameters(raw, 10)
  h1 <- project_herd(p10, 5)
  f1 <- feed_costs(h1, p10)
  m <- h1[month == 20]
  expect_equal(f1[month == 20, feed_lactating],
               m$in_milk * 281.75 * DAYS_PER_MONTH * 1.05, tolerance = 1e-6)
})

test_that("family labour applies only at small herd sizes and is capped", {
  p10 <- scale_parameters(raw, 10)
  l10 <- labour_costs(project_herd(p10, 5), p10)
  expect_gt(l10[month == 30, fte_family], 0)
  expect_lte(max(l10$fte_family), p10$family_labour_fte_cap)
  l50 <- labour_costs(h50, p50)
  expect_equal(sum(l50$fte_family), 0)
  # supervision is fractional FTE: a share of a manager's time at any size
  expect_equal(unique(l50$supervisors), 50 / 150)
  expect_equal(unique(l10$supervisors), 10 / 150)
})

test_that("labour is fractional FTE, not rounded up", {
  l <- labour_costs(h50, p50)
  expect_false(all(l$fte_hired == ceiling(l$fte_hired)))
})

test_that("mastitis cost scales with incidence and milk price", {
  hc <- health_costs(h50, p50)
  expect_true(all(hc$mastitis_treatment > 0))
  expect_true(all(hc$mastitis_milk_loss > 0))
  p_hi <- scale_parameters(raw, 50, overrides = list(mastitis_incidence = 0.30))
  hc_hi <- health_costs(h50, p_hi)
  expect_gt(hc_hi[month == 12, health_total], hc[month == 12, health_total])
})

test_that("capital schedule covers animals, civil, plant and working capital", {
  cap <- capital_schedule(p50, working_capital = 1e6)
  expect_setequal(unique(cap$class), c("animals", "building", "plant", "working_capital"))
  expect_equal(cap[item == "Milch animals", cost], 50 * p50$purchase_price)
  expect_gt(cap[class == "building", sum(cost)], 0)
})

test_that("book and tax depreciation differ and animals are excluded from tax WDV", {
  cap <- capital_schedule(p50, 1e6)
  bd <- book_depreciation(p50, cap, 10)
  td <- tax_depreciation(p50, cap, herd_annual(h50), 10)
  expect_true(all(bd$dep_animals > 0))                    # book depreciates animals
  expect_false("animals" %in% names(td))                  # tax WDV has no animal block
  # WDV declines; straight line is flat
  expect_true(all(diff(td$tax_dep_buildings) < 0))
  expect_equal(var(bd$dep_buildings), 0)
  # s.36(1)(vi): write-off follows purchased-animal deaths, decaying over time
  expect_gt(td[year == 2, animal_write_off_36vi], td[year == 9, animal_write_off_36vi])
})

test_that("terminal value uses salvage by default and market on request", {
  cap <- capital_schedule(p50, 1e6)
  ha <- herd_annual(h50)
  tv_s <- terminal_value(p50, cap, ha, 10)
  p_mkt <- scale_parameters(raw, 50, overrides = list(terminal_animals_at = "market"))
  tv_m <- terminal_value(p_mkt, cap, ha, 10)
  expect_gt(tv_m[item == "Herd at terminal value", value],
            tv_s[item == "Herd at terminal value", value])
  expect_equal(tv_s[item == "Working capital recovery", value], 1e6)
})

test_that("the operating statement balances", {
  expect_equal(oa$ebitda, oa$revenue_total - oa$opex_total, tolerance = 1e-9)
  expect_equal(oa$opex_total,
               oa$feed_total + oa$labour_total + oa$health_total + oa$other_total,
               tolerance = 1e-9)
})

test_that("cull revenue appears once the steady-state policy starts culling", {
  expect_equal(oa[year == 2, rev_culls], 0, tolerance = 1e-9)
  expect_gt(oa[year == 5, rev_culls], 0)
})
