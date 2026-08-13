# Herd cohort engine: steady-state policy, ramp, pipeline, milk.

library(testthat)
library(data.table)

WB  <- test_path("..", "..", "params", "parameters_v3_2026-08.xlsx")
raw <- suppressMessages(load_parameters(WB))
p50 <- scale_parameters(raw, 50)
h   <- project_herd(p50, 10)
ha  <- herd_annual(h)

test_that("the adult herd is held fixed at the target from the second batch on", {
  # Owner decision Aug 2026: steady state, extra animals culled.
  expect_true(all(abs(ha[year >= 2, adults] - 50) < 1e-6))
  # year 1 averages the two-batch ramp: 25 head for six months, then 50
  expect_equal(ha[year == 1, adults], 37.5, tolerance = 1e-6)
})

test_that("culling balances the pipeline instead of triggering purchases", {
  # while the pipeline is empty, only deaths are replaced and nothing is culled
  expect_equal(ha[year %in% 1:3, sum(culls)], 0, tolerance = 1e-9)
  expect_equal(ha[year %in% 1:3, sum(replacement_purchases)],
               ha[year %in% 1:3, sum(deaths_adult)], tolerance = 1e-9)
  # once heifers mature, surplus is culled and purchases stop
  expect_gt(ha[year == 5, culls], 10)
  expect_equal(ha[year >= 5, sum(replacement_purchases)], 0, tolerance = 1e-9)
  # conservation: joiners equal exits in steady state
  expect_equal(ha[year == 9, heifers_matured],
               ha[year == 9, culls + deaths_adult], tolerance = 1e-6)
})

test_that("fractional animals: no rounding anywhere in the projection", {
  expect_false(all(ha$culls == round(ha$culls)))
  expect_false(all(h$adults == round(h$adults)))
})

test_that("the in-milk share converges near the derived steady-state fraction", {
  f10 <- ha[year == 10, in_milk / adults]
  expect_gt(f10, p50$fraction_in_milk - 0.02)
  expect_lt(f10, p50$fraction_in_milk + 0.05)   # slight excess: fresh first-calvers
})

test_that("year 1 is a ramp, not full production from day one", {
  y1 <- ha[year == 1, milk_litres]
  y10 <- ha[year == 10, milk_litres]
  full_year_equivalent <- y10 * ha[year == 1, adults] / 50
  expect_lt(y1 / 12, y10 / 10 * 1.5)             # sanity
  expect_gt(h[month == 1, in_milk], 24)          # batch 1 arrives in milk
  expect_lt(h[month == 3, in_milk], 26)          # batch 2 not yet arrived
  expect_gt(h[month == 8, in_milk], 45)          # both batches in milk
})

test_that("milk per in-milk animal reproduces the entered herd-average yield", {
  daily <- ha[year == 10, milk_litres / (12 * DAYS_PER_MONTH) / in_milk]
  expect_equal(daily, p50$yield_herd_avg_litres_per_day *
                 ha[year == 10, parity_mult] / p50$parity_weighted_mult,
               tolerance = 1e-3)
  # and the settled parity mix lands near the workbook steady-state mix
  expect_equal(ha[year == 10, parity_mult], p50$parity_weighted_mult, tolerance = 0.01)
})

test_that("young stock pipeline conserves animals", {
  # births split by sex ratio; males all leave by sale or death
  total_born_y <- ha[year == 5, births]
  expect_equal(ha[year == 6, male_calves_sold] + 0,
               total_born_y * (1 - p50$sex_ratio_female) * (1 - p50$mortality_rate_calf)^0.5,
               tolerance = 0.05 * total_born_y)
  # non-retained females are sold at the calf gate
  expect_gt(ha[year == 6, female_calves_sold], 0)
})

test_that("purchased pool decays as home-bred replacements arrive", {
  ps <- ha$purchased_share
  expect_equal(ps[1], 1, tolerance = 1e-9)
  expect_true(all(diff(ps[4:10]) < 0))
  expect_lt(ps[10], 0.25)
})

test_that("the projection runs across the full herd-size range", {
  for (n in c(10, 21, 101, 500, 1000)) {
    pn <- scale_parameters(raw, n)
    hn <- herd_annual(project_herd(pn, 5))
    expect_true(all(abs(hn[year >= 2, adults] - n) < 1e-6), label = paste("n =", n))
    expect_true(all(hn$milk_litres > 0), label = paste("n =", n))
  }
})
