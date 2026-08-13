# Sensitivity, breakeven, Excel report -- and the continuity regression test
# that guards against NPV cliffs at scale boundaries.

library(testthat)
library(data.table)

WB  <- test_path("..", "..", "params", "parameters_v3_2026-08.xlsx")
raw <- suppressMessages(load_parameters(WB))

test_that("tornado ranks milk price and yield as equal top drivers", {
  tor <- tornado(raw, 50, 5)
  expect_equal(tor$driver[1:2] %in% c("price_per_litre", "yield_herd_avg_litres_per_day"),
               c(TRUE, TRUE))
  # both scale milk revenue linearly, so their swings must match
  expect_equal(tor[driver == "price_per_litre", swing_total],
               tor[driver == "yield_herd_avg_litres_per_day", swing_total],
               tolerance = 1e-6)
  # a longer calving interval must reduce NPV (less milk)
  ci <- tor[driver == "calving_interval_months"]
  expect_lt(ci$npv_high, ci$npv_low)
})

test_that("breakeven solvers invert the model", {
  be <- breakeven_milk_price(raw, 50, 5)
  expect_false(is.na(be))
  expect_equal(npv_for(raw, 50, 5, list(price_per_litre = be)), 0, tolerance = 500)
  # the DSCR-1.25 price is above the NPV-zero price
  bd <- breakeven_price_for_dscr(raw, 50, 5)
  expect_gt(bd, be)
})

test_that("the report writes a complete workbook to any path (tempfile-safe)", {
  r <- run_buffalo_model(50, 5, raw = raw,
                         overrides = list(price_per_litre = 70))
  f <- tempfile(fileext = ".xlsx")
  write_report(r, f)
  expect_true(file.exists(f))
  sheets <- wb_get_sheet_names(wb_load(f))
  expect_true(all(c("Summary", "Capital", "Herd", "Operating", "Financials",
                    "Loan", "Terminal", "Cash flows", "Overrides") %in% sheets))
  unlink(f)
})

test_that("NPV per head is continuous across the old scale boundaries", {
  # A steep but smooth curve is fine (fixed-cost dilution is real); what must
  # not exist is a boundary step out of line with its neighbours. So compare
  # each boundary step with the adjacent steps.
  npv_head <- function(n) npv_for(raw, n, 5) / n
  for (b in c(21, 101, 200)) {
    v <- vapply((b - 2):(b + 1), npv_head, numeric(1))
    steps <- abs(diff(v))                       # (b-2->b-1, b-1->b, b->b+1)
    expect_lt(steps[2], 3 * max(steps[1], steps[3]) + 1000,
              label = sprintf("NPV/head step into %d head", b))
  }
})

test_that("pre-tax economics improve with scale; post-tax may legitimately dip", {
  # Pre-tax NPV/head must rise with scale (fixed-cost dilution, plan 4b).
  # Post-tax is allowed to fall once turnover crosses the 44AD cap around
  # 150 head -- that is tax law, not a model artifact.
  sizes <- c(15, 30, 60, 120, 240)
  pre <- vapply(sizes, function(n) npv_for(raw, n, 5, basis = "pre") / n, numeric(1))
  expect_true(all(diff(pre) > 0))
})
