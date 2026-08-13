# Tax, loan and viability: one timeline, entity forms, 44AD cap.

library(testthat)
library(data.table)

WB  <- test_path("..", "..", "params", "parameters_v3_2026-08.xlsx")
raw <- suppressMessages(load_parameters(WB))
p50 <- scale_parameters(raw, 50)

test_that("slab tax reproduces hand-computed FY2026-27 cases", {
  # 12 lakh: slab tax 60k fully offset by the 87A rebate
  expect_equal(slab_tax(1200000, p50), 0)
  # 30 lakh: 0+20k+40k+60k+80k+100k+180k = 480k, no rebate, no surcharge, +4% cess
  expect_equal(slab_tax(3000000, p50), 480000 * 1.04, tolerance = 1e-9)
  # 60 lakh: base 1,380k, 10% surcharge, 4% cess
  expect_equal(slab_tax(6000000, p50), 1380000 * 1.10 * 1.04, tolerance = 1e-9)
})

test_that("entity forms use their workbook rates", {
  p_pt <- scale_parameters(raw, 50, overrides = list(entity_type = "partnership"))
  expect_equal(entity_tax(1e6, p_pt), 1e6 * 0.312)
  p_co <- scale_parameters(raw, 50, overrides = list(entity_type = "company"))
  expect_equal(entity_tax(1e6, p_co), 1e6 * 0.25168)
})

test_that("44AD applies only under the turnover cap and when beneficial", {
  fin <- data.table(year = 1:2,
                    turnover = c(2e7, 5e7),            # 2 cr ok, 5 cr over the cap
                    taxable_regular = c(5e6, 5e6))
  tx <- tax_schedule(fin, p50)
  expect_equal(tx$basis, c("44AD", "regular"))
  # deemed profit 6% of 2 cr = 12 lakh -> rebate zeroes the tax
  expect_equal(tx[year == 1, tax], 0)
  expect_gt(tx[year == 2, tax], 0)
})

test_that("regular-computation losses carry forward", {
  fin <- data.table(year = 1:3, turnover = rep(5e7, 3),
                    taxable_regular = c(-2e6, 1e6, 3e6))
  tx <- tax_schedule(fin, p50)
  expect_equal(tx[year == 1, tax], 0)
  expect_equal(tx[year == 2, tax], 0)                   # 1e6 fully absorbed by loss
  expect_equal(tx[year == 3, taxable], 2e6)             # 3e6 less remaining 1e6
})

test_that("loan schedule repays exactly and respects the moratorium", {
  l <- loan_schedule(1e7, p50, 10)
  s <- l$schedule
  expect_equal(l$loan + l$margin + l$subsidy, 1e7)
  expect_equal(sum(s$principal), l$loan, tolerance = 1e-6)
  expect_equal(s[.N, closing], 0, tolerance = 1e-6)
  p_mor <- scale_parameters(raw, 50, overrides = list(moratorium_months = 12))
  s_mor <- loan_schedule(1e7, p_mor, 10)$schedule
  expect_equal(s_mor[year == 1, principal], 0)
  expect_gt(s_mor[year == 1, interest], 0)              # interest still paid
})

test_that("NPV, IRR, BCR and payback agree on one timeline", {
  cf <- c(-1000, 300, 300, 300, 300, 300)
  expect_equal(npv(0, cf), 500)
  r <- irr(cf)
  expect_equal(npv(r, cf), 0, tolerance = 1e-6)
  expect_gt(bcr(0.10, cf), 1)                            # NPV@10% > 0 consistent
  expect_equal(payback_years(cf), 3 + 100 / 300, tolerance = 1e-9)
})

test_that("the full model is coherent at 50 head", {
  r <- run_buffalo_model(50, 10, raw = raw)
  s <- function(m) r$summary[metric == m, value]
  expect_equal(s("Project cost"), r$capital[, sum(cost)])
  # post-tax never beats pre-tax
  expect_lte(s("NPV post-tax @ 12%"), s("NPV pre-tax @ 12%") + 1e-6)
  expect_lte(s("IRR post-tax"), s("IRR pre-tax") + 1e-9)
  # BCR > 1 iff NPV > 0 at the same rate
  expect_equal(s("BCR @ 12%") > 1, s("NPV pre-tax @ 12%") > 0)
  # tenure: 50 head sits above the geometric mean of the anchors -> 7 years
  expect_equal(r$params$loan_tenure_years, 7)
  expect_equal(scale_parameters(raw, 20)$loan_tenure_years, 5)
  # tax binds at 200 head (turnover above the 44AD cap), not at 50
  expect_equal(sum(r$financials$tax), 0)
  r200 <- run_buffalo_model(200, 10, raw = raw)
  expect_gt(sum(r200$financials$tax), 0)
  # the 44AD cap binds in every steady year at 200 head (dip years may duck
  # under it and legitimately use the presumptive basis)
  expect_true(all(r200$financials[year >= 3, tax_basis] == "regular"))
  expect_true(all(r200$financials[year >= 3, turnover] >
                    r200$params$presumptive_44ad_turnover_limit))
})

test_that("equity leverage cuts both ways", {
  r50 <- run_buffalo_model(50, 10, raw = raw)     # healthy project
  s50 <- function(m) r50$summary[metric == m, value]
  expect_gt(s50("Equity IRR post-tax"), s50("IRR post-tax"))
  r10 <- run_buffalo_model(10, 10, raw = raw)     # weak project
  s10 <- function(m) r10$summary[metric == m, value]
  expect_lt(s10("Equity IRR post-tax"), s10("IRR post-tax"))
})

test_that("the s.44AD lock-out bars re-entry once the scheme is left", {
  # turnover ducks under the cap in year 3, so the opportunistic rule would
  # re-enter the scheme; the statutory bar should prevent that.
  fin <- data.table(year = 1:8,
                    turnover = c(2e7, 5e7, 2e7, 2e7, 2e7, 2e7, 2e7, 2e7),
                    taxable_regular = rep(5e6, 8))
  p_open   <- scale_parameters(raw, 50, overrides = list(presumptive_sticky_years = 0))
  p_sticky <- scale_parameters(raw, 50, overrides = list(presumptive_sticky_years = 5))

  open   <- tax_schedule(fin, p_open)
  sticky <- tax_schedule(fin, p_sticky)

  expect_equal(open$basis,   c("44AD", "regular", rep("44AD", 6)))
  # left the scheme in year 2, barred through year 6, available again in year 7
  expect_equal(sticky$basis, c("44AD", rep("regular", 5), "44AD", "44AD"))
  expect_gt(sum(sticky$tax), sum(open$tax))
})

test_that("the lock-out defaults to off so the engine is unchanged", {
  expect_equal(scale_parameters(raw, 50)$presumptive_sticky_years, 0)
})
