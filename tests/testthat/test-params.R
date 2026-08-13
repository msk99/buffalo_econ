# Loader, validator and scaling tests.
#
# Several of these encode known spreadsheet-interface failure modes, so that
# they can never appear silently.

library(testthat)
library(data.table)

WB <- test_path("..", "..", "params", "parameters_v3_2026-08.xlsx")
raw <- load_parameters(WB)

# ---------------------------------------------------------------- loading

test_that("the workbook loads and declares its anchors from the column headers", {
  expect_s3_class(raw, "buffalo_params_raw")
  expect_equal(raw$anchors[["small"]], 10L)
  expect_equal(raw$anchors[["large"]], 200L)
  expect_gt(nrow(raw$params), 80)
  expect_gt(nrow(raw$equipment), 10)
})

test_that("every parameter name is unique across the whole workbook", {
  # With duplicate names, a second block would silently overwrite the first.
  expect_equal(anyDuplicated(raw$params$parameter), 0L)
})

test_that("free text does not leak in as parameters", {
  expect_false(any(is.na(raw$params$parameter)))
  expect_true(all(nchar(raw$params$parameter) > 0))
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", raw$params$parameter)))
})

test_that("the parameter sheets and the schema sheet agree exactly", {
  expect_setequal(raw$params$parameter, raw$schema$parameter)
})

test_that("anchors are parsed from headers, not hard-coded", {
  expect_equal(parse_anchors(c("parameter", "small_5", "large_900"))[["small"]], 5L)
  expect_equal(parse_anchors(c("parameter", "small_5", "large_900"))[["large"]], 900L)
  expect_error(parse_anchors(c("parameter", "small_10")), "anchor herd sizes")
  expect_error(parse_anchors(c("small_500", "large_10")), "must be below")
})

# ---------------------------------------------------------------- validation

test_that("the shipped workbook passes validation", {
  res <- suppressMessages(validate_parameters(raw))
  expect_length(res$errors, 0)
})

test_that("a duplicated parameter name is an error", {
  bad <- raw
  bad$params <- rbind(raw$params, raw$params[parameter == "price_per_litre"])
  expect_error(suppressMessages(validate_parameters(bad)), "Duplicated parameter name")
})

test_that("a blank anchor cell is an error rather than being read as zero", {
  bad <- copy_raw(raw)
  bad$params[parameter == "price_per_litre", small := NA_real_]
  expect_error(suppressMessages(validate_parameters(bad)), "blank is never assumed to mean zero")
})

test_that("a value outside its declared range is an error", {
  bad <- copy_raw(raw)
  bad$params[parameter == "price_per_litre", small := 5000]
  expect_error(suppressMessages(validate_parameters(bad)), "above its maximum")
})

test_that("scale_mode 'constant' with differing anchors is an error", {
  bad <- copy_raw(raw)
  bad$params[parameter == "lactation_days", large := 250]
  expect_error(suppressMessages(validate_parameters(bad)), "'constant' but the two anchors differ")
})

test_that("an unrecognised scale_mode is an error", {
  bad <- copy_raw(raw)
  bad$params[parameter == "price_per_litre", scale_mode := "magic"]
  expect_error(suppressMessages(validate_parameters(bad)), "Unrecognised scale_mode")
})

test_that("parity shares must sum to one", {
  bad <- copy_raw(raw)
  bad$params[parameter == "parity_share_1", `:=`(small = 0.5, large = 0.5)]
  expect_error(suppressMessages(validate_parameters(bad)), "Parity shares sum to")
})

test_that("lactation must fit inside the calving interval", {
  bad <- copy_raw(raw)
  bad$params[parameter == "lactation_days", `:=`(small = 600, large = 600)]
  expect_error(suppressMessages(validate_parameters(bad)), "does not fit inside a calving interval")
})

test_that("a ration total that disagrees with its components is an error", {
  bad <- copy_raw(raw)
  bad$ration[animal_class == "lactating" & is_total == TRUE, cost_small := 999]
  expect_error(suppressMessages(validate_parameters(bad)), "TOTAL says")
})

test_that("equipment items sharing an activation threshold produce a warning", {
  # Synchronised steps produce cliffs in results.
  bad <- copy_raw(raw)
  bad$equipment[, min_herd := 50]
  expect_message(validate_parameters(bad, strict = FALSE), "Synchronised steps")
})

# ---------------------------------------------------------------- scaling

test_that("values are held flat outside the anchors and never extrapolated", {
  lo <- scale_parameters(raw, 10)
  hi <- scale_parameters(raw, 200)
  expect_equal(scale_parameters(raw,   5)$price_per_litre, lo$price_per_litre)
  expect_equal(scale_parameters(raw, 500)$price_per_litre, hi$price_per_litre)
  expect_equal(scale_parameters(raw, 900)$price_per_litre, hi$price_per_litre)
  expect_true(scale_parameters(raw, 900)$at_anchor_ceiling)
  expect_false(scale_parameters(raw, 100)$at_anchor_ceiling)
})

test_that("interpolated values sit exactly on the anchors at the anchors", {
  p <- raw$params[parameter == "price_per_litre"]
  expect_equal(scale_parameters(raw,  10)$price_per_litre, p$small)
  expect_equal(scale_parameters(raw, 200)$price_per_litre, p$large)
})

test_that("log interpolation lands where the arithmetic says it should", {
  p  <- raw$params[parameter == "yield_herd_avg_litres_per_day"]
  t  <- log(50 / 10) / log(200 / 10)
  expect_equal(scale_parameters(raw, 50)$yield_herd_avg_litres_per_day,
               p$small + (p$large - p$small) * t, tolerance = 1e-9)
})

test_that("constant parameters do not move with herd size", {
  for (n in c(10, 37, 200, 800)) {
    expect_equal(scale_parameters(raw, n)$lactation_days, 300)
    expect_equal(scale_parameters(raw, n)$wage_per_month, 15000)
  }
})

test_that("there is no cliff at any herd size", {
  # Guard: unit costs must move smoothly with herd size, with no banded jumps.
  sizes <- 10:400
  lact  <- vapply(sizes, function(n) scale_parameters(raw, n)$ration$lookup[["lactating"]], numeric(1))
  step  <- abs(diff(lact) / head(lact, -1))
  expect_lt(max(step), 0.01)
  expect_true(all(diff(lact) <= 1e-9))   # monotonic non-increasing
})

test_that("dry days are derived from the calving interval, not entered", {
  p <- scale_parameters(raw, 50)
  expect_equal(p$dry_days, p$calving_interval_months * 30.42 - p$lactation_days,
               tolerance = 1e-9)
  # lactation/(lactation+dry) would give 0.75; the calving interval gives less.
  expect_lt(p$fraction_in_milk, 0.70)
  expect_gt(p$fraction_in_milk, 0.60)
})

test_that("peak yield is back-solved so the parity mix reproduces the herd average", {
  p <- scale_parameters(raw, 50)
  expect_gt(p$yield_peak_litres_per_day, p$yield_herd_avg_litres_per_day)
  recombined <- p$yield_peak_litres_per_day * p$parity_weighted_mult
  expect_equal(recombined, p$yield_herd_avg_litres_per_day, tolerance = 1e-9)
})

test_that("the ration reproduces the workbook totals at both anchors", {
  expect_equal(scale_parameters(raw,  10)$ration$lookup[["lactating"]], 281.75, tolerance = 1e-6)
  expect_equal(scale_parameters(raw, 200)$ration$lookup[["lactating"]], 207.75, tolerance = 1e-6)
  expect_equal(scale_parameters(raw,  10)$ration$lookup[["dry"]],       133.75, tolerance = 1e-6)
  expect_equal(scale_parameters(raw, 200)$ration$lookup[["heifer"]],     89.25, tolerance = 1e-6)
})

test_that("equipment follows the capacity rule and keeps scaling above the anchor", {
  e10  <- equipment_schedule(raw, 10)
  e200 <- equipment_schedule(raw, 200)
  e900 <- equipment_schedule(raw, 900)
  cap  <- function(e) attr(e, "total_capital")

  expect_equal(cap(e10), 36000)
  expect_gt(cap(e900), cap(e200))          # not clamped: units keep growing
  expect_false("Tractor" %in% e10$item)    # below its activation threshold
  expect_true("Tractor" %in% e200$item)

  # units = ceiling(herd / capacity)
  fans <- raw$equipment[item == "Fans for heat stress"]
  expect_equal(e200[item == "Fans for heat stress", units],
               ceiling(200 / fans$capacity_head))
})

test_that("total capital per head falls smoothly and stays monotonic", {
  sizes <- 10:400
  cap <- vapply(sizes, function(n) {
    p <- scale_parameters(raw, n)
    animals <- p$purchase_price * n
    shed <- (p$covered_sqft_per_adult * p$covered_cost_per_sqft +
             p$open_sqft_per_adult    * p$open_cost_per_sqft) * n
    civil <- p$bore_well_cost + p$storage_room_cost + p$misc_civil_cost
    animals + shed + civil + attr(p$equipment, "total_capital")
  }, numeric(1))

  expect_true(all(diff(cap) > 0))                        # total always rises
  per_head <- cap / sizes
  expect_lt(max(abs(diff(per_head) / head(per_head, -1))), 0.035)   # no cliff
})

test_that("overrides are applied last and unknown ones warn", {
  p <- scale_parameters(raw, 50, overrides = list(price_per_litre = 99))
  expect_equal(p$price_per_litre, 99)
  expect_warning(scale_parameters(raw, 50, overrides = list(not_a_param = 1)),
                 "not recognised")
})

test_that("app defaults resolve from the workbook rather than a fixed number", {
  # Hard-coded defaults would stop workbook edits from reaching the app.
  d50  <- app_defaults(raw, 50)
  d200 <- app_defaults(raw, 200)
  price50  <- d50[control  == "price_per_litre", resolved]
  price200 <- d200[control == "price_per_litre", resolved]
  expect_false(price50 == price200)
  expect_equal(as.numeric(price200), scale_parameters(raw, 200)$price_per_litre)
})

# ------------------------------------------------- 2026-08 workbook extension

test_that("the tax slab schedule lives in the workbook, not in code", {
  p <- scale_parameters(raw, 50)
  expect_equal(p$tax_slab_limit_1, 400000)
  expect_equal(p$tax_slab_rate_top, 0.30)
  expect_equal(p$surcharge_rate_2, 0.15)
})

test_that("manual automation skips flagged equipment and costs less", {
  mech <- equipment_schedule(raw, 100, automation = "mechanised")
  man  <- equipment_schedule(raw, 100, automation = "manual")
  expect_true(any(grepl("Milking", mech$item)))
  expect_false(any(grepl("Milking|cooler", man$item)))
  expect_lt(attr(man, "total_capital"), attr(mech, "total_capital"))
})

test_that("terminal_animals_at is a regime choice carried as text", {
  expect_equal(scale_parameters(raw, 50)$terminal_animals_at, "salvage")
})
