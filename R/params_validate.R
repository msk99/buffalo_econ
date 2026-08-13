# params_validate.R -- check the workbook before anything downstream trusts it.
#
# The silent failure modes of a spreadsheet interface -- a duplicated block
# overwriting itself, free-text rows coerced to NA, a hard-coded UI value
# replacing the workbook -- are all errors here, and the run stops.
#
# All problems are collected and reported together, so a reviewer fixes the
# workbook once rather than discovering faults one run at a time.

library(data.table)


new_issue_log <- function() new.env(parent = emptyenv(), size = 8L)

add_issue <- function(log, kind, where, msg) {
  key <- paste0(kind, "_", length(ls(log)) + 1L)
  assign(key, list(kind = kind, where = where, msg = msg), envir = log)
  invisible(NULL)
}

collect <- function(log, kind) {
  items <- mget(ls(log), envir = log)
  items <- Filter(function(i) i$kind == kind, items)
  vapply(items, function(i) sprintf("  [%s] %s", i$where, i$msg), character(1), USE.NAMES = FALSE)
}


#' Validate a loaded parameter workbook
#'
#' @param raw object from load_parameters()
#' @param strict if TRUE (default) errors stop execution; warnings always print
#' @return invisibly, a list of errors and warnings
validate_parameters <- function(raw, strict = TRUE) {

  stopifnot(inherits(raw, "buffalo_params_raw"))
  log <- new_issue_log()
  p   <- raw$params
  err <- function(where, msg) add_issue(log, "error", where, msg)
  wrn <- function(where, msg) add_issue(log, "warning", where, msg)

  # ---- 1. parameter names: present, unique, well formed -------------------
  if (any(is.na(p$parameter) | p$parameter == "")) {
    err("params", "One or more rows have a blank parameter name.")
  }
  dupes <- p[, .N, by = parameter][N > 1L]
  if (nrow(dupes) > 0) {
    err("params", sprintf(
      "Duplicated parameter name(s): %s. A name must appear exactly once in the whole workbook.",
      paste(sprintf("%s (x%d)", dupes$parameter, dupes$N), collapse = ", ")))
  }
  bad_name <- p[!grepl("^[a-z][a-z0-9_]*$", parameter), parameter]
  if (length(bad_name) > 0) {
    wrn("params", sprintf("Parameter name(s) not in lower_snake_case: %s",
                          paste(bad_name, collapse = ", ")))
  }

  # ---- 2. declared modes and rounding are recognised ----------------------
  bad_mode <- p[!scale_mode %in% VALID_SCALE_MODES]
  if (nrow(bad_mode) > 0) {
    err("scale_mode", sprintf("Unrecognised scale_mode: %s. Allowed: %s",
        paste(sprintf("%s='%s'", bad_mode$parameter, bad_mode$scale_mode), collapse = ", "),
        paste(VALID_SCALE_MODES, collapse = ", ")))
  }
  bad_round <- p[!rounding %in% VALID_ROUNDING]
  if (nrow(bad_round) > 0) {
    err("rounding", sprintf("Unrecognised rounding: %s. Allowed: %s",
        paste(sprintf("%s='%s'", bad_round$parameter, bad_round$rounding), collapse = ", "),
        paste(VALID_ROUNDING, collapse = ", ")))
  }

  # ---- 3. anchor cells: populated and numeric where required --------------
  # "regime" parameters may hold text; "derived" ones are informational.
  numeric_needed <- p[!scale_mode %in% c("regime", "derived")]
  blank <- numeric_needed[is.na(small) | is.na(large)]
  if (nrow(blank) > 0) {
    err("anchors", sprintf(
      paste0("Anchor cell is blank or non-numeric for: %s. ",
             "Enter 0 if the value is genuinely zero; a blank is never assumed to mean zero."),
      paste(blank$parameter, collapse = ", ")))
  }

  # ---- 4. constant parameters must actually be constant -------------------
  const_diff <- p[scale_mode == "constant" & !is.na(small) & !is.na(large) & small != large]
  if (nrow(const_diff) > 0) {
    err("constant", sprintf(
      "scale_mode is 'constant' but the two anchors differ for: %s. Use 'log' or 'linear' instead.",
      paste(sprintf("%s (%s vs %s)", const_diff$parameter, const_diff$small, const_diff$large),
            collapse = ", ")))
  }

  # ---- 5. values inside their declared validation range -------------------
  rng <- p[!is.na(min) & !is.na(max) & !is.na(small) & !is.na(large)]
  out_lo <- rng[small < min | large < min]
  out_hi <- rng[small > max | large > max]
  for (i in seq_len(nrow(out_lo))) {
    err("range", sprintf("%s is below its minimum of %s (anchors: %s, %s).",
        out_lo$parameter[i], out_lo$min[i], out_lo$small[i], out_lo$large[i]))
  }
  for (i in seq_len(nrow(out_hi))) {
    err("range", sprintf("%s is above its maximum of %s (anchors: %s, %s).",
        out_hi$parameter[i], out_hi$min[i], out_hi$small[i], out_hi$large[i]))
  }
  bad_rng <- p[!is.na(min) & !is.na(max) & min > max]
  if (nrow(bad_rng) > 0) {
    err("range", sprintf("min exceeds max for: %s", paste(bad_rng$parameter, collapse = ", ")))
  }

  # ---- 6. schema agreement ------------------------------------------------
  in_schema  <- setdiff(raw$schema$parameter, p$parameter)
  in_params  <- setdiff(p$parameter, raw$schema$parameter)
  if (length(in_schema) > 0) {
    err("schema", sprintf("Present in %s but not on any parameter sheet: %s",
        SCHEMA_SHEET, paste(in_schema, collapse = ", ")))
  }
  if (length(in_params) > 0) {
    err("schema", sprintf("On a parameter sheet but missing from %s: %s",
        SCHEMA_SHEET, paste(in_params, collapse = ", ")))
  }

  # ---- 7. cross-parameter consistency ------------------------------------
  val <- function(nm, col = "small") {
    v <- p[parameter == nm][[col]]
    if (length(v) == 1L) v else NA_real_
  }

  # Parity shares describe the whole adult herd and must sum to 1.
  parity <- c("parity_share_1", "parity_share_2_3", "parity_share_4", "parity_share_5_plus")
  if (all(parity %in% p$parameter)) {
    for (col in c("small", "large")) {
      tot <- sum(vapply(parity, val, numeric(1), col = col))
      if (!is.na(tot) && abs(tot - 1) > 1e-6) {
        err("parity", sprintf("Parity shares sum to %.4f at the %s anchor, not 1.", tot, col))
      }
    }
  }

  # Lactation must fit inside the calving interval, since dry days are derived.
  for (col in c("small", "large")) {
    ci   <- val("calving_interval_months", col) * 30.42
    lact <- val("lactation_days", col)
    if (!is.na(ci) && !is.na(lact)) {
      if (lact >= ci) {
        err("reproduction", sprintf(
          "Lactation of %.0f days does not fit inside a calving interval of %.0f days at the %s anchor; derived dry days would be <= 0.",
          lact, ci, col))
      }
      sp  <- val("service_period_days", col)
      ges <- val("gestation_days", col)
      if (!is.na(sp) && !is.na(ges)) {
        gap <- (sp + ges) - ci
        if (abs(gap) > 30) {
          wrn("reproduction", sprintf(
            "At the %s anchor, service period + gestation = %.0f days but the calving interval implies %.0f days (gap %+.0f). Calving interval is the primary clock; the others are cross-checks.",
            col, sp + ges, ci, gap))
        }
      }
    }
  }

  # ---- 8. ration ----------------------------------------------------------
  r <- raw$ration
  for (cls in unique(r$animal_class)) {
    comp  <- r[animal_class == cls & !is_total]
    total <- r[animal_class == cls & is_total]
    if (nrow(total) == 0L) {
      wrn("ration", sprintf("Class '%s' has no TOTAL row.", cls))
      next
    }
    for (side in c("small", "large")) {
      kg   <- comp[[paste0("kg_", side)]]
      rate <- comp[[paste0("rate_", side)]]
      cost <- comp[[paste0("cost_", side)]]
      # component cost must equal kg x rate
      bad <- which(!is.na(kg) & !is.na(rate) & !is.na(cost) & abs(kg * rate - cost) > 0.01)
      if (length(bad) > 0) {
        err("ration", sprintf("Class '%s' (%s anchor): cost does not equal kg x rate for %s.",
            cls, side, paste(comp$feedstuff[bad], collapse = ", ")))
      }
      # stated total must equal the sum of components
      stated <- total[[paste0("cost_", side)]][1]
      summed <- sum(cost, na.rm = TRUE)
      if (!is.na(stated) && abs(stated - summed) > 0.01) {
        err("ration", sprintf(
          "Class '%s' (%s anchor): TOTAL says %.2f but the components sum to %.2f.",
          cls, side, stated, summed))
      }
    }
  }

  # ---- 9. equipment -------------------------------------------------------
  e <- raw$equipment
  if (nrow(e) == 0L) {
    err("equipment", "No equipment rows found.")
  } else {
    if (any(is.na(e$capacity_head) | e$capacity_head <= 0)) {
      err("equipment", sprintf("capacity_head must be a positive number. Offending item(s): %s",
          paste(e[is.na(capacity_head) | capacity_head <= 0, item], collapse = ", ")))
    }
    if (any(is.na(e$min_herd) | e$min_herd < 1)) {
      err("equipment", sprintf("min_herd must be at least 1. Offending item(s): %s",
          paste(e[is.na(min_herd) | min_herd < 1, item], collapse = ", ")))
    }
    bad_span <- e[!is.na(max_herd) & max_herd < min_herd]
    if (nrow(bad_span) > 0) {
      err("equipment", sprintf("max_herd is below min_herd for: %s",
          paste(bad_span$item, collapse = ", ")))
    }
    if (any(is.na(e$cost_per_unit) | e$cost_per_unit < 0)) {
      err("equipment", sprintf("cost_per_unit must be a non-negative number. Offending item(s): %s",
          paste(e[is.na(cost_per_unit) | cost_per_unit < 0, item], collapse = ", ")))
    }
    if (any(is.na(e$life_years) | e$life_years <= 0)) {
      err("equipment", sprintf("life_years must be positive. Offending item(s): %s",
          paste(e[is.na(life_years) | life_years <= 0, item], collapse = ", ")))
    }
    # Steps must not stack on one herd size.
    clash <- e[min_herd > 1, .N, by = min_herd][N > 1L]
    if (nrow(clash) > 0) {
      wrn("equipment", sprintf(
        paste0("More than one item activates at the same herd size (%s). ",
               "Synchronised steps produce cliffs in results; spread the thresholds apart."),
        paste(sprintf("%d head: %d items", clash$min_herd, clash$N), collapse = "; ")))
    }
  }

  # ---- 10. app defaults ---------------------------------------------------
  ad  <- raw$app_defaults
  src <- ad[!is.na(source_parameter) & source_parameter != "", source_parameter]
  # Only check references that name a single parameter (some point at a sheet).
  src <- src[!grepl("\\s", src)]
  unknown <- setdiff(src, p$parameter)
  if (length(unknown) > 0) {
    err("app_defaults", sprintf("source_parameter refers to unknown parameter(s): %s",
        paste(unknown, collapse = ", ")))
  }

  # ---- report -------------------------------------------------------------
  errors   <- collect(log, "error")
  warnings <- collect(log, "warning")

  if (length(warnings) > 0) {
    message(sprintf("Parameter workbook: %d warning(s)\n%s",
                    length(warnings), paste(warnings, collapse = "\n")))
  }
  if (length(errors) > 0 && strict) {
    stop(sprintf("Parameter workbook failed validation with %d error(s):\n%s\n\nSource: %s",
                 length(errors), paste(errors, collapse = "\n"), raw$source), call. = FALSE)
  }

  invisible(list(errors = errors, warnings = warnings))
}
