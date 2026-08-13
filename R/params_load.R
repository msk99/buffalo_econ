# params_load.R -- read the parameter workbook into a typed object.
#
# The workbook is the domain-expert interface; this file is the only place that
# knows its physical layout. Everything downstream sees data.tables.
#
# Layout contract (see sheet 00_Read_Me):
#   row 1  sheet title
#   row 2  sheet note
#   row 3  scaling rule
#   row 4  column headers
#   row 5+ data, contiguous, no explanatory rows below
#
# Anchor herd sizes are read from the column headers (small_10 / large_200) so
# that moving an anchor is a workbook edit, not a code change.

library(data.table)
library(openxlsx2)

# Sheets using the standard parameter layout.
PARAM_SHEETS <- c(
  "01_Herd_Biology", "02_Milk_Production", "04_Labour", "05_Infrastructure",
  "07_Health_Breeding", "08_Other_Costs", "09_Revenue", "10_Finance_Loan", "11_Tax"
)

RATION_SHEET    <- "03_Feed_Ration"
EQUIPMENT_SHEET <- "06_Equipment"
DEFAULTS_SHEET  <- "12_App_Defaults"
SCHEMA_SHEET    <- "13_Schema"

HEADER_ROW <- 4L

VALID_SCALE_MODES <- c("constant", "log", "linear", "derived", "capacity", "regime")
VALID_ROUNDING    <- c("none", "int", "ceil")


#' Read one sheet, taking row 4 as the header and row 5 onward as data
#' @return data.table with character columns, trimmed, fully-blank rows dropped
read_sheet <- function(path, sheet) {
  dt <- as.data.table(read_xlsx(
    path, sheet = sheet, start_row = HEADER_ROW, col_names = TRUE, skip_empty_rows = FALSE
  ))
  # Coerce everything to character first; typing happens after validation so
  # that a bad cell produces a clear message rather than a silent NA.
  dt[, names(dt) := lapply(.SD, function(x) trimws(as.character(x)))]
  dt[dt == "" ] <- NA
  # Drop rows that are blank across every column (the sheet separators).
  keep <- dt[, rowSums(!is.na(.SD)) > 0]
  dt[keep]
}


#' Extract the anchor herd sizes declared by the column headers
#' @param cols character vector of column names from a parameter sheet
#' @return named integer vector c(small = , large = )
parse_anchors <- function(cols) {
  small <- grep("^small_[0-9]+$", cols, value = TRUE)
  large <- grep("^large_[0-9]+$", cols, value = TRUE)
  if (length(small) != 1L || length(large) != 1L) {
    stop(sprintf(
      paste0("Cannot determine anchor herd sizes from the column headers.\n",
             "Expected exactly one 'small_<n>' and one 'large_<n>' column, found: %s"),
      paste(c(small, large), collapse = ", ")
    ), call. = FALSE)
  }
  out <- c(
    small = as.integer(sub("^small_", "", small)),
    large = as.integer(sub("^large_", "", large))
  )
  if (out[["small"]] >= out[["large"]]) {
    stop(sprintf("Small anchor (%d) must be below the large anchor (%d).",
                 out[["small"]], out[["large"]]), call. = FALSE)
  }
  out
}


#' Load the whole parameter workbook
#'
#' @param path path to parameters_v3_*.xlsx
#' @param validate run validate_parameters() before returning (default TRUE)
#' @return object of class "buffalo_params_raw"
load_parameters <- function(path = NULL, validate = TRUE) {

  if (is.null(path)) path <- find_param_file()
  if (!file.exists(path)) {
    stop(sprintf("Parameter workbook not found: %s", path), call. = FALSE)
  }

  present <- wb_get_sheet_names(wb_load(path))
  required <- c(PARAM_SHEETS, RATION_SHEET, EQUIPMENT_SHEET, DEFAULTS_SHEET, SCHEMA_SHEET)
  missing  <- setdiff(required, present)
  if (length(missing) > 0) {
    stop(sprintf("Parameter workbook is missing required sheet(s): %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }

  # ---- scalar parameter sheets -------------------------------------------
  parts <- lapply(PARAM_SHEETS, function(sh) {
    dt <- read_sheet(path, sh)
    dt[, sheet := sh]
    dt
  })
  anchors <- parse_anchors(names(parts[[1]]))
  small_col <- paste0("small_", anchors[["small"]])
  large_col <- paste0("large_", anchors[["large"]])

  params <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  setnames(params, c(small_col, large_col), c("small", "large"))
  params <- params[!is.na(parameter)]

  # Type the numeric columns. scale_mode "regime" may legitimately hold text
  # (for example entity_type = "proprietor"), so keep the raw string alongside.
  params[, small_raw := small]
  params[, large_raw := large]
  suppressWarnings({
    params[, small := as.numeric(small_raw)]
    params[, large := as.numeric(large_raw)]
    params[, min   := as.numeric(min)]
    params[, max   := as.numeric(max)]
  })
  setcolorder(params, c("parameter", "sheet", "label", "small", "large",
                        "unit", "basis", "scale_mode", "rounding", "min", "max"))

  # ---- ration ------------------------------------------------------------
  ration <- read_sheet(path, RATION_SHEET)
  ration <- ration[!is.na(animal_class) & !is.na(feedstuff)]
  num_cols <- grep("^(kg_per_day|rate_per_kg|cost_per_day)_", names(ration), value = TRUE)
  suppressWarnings(ration[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols])
  setnames(ration,
           c(paste0("kg_per_day_small_",   anchors[["small"]]),
             paste0("rate_per_kg_small_",  anchors[["small"]]),
             paste0("kg_per_day_large_",   anchors[["large"]]),
             paste0("rate_per_kg_large_",  anchors[["large"]]),
             paste0("cost_per_day_small_", anchors[["small"]]),
             paste0("cost_per_day_large_", anchors[["large"]])),
           c("kg_small", "rate_small", "kg_large", "rate_large", "cost_small", "cost_large"))
  ration[, is_total := toupper(trimws(feedstuff)) == "TOTAL"]

  # ---- equipment ---------------------------------------------------------
  equipment <- read_sheet(path, EQUIPMENT_SHEET)
  equipment <- equipment[!is.na(item)]
  suppressWarnings(equipment[, `:=`(
    min_herd      = as.numeric(min_herd),
    max_herd      = as.numeric(max_herd),
    capacity_head = as.numeric(capacity_head),
    cost_per_unit = as.numeric(cost_per_unit_inr),
    life_years    = as.numeric(life_years)
  )])
  equipment[, cost_per_unit_inr := NULL]
  # Light Option B automation: rows flagged 1 are bought only on a mechanised farm.
  if ("needs_automation" %in% names(equipment)) {
    suppressWarnings(equipment[, needs_automation := as.numeric(needs_automation)])
    equipment[is.na(needs_automation), needs_automation := 0]
  } else {
    equipment[, needs_automation := 0]
  }

  # ---- app defaults ------------------------------------------------------
  app_defaults <- read_sheet(path, DEFAULTS_SHEET)
  app_defaults <- app_defaults[!is.na(control)]

  # ---- schema ------------------------------------------------------------
  schema <- read_sheet(path, SCHEMA_SHEET)
  schema <- schema[!is.na(parameter)]
  suppressWarnings(schema[, `:=`(min = as.numeric(min), max = as.numeric(max))])

  raw <- structure(
    list(
      params       = params[],
      ration       = ration[],
      equipment    = equipment[],
      app_defaults = app_defaults[],
      schema       = schema[],
      anchors      = anchors,
      source       = normalizePath(path),
      loaded_at    = Sys.time()
    ),
    class = "buffalo_params_raw"
  )

  if (validate) validate_parameters(raw)
  raw
}


#' Locate the parameter workbook without hard-coding a path
find_param_file <- function() {
  candidates <- c(
    "params/parameters_v3_2026-08.xlsx",
    file.path("..", "params", "parameters_v3_2026-08.xlsx"),
    "data/parameters.xlsx",
    "parameters.xlsx"
  )
  # Also accept any parameters_v*.xlsx in a params/ folder at or above the wd.
  for (dir in c("params", file.path("..", "params"))) {
    if (dir.exists(dir)) {
      hits <- sort(list.files(dir, pattern = "^parameters_v.*\\.xlsx$", full.names = TRUE),
                   decreasing = TRUE)
      candidates <- c(candidates, hits)
    }
  }
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    stop("Cannot locate a parameter workbook. Pass path= explicitly.", call. = FALSE)
  }
  hit[1]
}


#' @export
print.buffalo_params_raw <- function(x, ...) {
  cat("<buffalo parameter workbook>\n")
  cat(sprintf("  source   : %s\n", basename(x$source)))
  cat(sprintf("  anchors  : %d and %d head\n", x$anchors[["small"]], x$anchors[["large"]]))
  cat(sprintf("  params   : %d across %d sheets\n",
              nrow(x$params), length(unique(x$params$sheet))))
  cat(sprintf("  ration   : %d rows (%d classes)\n",
              nrow(x$ration), length(unique(x$ration$animal_class))))
  cat(sprintf("  equipment: %d items\n", nrow(x$equipment)))
  cat(sprintf("  modes    : %s\n",
              paste(sprintf("%s=%d", names(table(x$params$scale_mode)),
                            as.integer(table(x$params$scale_mode))), collapse = ", ")))
  invisible(x)
}
