# Loaded by testthat before the test files.

for (f in list.files(test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

#' Deep copy a raw parameter object so a test can corrupt it safely.
#' data.table modifies in place, so a shallow copy would leak between tests.
copy_raw <- function(raw) {
  out <- raw
  for (nm in c("params", "ration", "equipment", "app_defaults", "schema")) {
    out[[nm]] <- data.table::copy(raw[[nm]])
  }
  out
}
