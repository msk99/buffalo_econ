# setup.R -- install everything needed to run the model, app and tests.
# Run once after cloning:  Rscript setup.R

required <- c(
  "data.table",   # engine: all tabular computation
  "openxlsx2",    # workbook in, Excel report out
  "shiny",        # application
  "bslib",        # application theming
  "DT",           # application tables
  "ggplot2",      # application charts
  "testthat"      # test suite
)

missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) == 0) {
  message("All required packages are already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# quick self-check: load the engine and run one projection
ok <- tryCatch({
  for (f in list.files("R", full.names = TRUE)) source(f)
  r <- run_buffalo_model(50, 5)
  is.finite(r$summary$value[1])
}, error = function(e) { message("Self-check failed: ", conditionMessage(e)); FALSE })

if (ok) message("Setup complete. Try:  R -e 'shiny::runApp(\"app\", launch.browser = TRUE)'")
