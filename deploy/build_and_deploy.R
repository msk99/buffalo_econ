# build_and_deploy.R -- bundle the app for shinyapps.io.
#
# Builds a self-contained bundle in deploy/bundle/. Run from the project
# root:  Rscript deploy/build_and_deploy.R
#
# Deployment is a separate, deliberate step, gated on the validation
# sign-off (reproducing two reference project reports). Credentials belong in
# ~/.Renviron (SHINYAPPS_TOKEN / SHINYAPPS_SECRET), never in this file.

bundle <- file.path("deploy", "bundle")
if (dir.exists(bundle)) unlink(bundle, recursive = TRUE)
dir.create(file.path(bundle, "R"), recursive = TRUE)
dir.create(file.path(bundle, "params"))

# app/app.R already resolves the engine from ./R and the workbook from
# ./params when ../R does not exist, so the bundle needs no code rewriting.
stopifnot(file.copy("app/app.R", bundle))
stopifnot(file.copy(list.files("R", full.names = TRUE), file.path(bundle, "R")))
stopifnot(file.copy("params/parameters_v3_2026-08.xlsx", file.path(bundle, "params")))

cat("Bundle created:", normalizePath(bundle), "\n")
cat(paste(" ", list.files(bundle, recursive = TRUE)), sep = "\n")

# ---- deploy (only when explicitly requested) ---------------------------------
if (identical(Sys.getenv("BUFFALO_DEPLOY"), "yes")) {
  library(rsconnect)
  rsconnect::setAccountInfo(name = "khatkar",
                            token = Sys.getenv("SHINYAPPS_TOKEN"),
                            secret = Sys.getenv("SHINYAPPS_SECRET"))
  rsconnect::deployApp(appDir = bundle, appName = "buffalo_planner",
                       account = "khatkar", forceUpdate = TRUE)
} else {
  cat("\nNot deploying. To deploy after validation sign-off:\n",
      "  BUFFALO_DEPLOY=yes Rscript deploy/build_and_deploy.R\n")
}
