## ============================================================================ ##
##  FxS 2026 — Master Replication Script                                        ##
##  Requires: R >= 4.3, packages installed via code/00_packages.R               ##
##  Working directory: fxs_2026/ (this file's directory)                        ##
##                                                                               ##
##  NOTE: lfe does NOT compile on Apple Silicon (arm64). This package uses       ##
##  fixest::feols() as a drop-in replacement. Estimates are identical.           ##
##                                                                               ##
##  Scripts NOT included (require data not in repo):                             ##
##    - 05_mechanism/tables/01_tbl_test_household_survey.R (needs survey data)   ##
##    - 05_mechanism/tables/02_tbl_test_public_services.R  (needs survey data)   ##
##    - 06_roads/tables/05_tbl_test_roads_household_survey.R (needs survey data) ##
##    - 08_robustness/tables/01-03 (needs SECOP data)                            ##
##    - 07_descriptives/figures/01-05 (needs external shapefiles)                ##
## ============================================================================ ##

## Set working directory to fxs_2026/ regardless of how the script is invoked
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  ## When run via Rscript, use the script's own location
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])
  if (length(script_path) > 0 && nchar(script_path) > 0) {
    setwd(dirname(normalizePath(script_path)))
  }
}
library(here)

## ── Track results ─────────────────────────────────────────────────────────────
results <- list()

run_script <- function(path, label) {
  cat(sprintf("\n%s\n── Running: %s\n", strrep("=", 60), label))
  tryCatch({
    source(here(path), local = new.env())
    results[[label]] <<- "OK"
    cat(sprintf("── DONE: %s\n", label))
  }, error = function(e) {
    results[[label]] <<- paste("ERROR:", conditionMessage(e))
    cat(sprintf("── ERROR in %s:\n   %s\n", label, conditionMessage(e)))
  })
}

## ── 04 Main Regressions ───────────────────────────────────────────────────────
run_script("code/04_main_regressions/01_event_study_crops.R",     "Event Study: Main (crops)")
run_script("code/04_main_regressions/02_main_reg_table.R",        "Main Regression Table")
run_script("code/04_main_regressions/03_staggered_table.R",       "Staggered Regression Table")
run_script("code/04_main_regressions/04_staggered_event_study.R", "Staggered Event Study")

## ── 05 Mechanisms ────────────────────────────────────────────────────────────
run_script("code/05_mechanism/01_event_study_mechanism.R", "Event Studies: Mechanisms")
run_script("code/05_mechanism/02_mechanism_table.R",       "Mechanism Table: Economic Activity")
run_script("code/05_mechanism/03_enforcement_table.R",     "Mechanism Table: Enforcement")

## ── 06 Roads ─────────────────────────────────────────────────────────────────
run_script("code/06_roads/01_roads_main_table.R",        "Roads × Crops Table")
run_script("code/06_roads/02_placa_huella_mechanism.R",  "Placa Huella × Mechanism")
run_script("code/06_roads/03_roads_mechanism.R",         "Rural Roads × Mechanism")

## ── 08 Robustness ────────────────────────────────────────────────────────────
run_script("code/08_robustness/01_donut_test.R",     "Robustness: Donut Test (Ring2 controls)")
run_script("code/08_robustness/02_conley_se.R",      "Robustness: Conley Spatial SE")
run_script("code/08_robustness/03_leave_one_out.R",  "Robustness: Leave-One-Out (doubly-treated)")

## ── Summary ───────────────────────────────────────────────────────────────────
cat(sprintf("\n%s\nREPLICATION SUMMARY\n%s\n", strrep("=", 60), strrep("=", 60)))
for (nm in names(results)) {
  status <- ifelse(results[[nm]] == "OK", "✓", "✗")
  cat(sprintf("  %s  %s\n      %s\n", status, nm, results[[nm]]))
}
n_ok  <- sum(sapply(results, `==`, "OK"))
n_err <- length(results) - n_ok
cat(sprintf("\n  %d/%d scripts completed successfully\n", n_ok, length(results)))

## ── Output inventory ──────────────────────────────────────────────────────────
cat(sprintf("\nOutput files generated:\n"))
tabs <- list.files(here("output/tables"), pattern = "\\.tex$", recursive = TRUE, full.names = FALSE)
figs <- list.files(here("output/figures"), pattern = "\\.(png|pdf)$", recursive = TRUE, full.names = FALSE)
cat("  Tables:", length(tabs), "\n")
cat("  Figures:", length(figs), "\n")
