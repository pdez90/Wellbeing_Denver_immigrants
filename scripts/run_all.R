# =============================================================================
# run_all.R
# Runs the whole pipeline end to end, in dependency order.
#
#   Rscript run_all.R              # everything, including the spatial joins
#   Rscript run_all.R --analysis   # skip 01-06, re-run models and tables only
#
# The spatial stage (01-06) is slow -- budget 1-3 hours, most of it in the
# DRCOG land-cover raster extraction in script 02. The analysis stage (07-09)
# takes about five minutes.
#
# Each script runs in a FRESH environment. This is deliberate: several of the
# original chunks began with `if (!exists("dat"))`, which silently reused
# whatever `dat` was left in the session. That made results depend on the order
# you happened to click through the notebook. Now every script must read its
# input from disk, so the chain is explicit and verifiable.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
analysis_only <- "--analysis" %in% args

source("00_config.R")

spatial_scripts <- c(
  "01_geocode_acs.R",
  "02_enviro_zoning_walkability_landcover.R",
  "03_tree_parks.R",
  "04_transport_connectivity_crash.R",
  "05_ht_poi_stoz_diversity.R",
  "06_maps_descriptives.R"
)

analysis_scripts <- c(
  "07_tables_descriptive.R",
  "08_domain_models.R",
  "09_mediation.R",
  "10_greenness_sensitivity.R",
  "11_manuscript_tables.R",
  "12_figure_mediation_dag.R",
  "13_descriptives_all.R",
  "14_figure_coefficients.R",
  "15_buffer_sensitivity.R",
  "16_robustness.R"
)

scripts <- if (analysis_only) analysis_scripts else c(spatial_scripts, analysis_scripts)

run_one <- function(f) {
  cat("\n", strrep("=", 78), "\n== ", f, "\n", strrep("=", 78), "\n", sep = "")
  t0 <- Sys.time()
  env <- new.env(parent = globalenv())
  source("00_config.R", local = env)
  ok <- tryCatch({ source(f, local = env, echo = FALSE); TRUE },
                 error = function(e) { message("FAILED: ", f, "\n  ", conditionMessage(e)); FALSE })
  cat(sprintf("== %s %s in %.1f min\n", f, if (ok) "finished" else "FAILED",
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  ok
}

results <- vapply(scripts, run_one, logical(1))

cat("\n", strrep("=", 78), "\nPIPELINE SUMMARY\n", strrep("=", 78), "\n", sep = "")
for (i in seq_along(scripts)) {
  cat(sprintf("  %-45s %s\n", scripts[i], if (results[i]) "OK" else "FAILED"))
}

wb_write_session_info()

if (!all(results)) {
  quit(status = 1)
}
