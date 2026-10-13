# =============================================================================
# app/prepare_app_data.R
# Assemble the aggregate-only inputs for the Shiny explorer.
#
#   WELLBEING_DIR=~/Wellbeing/data Rscript app/prepare_app_data.R
#
# PRIVACY RULE. Nothing written to app/data/ or app/www/ is a respondent row.
# The app reads model output, descriptive tables, and census-geography
# aggregates only. Respondent counts by tract are suppressed below
# MIN_CELL. The one file in the pipeline that holds respondent rows
# (model_data_domain_models.csv) is never copied, and the analysis file is
# read here only to count respondents per tract and to average public
# area-level measures within tracts and block groups -- values that are
# identical for everyone in the unit and come from public sources.
#
# Run from the repository root. Needs internet the first time for the
# census geometry (tigris caches it).
# =============================================================================

MIN_CELL <- 5

data_dir <- Sys.getenv("WELLBEING_DIR", unset = "")
if (!nzchar(data_dir)) stop("Set WELLBEING_DIR to the data folder (the one with swb_data.csv).")
app_dir  <- "app"
if (!dir.exists(app_dir)) stop("Run from the repository root; app/ not found.")
dir.create(file.path(app_dir, "data"), showWarnings = FALSE)
dir.create(file.path(app_dir, "www"),  showWarnings = FALSE)

need <- c("dplyr", "readr", "sf", "tigris", "janitor")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("Install first: install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))")
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sf) })
options(tigris_use_cache = TRUE)

src <- function(...) file.path(data_dir, ...)
dst <- function(...) file.path(app_dir, "data", ...)
cp  <- function(from, to = basename(from)) {
  if (!file.exists(from)) { message("  missing (skipped): ", from); return(invisible(FALSE)) }
  file.copy(from, dst(to), overwrite = TRUE); invisible(TRUE)
}

# ---- 1. Aggregate outputs: copied as they are ---------------------------------
message("Copying aggregate outputs")
agg <- c(
  "all_model_coefficients.csv", "model_fit_summary.csv",
  "formal_mediation_results_domain_models.csv", "mediation_sensitivity.csv",
  "greenness_sensitivity.csv", "buffer_sensitivity.csv", "buffer_sensitivity_wide.csv",
  "robustness_common_sample.csv", "robustness_standard_errors.csv",
  "robustness_robust_regression.csv", "robustness_swb_leave_one_out.csv",
  "zoning_joint_tests.csv", "vif_diagnostics.csv", "integrated_model_selection.csv",
  "variable_table.csv", "rebuild_verification.csv"
)
for (f in agg) cp(src(f))
for (f in list.files(src("manuscript_tables"), pattern = "^Table\\d+\\.csv$|table_numbering.csv", full.names = TRUE))
  cp(f)
dir.create(dst("descriptives"), showWarnings = FALSE)
for (f in list.files(src("descriptives"), pattern = "\\.csv$", full.names = TRUE))
  file.copy(f, dst("descriptives", basename(f)), overwrite = TRUE)
for (f in c("Figure2_Coefficients.csv", "Figure3_IntegratedModel.csv", "si_map_counts_by_tract.csv"))
  cp(src("figures", f))

# figures
message("Copying figures")
for (f in c("Figure1_Framework.png", "Figure1_Mediation_DAG.png", "Figure2_Coefficients.png",
            "Figure3_IntegratedModel.png", "Graphical_Abstract.png",
            "FigureS13_MediationSensitivity.png", "Figure_S10_respondents.png",
            "Figure_S11_zoning.png", "Figure_S12_measures.png")) {
  p <- src("figures", f); if (file.exists(p)) file.copy(p, file.path(app_dir, "www", f), overwrite = TRUE)
}
for (f in list.files(src("descriptives", "figures"), pattern = "^Panel_.*\\.png$", full.names = TRUE))
  file.copy(f, file.path(app_dir, "www", basename(f)), overwrite = TRUE)

# ---- 2. Census-geography aggregates ------------------------------------------
message("Building tract and block-group aggregates")
analysis_file <- if (file.exists(src("respondents_with_new_variables.csv")))
  src("respondents_with_new_variables.csv") else src("respondents_with_transport_ht_segpoi_stoz_diversity.csv")
dat <- readr::read_csv(analysis_file, show_col_types = FALSE, guess_max = 5000) %>% janitor::clean_names()
pad <- function(x, w) { x <- gsub("\\.0+$", "", as.character(x)); x[!is.na(x)] <- formatC(x[!is.na(x)], width = w, flag = "0"); x }
dat$tract_geoid <- pad(dat$tract_geoid, 11)
dat$bg_geoid    <- pad(dat$bg_geoid, 12)

# respondent counts by tract, suppressed
counts <- dat %>% filter(!is.na(tract_geoid)) %>% count(tract_geoid, name = "n") %>%
  mutate(n_shown = ifelse(n < MIN_CELL, NA_integer_, n),
         label   = ifelse(n < MIN_CELL, paste0("fewer than ", MIN_CELL), as.character(n))) %>%
  select(-n)
message("  tracts with respondents: ", nrow(counts), "; shown exactly: ", sum(!is.na(counts$n_shown)))

# public area-level measures, averaged within their own geography
area_vars <- list(
  list(col = "pop_density",            geo = "tract", lab = "Population density",            unit = "persons per sq km (ACS 2015-2019)"),
  list(col = "pct_poverty",            geo = "tract", lab = "Population below poverty",      unit = "% (ACS 2015-2019)"),
  list(col = "pct_non_native",         geo = "tract", lab = "Foreign-born population",       unit = "% (ACS 2015-2019)"),
  list(col = "pct_nh_white",           geo = "tract", lab = "Non-Hispanic white population", unit = "% (ACS 2015-2019)"),
  list(col = "walk_nat_walk_ind",      geo = "bg",    lab = "EPA National Walkability Index", unit = "index, 1-20"),
  list(col = "tree_tes",               geo = "bg",    lab = "Tree Equity Score",             unit = "score, 0-100"),
  list(col = "hudjob_jobs_idx",        geo = "bg",    lab = "HUD Jobs Proximity Index",      unit = "index, 0-100"),
  list(col = "env_pm25",               geo = "bg",    lab = "Fine particle pollution (PM2.5)", unit = "micrograms per cubic metre (EnviroScreen)")
)
area_vars <- Filter(function(v) v$col %in% names(dat), area_vars)
agg_by <- function(geo) {
  key <- if (geo == "tract") "tract_geoid" else "bg_geoid"
  vs  <- Filter(function(v) v$geo == geo, area_vars)
  if (!length(vs)) return(NULL)
  dat %>% filter(!is.na(.data[[key]])) %>%
    group_by(GEOID = .data[[key]]) %>%
    summarise(across(all_of(vapply(vs, `[[`, "", "col")), ~ mean(suppressWarnings(as.numeric(.x)), na.rm = TRUE)), .groups = "drop")
}
tr_vals <- agg_by("tract"); bg_vals <- agg_by("bg")

# geometry
message("Fetching census geometry (tigris, 2019 cartographic boundaries)")
metro_names <- c("Denver", "Adams", "Arapahoe", "Jefferson", "Douglas", "Broomfield")
co  <- tigris::counties(state = "CO", cb = TRUE, year = 2019, progress_bar = FALSE)
metro <- co[co$NAME %in% metro_names, ] %>% st_transform(4326) %>% select(NAME)
tr  <- tigris::tracts(state = "CO", cb = TRUE, year = 2019, progress_bar = FALSE) %>% st_transform(4326)
bg  <- tigris::block_groups(state = "CO", cb = TRUE, year = 2019, progress_bar = FALSE) %>% st_transform(4326)
metro_fips <- unique(co$COUNTYFP[co$NAME %in% metro_names])
tr_metro <- tr[tr$COUNTYFP %in% metro_fips, c("GEOID")]
bg_metro <- bg[bg$COUNTYFP %in% metro_fips, c("GEOID")]

tr_metro <- tr_metro %>% left_join(counts, by = c("GEOID" = "tract_geoid"))
if (!is.null(tr_vals)) tr_metro <- tr_metro %>% left_join(tr_vals, by = "GEOID")
if (!is.null(bg_vals)) bg_metro <- bg_metro %>% left_join(bg_vals, by = "GEOID")
bg_cols  <- vapply(Filter(function(v) v$geo == "bg", area_vars), `[[`, "", "col")
if (length(bg_cols)) bg_metro <- bg_metro[rowSums(!is.na(st_drop_geometry(bg_metro)[, bg_cols, drop = FALSE])) > 0, ]

# simplify for a light map
tr_metro <- st_simplify(tr_metro, dTolerance = 30, preserveTopology = TRUE)
bg_metro <- st_simplify(bg_metro, dTolerance = 30, preserveTopology = TRUE)
metro    <- st_simplify(metro,    dTolerance = 50, preserveTopology = TRUE)

saveRDS(list(metro = metro, tracts = tr_metro, block_groups = bg_metro,
             area_vars = area_vars, min_cell = MIN_CELL,
             built = format(Sys.time(), "%Y-%m-%d")), dst("geo.rds"))

# ---- 3. A manifest, so the privacy claim is checkable -----------------------
files <- list.files(file.path(app_dir, "data"), recursive = TRUE)
writeLines(c("# Files the app reads. None contains a respondent row.",
             paste0("- ", files)), file.path(app_dir, "data", "MANIFEST.md"))
message("Done. ", length(files), " files in app/data; run with: shiny::runApp('app')")
