# =============================================================================
# app/prepare_app_data.R
# Assemble the aggregate-only inputs for the Shiny explorer.
#
#   WELLBEING_DIR=~/Wellbeing/data Rscript app/prepare_app_data.R
#
# PRIVACY RULE. Nothing written to app/data/ or app/www/ is a respondent row.
# The app reads model output, descriptive tables, and one census-geography
# aggregate: respondent counts by tract, suppressed below MIN_CELL. The one
# file in the pipeline that holds respondent rows
# (model_data_domain_models.csv) is never copied, and the analysis file is
# read here only to count respondents per tract. The area-level layers are
# built from the public source files for EVERY tract and block group in the
# six metro counties, never from respondent rows: a set of polygons drawn
# only where respondents live would disclose residence at block-group
# resolution, finer than the suppression rule.
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

need <- c("dplyr", "readr", "purrr", "sf", "tigris", "janitor", "tidycensus")
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
message("Counting respondents by tract")
analysis_file <- if (file.exists(src("respondents_with_new_variables.csv")))
  src("respondents_with_new_variables.csv") else src("respondents_with_transport_ht_segpoi_stoz_diversity.csv")
dat <- readr::read_csv(analysis_file, show_col_types = FALSE, guess_max = 5000) %>% janitor::clean_names()
pad <- function(x, w) {
  x <- gsub("\\.0+$", "", as.character(x)); ok <- !is.na(x)
  x[ok] <- paste0(strrep("0", pmax(0, w - nchar(x[ok]))), x[ok])   # formatC pads character with blanks, not zeros
  if (any(nchar(x[ok]) != w)) stop("GEOID of unexpected width after padding")
  x
}
dat$tract_geoid <- pad(dat$tract_geoid, 11)

# respondent counts by tract, suppressed
counts <- dat %>% filter(!is.na(tract_geoid)) %>% count(tract_geoid, name = "n") %>%
  mutate(n_shown = ifelse(n < MIN_CELL, NA_integer_, n),
         label   = ifelse(n < MIN_CELL, paste0("fewer than ", MIN_CELL), as.character(n))) %>%
  select(-n)
message("  tracts with respondents: ", nrow(counts), "; shown exactly: ", sum(!is.na(counts$n_shown)))

# geometry
message("Fetching census geometry (tigris, 2019 cartographic boundaries)")
metro_names <- c("Denver", "Adams", "Arapahoe", "Jefferson", "Douglas", "Broomfield")
co  <- tigris::counties(state = "CO", cb = TRUE, year = 2019, progress_bar = FALSE)
metro <- co[co$NAME %in% metro_names, ] %>% st_transform(4326) %>% select(NAME)
tr  <- tigris::tracts(state = "CO", cb = TRUE, year = 2019, progress_bar = FALSE) %>% st_transform(4326)
metro_fips <- unique(co$COUNTYFP[co$NAME %in% metro_names])
# A respondent tract that matches no Colorado tract means a GEOID problem
# (padding, vintage), so stop. A tract that matches but lies outside the six
# metro counties is a respondent living just beyond them: keep it on the map.
unmatched <- setdiff(counts$tract_geoid, tr$GEOID)
if (length(unmatched)) stop(length(unmatched), " respondent tract(s) match no 2019 Colorado tract, e.g. ", unmatched[1])
outside <- setdiff(counts$tract_geoid, tr$GEOID[tr$COUNTYFP %in% metro_fips])
if (length(outside)) message("  ", length(outside), " respondent tract(s) lie outside the six metro counties; kept")
tr_metro <- tr[tr$COUNTYFP %in% metro_fips | tr$GEOID %in% counts$tract_geoid, c("GEOID")] %>%
  left_join(counts, by = c("GEOID" = "tract_geoid"))

# ---- 3. Public area-level layers for every unit in the six counties ---------
# Each source is read in full from its public file (or the Census API) and
# joined to census geometry of its own vintage. A source that fails is skipped
# with a message rather than stopping the build.
message("Building public area-level layers (all metro tracts and block groups)")
bg10 <- tigris::block_groups(state = "CO", cb = TRUE, year = 2019, progress_bar = FALSE) %>%   # 2010 geography
  filter(COUNTYFP %in% metro_fips) %>% st_transform(4326) %>% select(GEOID, ALAND)
bg20 <- tigris::block_groups(state = "CO", cb = TRUE, year = 2020, progress_bar = FALSE) %>%   # 2020 geography
  filter(COUNTYFP %in% metro_fips) %>% st_transform(4326) %>% select(GEOID)
tr_layers <- tr[tr$COUNTYFP %in% metro_fips, c("GEOID")]
area_vars <- list()
add_layer <- function(layer, col, lab, unit, geo) {
  area_vars[[length(area_vars) + 1]] <<- list(col = col, lab = lab, unit = unit, layer = layer, geo = geo)
}
try_src <- function(what, expr) tryCatch(expr, error = function(e) { message("  skipped ", what, ": ", conditionMessage(e)); NULL })
metro_cty <- c("Denver", "Adams", "Arapahoe", "Jefferson", "Douglas", "Broomfield")

# ACS 2015-2019 (needs the Census API key that script 01 uses)
acs_bg <- try_src("ACS block groups", purrr::map_dfr(metro_cty, function(cn) tidycensus::get_acs(
  geography = "block group", state = "CO", county = cn, year = 2019, survey = "acs5", output = "wide",
  variables = c(pop = "B01003_001", pov_tot = "C17002_001", pov_lt50 = "C17002_002", pov_50_99 = "C17002_003",
                race_tot = "B03002_001", nh_white = "B03002_003"))))
if (!is.null(acs_bg)) {
  acs_bg <- acs_bg %>% transmute(GEOID,
    acs_pct_poverty = 100 * (pov_lt50E + pov_50_99E) / pov_totE,
    acs_pct_nh_white = 100 * nh_whiteE / race_totE, acs_pop = popE)
  bg10 <- bg10 %>% left_join(acs_bg, by = "GEOID") %>%
    mutate(acs_pop_density = ifelse(ALAND > 0, acs_pop / (ALAND / 1e6), NA_real_))
  add_layer("bg10", "acs_pop_density",  "Population density",            "persons per sq km, block group (ACS 2015-2019)", "block group")
  add_layer("bg10", "acs_pct_poverty",  "Population below poverty",      "%, block group (ACS 2015-2019)", "block group")
  add_layer("bg10", "acs_pct_nh_white", "Non-Hispanic white population", "%, block group (ACS 2015-2019)", "block group")
}
acs_tr <- try_src("ACS tracts", purrr::map_dfr(metro_cty, function(cn) tidycensus::get_acs(
  geography = "tract", state = "CO", county = cn, year = 2019, survey = "acs5", output = "wide",
  variables = c(nat_tot = "B05012_001", fborn = "B05012_003"))))
if (!is.null(acs_tr)) {
  tr_layers <- tr_layers %>% left_join(acs_tr %>% transmute(GEOID, acs_pct_foreign_born = 100 * fbornE / nat_totE), by = "GEOID")
  add_layer("tr", "acs_pct_foreign_born", "Foreign-born population", "%, tract (ACS 2015-2019)", "tract")
}

# EPA National Walkability Index (2010 block groups)
walk <- try_src("Walkability Index", {
  gdb <- src("WalkabilityIndex", "Natl_WI.gdb"); lyr <- sf::st_layers(gdb)$name[1]
  w <- sf::st_read(gdb, layer = lyr, quiet = TRUE,
                   query = sprintf("SELECT * FROM \"%s\" WHERE STATEFP = '08'", lyr)) %>% st_drop_geometry()
  gcol <- intersect(c("GEOID10", "GEOID20", "GEOID"), names(w))[1]
  data.frame(GEOID = pad(w[[gcol]], 12), walk_index = as.numeric(w$NatWalkInd))
})
if (!is.null(walk)) {
  bg10 <- bg10 %>% left_join(walk, by = "GEOID")
  add_layer("bg10", "walk_index", "EPA National Walkability Index", "index 1-20, block group (EPA 2021)", "block group")
}

# HUD Jobs Proximity Index, Colorado EnviroScreen and Tree Equity Score (2020 block groups)
jobs <- try_src("Jobs Proximity Index", {
  f <- list.files(data_dir, pattern = "^Jobs_Proximity_Index.*\\.csv$", full.names = TRUE)[1]
  readr::read_csv(f, col_types = readr::cols(GEOID = "c", .default = "?"), show_col_types = FALSE) %>%
    filter(STUSAB == "CO") %>% transmute(GEOID = pad(GEOID, 12), jobs_idx = as.numeric(jobs_idx))
})
if (!is.null(jobs)) {
  bg20 <- bg20 %>% left_join(jobs, by = "GEOID")
  add_layer("bg20", "jobs_idx", "HUD Jobs Proximity Index", "index 0-100, block group (HUD 2020)", "block group")
}
env <- try_src("EnviroScreen", {
  e <- readr::read_csv(src("Enviroscreen.csv"), col_types = readr::cols(GEOID = "c", .default = "?"), show_col_types = FALSE)
  pm <- grep("^Fine Particle Pollution \\(", names(e), value = TRUE)[1]
  npl <- grep("^Proximity to National Priorities List Sites$", names(e), value = TRUE)[1]
  data.frame(GEOID = pad(e$GEOID, 12), env_pm25 = as.numeric(e[[pm]]), env_npl = as.numeric(e[[npl]]))
})
if (!is.null(env)) {
  bg20 <- bg20 %>% left_join(env, by = "GEOID")
  add_layer("bg20", "env_pm25", "Fine particle pollution (PM2.5)", "\u00b5g/m\u00b3, block group (Colorado EnviroScreen)", "block group")
  add_layer("bg20", "env_npl",  "Superfund (NPL) site proximity",  "index, block group (Colorado EnviroScreen)", "block group")
}
tes <- try_src("Tree Equity Score", {
  t <- sf::st_read(src("co_shp", "co_tes.shp"), quiet = TRUE) %>% st_drop_geometry()
  data.frame(GEOID = pad(t$GEOID, 12), tree_tes = as.numeric(t$tes), tree_canopy = 100 * as.numeric(t$treecanopy))
})
if (!is.null(tes)) {
  bg20 <- bg20 %>% left_join(tes, by = "GEOID")
  add_layer("bg20", "tree_tes",    "Tree Equity Score",  "score 0-100, block group (urban areas only; American Forests)", "block group")
  add_layer("bg20", "tree_canopy", "Tree canopy (Tree Equity)", "% of block group (urban areas only; American Forests)", "block group")
}
for (v in area_vars) {
  g <- switch(v$layer, bg10 = bg10, bg20 = bg20, tr = tr_layers)
  message(sprintf("  %-32s %d of %d units with a value", v$lab, sum(!is.na(g[[v$col]])), nrow(g)))
}

# simplify for a light map
tr_metro  <- st_simplify(tr_metro,  dTolerance = 30, preserveTopology = TRUE)
tr_layers <- st_simplify(tr_layers, dTolerance = 30, preserveTopology = TRUE)
bg10      <- st_simplify(select(bg10, -any_of(c("ALAND", "acs_pop"))), dTolerance = 30, preserveTopology = TRUE)
bg20      <- st_simplify(bg20, dTolerance = 30, preserveTopology = TRUE)
metro     <- st_simplify(metro, dTolerance = 50, preserveTopology = TRUE)

saveRDS(list(metro = metro, tracts = tr_metro, min_cell = MIN_CELL,
             layers = list(tr = tr_layers, bg10 = bg10, bg20 = bg20), area_vars = area_vars,
             built = format(Sys.time(), "%Y-%m-%d")), dst("geo.rds"))

# ---- 4. A manifest, so the privacy claim is checkable -----------------------
files <- list.files(file.path(app_dir, "data"), recursive = TRUE)
writeLines(c("# Files the app reads. None contains a respondent row.",
             paste0("- ", files)), file.path(app_dir, "data", "MANIFEST.md"))
message("Done. ", length(files), " files in app/data; run with: shiny::runApp('app')")
