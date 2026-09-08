# =============================================================================
# 20_new_variables.R
# The variables added for the re-categorised models (Sep 2026).
#
#   INPUT : respondents_with_transport_ht_segpoi_stoz_diversity.csv  (script 05)
#           ndvi_by_respondent.csv                                   (script 21,
#                                                                     optional)
#   OUTPUT: respondents_with_new_variables.csv   <-- the new analysis file
#           new_variable_coverage.csv            how many respondents each
#                                                new measure is observed for
#
# Four of the five new measures need no new download:
#
#   env_pm25           fine particle pollution, ug/m3  ) already joined by
#   env_ozone          ozone, ppb                      ) script 02 from
#   env_npl_proximity  proximity to National Priorities) Colorado EnviroScreen
#                      List (Superfund) sites          ) v2, block group
#
# They are renamed here to short, readable names; the EnviroScreen columns
# themselves are left untouched.
#
# Two are new ACS pulls at block group, 2015-2019 five-year estimates, the same
# vintage and geography as the socioeconomic measures script 01 downloads:
#
#   pct_nh_white     B03002: non-Hispanic white as a percent of total population
#   pct_multifamily  B25024: housing units in structures of two or more units,
#                    as a percent of all units
#
# NDVI is the one measure that needs imagery; 21_ndvi.R computes it and writes
# ndvi_by_respondent.csv. This script joins that file if it is present and
# carries on without it if it is not, so the rest of the pipeline can be re-run
# before the imagery step finishes.
#
# Run this AFTER 05 and BEFORE 08.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
wb_require(c("tidyverse", "janitor", "readr", "tidycensus"))

src_file <- wb_path("s05_final")
if (!file.exists(src_file)) {
  stop("20_new_variables.R: ", src_file, " not found. Run scripts 01-05 first.")
}

dat <- readr::read_csv(src_file, show_col_types = FALSE) %>% janitor::clean_names()
cat("20_new_variables.R: read", nrow(dat), "respondents from", basename(src_file), "\n")

if (!"bg_geoid" %in% names(dat)) {
  stop("20_new_variables.R: bg_geoid is missing from the analysis file. ",
       "Re-run 01_geocode_acs.R.")
}
dat$bg_geoid <- clean_geoid(dat$bg_geoid, 12)

# -----------------------------------------------------------------------------
# 1. EnviroScreen measures already in the file, renamed
# -----------------------------------------------------------------------------
# Script 02 joins the whole EnviroScreen table with an `env_` prefix and
# janitor-cleaned names, so "Fine Particle Pollution (ug/m3)" arrives as
# env_fine_particle_pollution_mg_m3 (the "mg" is janitor transliterating the
# micro sign, not a unit error -- the values are ug/m3).

ENV_RENAMES <- c(
  env_pm25          = "env_fine_particle_pollution_mg_m3",
  env_ozone         = "env_ozone_ppb",
  env_npl_proximity = "env_proximity_to_national_priorities_list_sites"
)

for (new_name in names(ENV_RENAMES)) {
  src <- ENV_RENAMES[[new_name]]
  if (!src %in% names(dat)) {
    stop("20_new_variables.R: expected EnviroScreen column '", src, "' is not ",
         "in the analysis file. Columns starting env_ are:\n  ",
         paste(grep("^env_", names(dat), value = TRUE), collapse = ", "))
  }
  dat[[new_name]] <- to_num(dat[[src]])
}
cat("EnviroScreen measures surfaced: ", paste(names(ENV_RENAMES), collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# 2. Two new ACS block-group measures
# -----------------------------------------------------------------------------
# B03002 is Hispanic or Latino Origin by Race, which is the table that separates
# non-Hispanic white from white alone. B25024 is Units in Structure. Both are
# published at block group.
#
# "Multifamily" here means a structure with two or more units (B25024_004 to
# _009). Single-family detached and attached, mobile homes and boats/RVs are the
# remainder. If the co-authors prefer the 5+ definition, drop _004 and _005 from
# MULTIFAMILY_PARTS below and re-run; nothing else changes.

acs_new_vars <- c(
  nhw_total     = "B03002_001",
  nhw_nh_white  = "B03002_003",
  units_total   = "B25024_001",
  units_2       = "B25024_004",
  units_3_4     = "B25024_005",
  units_5_9     = "B25024_006",
  units_10_19   = "B25024_007",
  units_20_49   = "B25024_008",
  units_50plus  = "B25024_009"
)
MULTIFAMILY_PARTS <- c("units_2E", "units_3_4E", "units_5_9E",
                       "units_10_19E", "units_20_49E", "units_50plusE")

if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
  stop("20_new_variables.R: no Census API key found. Run once, ever:\n",
       "  tidycensus::census_api_key(\"YOUR_KEY\", install = TRUE, overwrite = TRUE)\n",
       "then restart R. Script 01 used the same key.")
}

if (!exists("metro_counties")) metro_counties <- WB_METRO_COUNTIES

get_bg <- function(county_name) {
  message("Downloading ACS block groups (B03002, B25024) for: ", county_name)
  tidycensus::get_acs(geography = "block group", state = "CO", county = county_name,
                      year = 2019, survey = "acs5", variables = acs_new_vars,
                      geometry = FALSE, output = "wide")
}

bg_new <- purrr::map_dfr(metro_counties, get_bg) %>%
  dplyr::distinct(GEOID, .keep_all = TRUE) %>%
  dplyr::mutate(
    bg_geoid = clean_geoid(GEOID, 12),
    pct_nh_white = dplyr::if_else(!is.na(nhw_totalE) & nhw_totalE > 0,
                                  100 * nhw_nh_whiteE / nhw_totalE, NA_real_),
    multifamily_units = rowSums(dplyr::across(dplyr::all_of(MULTIFAMILY_PARTS)), na.rm = TRUE),
    pct_multifamily = dplyr::if_else(!is.na(units_totalE) & units_totalE > 0,
                                     100 * multifamily_units / units_totalE, NA_real_)
  ) %>%
  dplyr::select(bg_geoid, pct_nh_white, pct_multifamily)

cat("ACS block groups downloaded:", nrow(bg_new), "\n")

dat <- dat %>%
  dplyr::select(-dplyr::any_of(c("pct_nh_white", "pct_multifamily"))) %>%
  dplyr::left_join(bg_new, by = "bg_geoid")

# -----------------------------------------------------------------------------
# 3. NDVI, if 21_ndvi.R has been run
# -----------------------------------------------------------------------------
ndvi_file <- file.path(out_dir, "ndvi_by_respondent.csv")
if (file.exists(ndvi_file)) {
  ndvi <- readr::read_csv(ndvi_file, show_col_types = FALSE) %>% janitor::clean_names()
  key <- intersect(c("respondent_row_id", "row_id", "id"), names(ndvi))[1]
  if (is.na(key) || !key %in% names(dat)) {
    stop("20_new_variables.R: ", basename(ndvi_file), " has no join key in common ",
         "with the analysis file. Expected respondent_row_id.")
  }
  keep <- c(key, grep("^ndvi_", names(ndvi), value = TRUE))
  dat <- dat %>%
    dplyr::select(-dplyr::any_of(setdiff(keep, key))) %>%
    dplyr::left_join(ndvi[keep], by = key)
  cat("NDVI joined:", paste(setdiff(keep, key), collapse = ", "), "\n")
} else {
  cat("\nNOTE: ", basename(ndvi_file), " not found, so NDVI is not in this file.\n",
      "      Run 21_ndvi.R and then re-run this script to add it. The models\n",
      "      will run without it; the green space domain will simply have one\n",
      "      fewer measure.\n", sep = "")
}

# -----------------------------------------------------------------------------
# 4. Coverage report and write
# -----------------------------------------------------------------------------
NEW_VARS <- c("pct_nh_white", "pct_multifamily", "env_pm25", "env_ozone",
              "env_npl_proximity", grep("^ndvi_", names(dat), value = TRUE))
NEW_VARS <- NEW_VARS[NEW_VARS %in% names(dat)]

coverage <- dplyr::tibble(
  variable = NEW_VARS,
  n_observed = vapply(NEW_VARS, function(v) sum(!is.na(to_num(dat[[v]]))), integer(1)),
  n_total = nrow(dat),
  mean = vapply(NEW_VARS, function(v) mean(to_num(dat[[v]]), na.rm = TRUE), numeric(1)),
  sd = vapply(NEW_VARS, function(v) stats::sd(to_num(dat[[v]]), na.rm = TRUE), numeric(1)),
  min = vapply(NEW_VARS, function(v) suppressWarnings(min(to_num(dat[[v]]), na.rm = TRUE)), numeric(1)),
  max = vapply(NEW_VARS, function(v) suppressWarnings(max(to_num(dat[[v]]), na.rm = TRUE)), numeric(1))
)
readr::write_csv(coverage, file.path(out_dir, "new_variable_coverage.csv"))

cat("\nCoverage of the new variables:\n")
print(as.data.frame(coverage), row.names = FALSE, digits = 4)

out_file <- wb_path("s06_newvars")
readr::write_csv(dat, out_file)
cat("\nWrote", out_file, "with", nrow(dat), "rows and", ncol(dat), "columns.\n")
cat("00_config.R points analysis_file at this file whenever it exists, so\n")
cat("scripts 06, 07, 08 and 13 will pick it up on their next run.\n")
