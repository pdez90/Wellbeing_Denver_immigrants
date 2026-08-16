# =============================================================================
# 01_geocode_acs.R
# Geocode respondents, attach census tracts/block groups, merge 2015-2019 ACS
#
#   INPUT : swb_data.csv
#   OUTPUT: respondents_with_acs.csv
#
# Split from Wellbeing.Rmd (lines 852-1336). Body is verbatim from the original
# chunk except where noted; run 00_config.R first or source() it below.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")

# ============================================================
# DENVER RESPONDENT GEOCODING + ACS MERGE
# Working block-group ACS variables only, 2015-2019 ACS
# ============================================================
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(janitor)
options(tigris_use_cache = TRUE)
# out_dir, target_crs, map_dir, table_dir come from 00_config.R -- do not hardcode.
# ============================================================
# READ SURVEY FILE
# ============================================================
infile <- file.path(out_dir, "swb_data.csv")
raw <- readr::read_csv(
  infile,
  show_col_types = FALSE
)
dat <- raw %>%
  janitor::clean_names() %>%
  dplyr::filter(
    !is.na(response_id),
    response_id != "Response ID",
    response_id != "response_id"
  ) %>%
  dplyr::rename(
    latitude = lat,
    longitude = long
  ) %>%
  dplyr::mutate(
    respondent_row_id = dplyr::row_number(),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude))
  )
print(summary(dat$latitude))
print(summary(dat$longitude))
dat <- dat %>%
  dplyr::mutate(
    valid_coords =
      !is.na(latitude) &
      !is.na(longitude) &
      latitude > 35 & latitude < 42 &
      longitude > -110 & longitude < -100
  )
cat("Total survey rows:", nrow(dat), "\n")
cat("Rows with valid coordinates:", sum(dat$valid_coords), "\n")
# ============================================================
# CREATE RESPONDENT POINTS
# ============================================================
pts <- dat %>%
  dplyr::filter(valid_coords) %>%
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
cat("Respondents with coordinates:", nrow(pts), "\n")
# ============================================================
# DENVER METRO COUNTIES
# ============================================================
metro_counties <- c(
  "Denver",
  "Adams",
  "Arapahoe",
  "Jefferson",
  "Douglas",
  "Broomfield"
)
denver_counties <- tigris::counties(
  state = "CO",
  cb = TRUE,
  year = 2019
) %>%
  sf::st_transform(sf::st_crs(pts)) %>%
  dplyr::filter(NAME %in% metro_counties)
# ============================================================
# PLOT RESPONDENTS
# ============================================================
p <- ggplot() +
  geom_sf(
    data = denver_counties,
    fill = "grey95",
    color = "grey50"
  ) +
  geom_sf(
    data = pts,
    alpha = 0.6,
    size = 1
  ) +
  theme_minimal() +
  labs(
    title = "Survey Respondent Locations",
    subtitle = "Denver Metropolitan Area"
  )
print(p)
ggsave(
  file.path(out_dir, "respondent_map.png"),
  p,
  width = 8,
  height = 6,
  dpi = 300
)
# ============================================================
# CENSUS TRACTS
# ============================================================
tracts_co <- tigris::tracts(
  state = "CO",
  cb = TRUE,
  year = 2019
) %>%
  sf::st_transform(sf::st_crs(pts))
pts_tract <- sf::st_join(
  pts,
  tracts_co %>%
    dplyr::select(tract_geoid = GEOID),
  join = sf::st_intersects,
  left = TRUE
)
dat$tract_geoid <- NA_character_
dat$tract_geoid[
  match(
    pts_tract$respondent_row_id,
    dat$respondent_row_id
  )
] <- pts_tract$tract_geoid
cat("Matched tracts:", sum(!is.na(dat$tract_geoid)), "\n")
# ============================================================
# BLOCK GROUPS
# ============================================================
bg_co <- tigris::block_groups(
  state = "CO",
  cb = TRUE,
  year = 2019
) %>%
  sf::st_transform(sf::st_crs(pts))
pts_bg <- sf::st_join(
  pts,
  bg_co %>%
    dplyr::select(bg_geoid = GEOID),
  join = sf::st_intersects,
  left = TRUE
)
dat$bg_geoid <- NA_character_
dat$bg_geoid[
  match(
    pts_bg$respondent_row_id,
    dat$respondent_row_id
  )
] <- pts_bg$bg_geoid
cat("Matched block groups:", sum(!is.na(dat$bg_geoid)), "\n")
# ============================================================
# DISTANCE TO DOWNTOWN DENVER
# ============================================================
downtown <- sf::st_sfc(
  sf::st_point(c(-104.9903, 39.7392)),
  crs = 4326
)
pts_proj <- pts %>%
  sf::st_transform(26913)
downtown_proj <- downtown %>%
  sf::st_transform(26913)
pts_dist <- pts_proj %>%
  dplyr::mutate(
    dist_downtown_km =
      as.numeric(
        sf::st_distance(
          geometry,
          downtown_proj
        )
      ) / 1000
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    respondent_row_id,
    dist_downtown_km
  )
dat <- dat %>%
  dplyr::left_join(
    pts_dist,
    by = "respondent_row_id"
  )
# ============================================================
# ACS API
# ============================================================
# Run once if needed:
# tidycensus::census_api_key("YOUR_KEY", install = TRUE, overwrite = TRUE)
# readRenviron("~/.Renviron")
acs_vars <- c(
  population = "B01003_001",
  housing_units = "B25001_001",
  median_income = "B19013_001",
  median_rent = "B25064_001",
  median_home_value = "B25077_001",
  # --- Poverty (C17002 IS published at block group; B17001 is NOT) ---
  # Ratio of income to poverty level. Below poverty = ratio < 1.00,
  # i.e. categories _002 (<0.50) + _003 (0.50-0.99).
  pov_total     = "C17002_001",
  pov_under_050 = "C17002_002",
  pov_050_099   = "C17002_003"
)
# NOTE: Foreign-born / nativity (B05002, B05012) is NOT tabulated at the
# block-group level by the Census Bureau. The finest available geography is
# the census tract, so pct_non_native is computed at tract level below and
# attached to respondents via tract_geoid.
# ============================================================
# DOWNLOAD ACS BLOCK GROUP DATA BY COUNTY
# ============================================================
get_bg_acs_county <- function(county_name) {
  message("Downloading ACS block groups for: ", county_name)
  tidycensus::get_acs(
    geography = "block group",
    state = "CO",
    county = county_name,
    year = 2019,
    survey = "acs5",
    variables = acs_vars,
    geometry = FALSE,
    output = "wide"
  )
}
bg_acs <- purrr::map_dfr(
  metro_counties,
  get_bg_acs_county
)
cat("\nACS columns downloaded:\n")
print(names(bg_acs))
cat("\nSample ACS rows:\n")
print(head(bg_acs))
# ============================================================
# CLEAN ACS ESTIMATES
# ============================================================
bg_acs_wide <- bg_acs %>%
  dplyr::transmute(
    GEOID,
    NAME,
    population = populationE,
    housing_units = housing_unitsE,
    median_income = median_incomeE,
    median_rent = median_rentE,
    median_home_value = median_home_valueE,
    pov_total = pov_totalE,
    pov_under_050 = pov_under_050E,
    pov_050_099 = pov_050_099E
  ) %>%
  dplyr::distinct(GEOID, .keep_all = TRUE)
# ============================================================
# CALCULATE BLOCK GROUP AREA
# ============================================================
bg_area <- bg_co %>%
  sf::st_transform(26913) %>%
  dplyr::mutate(
    area_sqkm = as.numeric(sf::st_area(.)) / 1e6
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(GEOID, area_sqkm)
# ============================================================
# CREATE NEIGHBORHOOD VARIABLES
# ============================================================
bg_acs_final <- bg_acs_wide %>%
  dplyr::left_join(
    bg_area,
    by = "GEOID"
  ) %>%
  dplyr::mutate(
    pop_density =
      population / area_sqkm,
    housing_density =
      housing_units / area_sqkm,
    # Percent below poverty level (income-to-poverty ratio < 1.00), percent.
    # Guard against divide-by-zero in unpopulated block groups.
    pct_poverty = dplyr::if_else(
      !is.na(pov_total) & pov_total > 0,
      100 * (pov_under_050 + pov_050_099) / pov_total,
      NA_real_
    )
  )
cat("\nBlock-group ACS diagnostics before respondent merge:\n")
cat("Rows:", nrow(bg_acs_final), "\n")
cat("Non-missing population:", sum(!is.na(bg_acs_final$population)), "\n")
cat("Non-missing housing_units:", sum(!is.na(bg_acs_final$housing_units)), "\n")
cat("Non-missing median_income:", sum(!is.na(bg_acs_final$median_income)), "\n")
cat("Non-missing median_rent:", sum(!is.na(bg_acs_final$median_rent)), "\n")
cat("Non-missing median_home_value:", sum(!is.na(bg_acs_final$median_home_value)), "\n")
cat("Non-missing pct_poverty:", sum(!is.na(bg_acs_final$pct_poverty)), "\n")
# ============================================================
# TRACT-LEVEL NATIVITY (foreign-born NOT available at block group)
# Pulled at census tract and attached via tract_geoid.
# B05012: _001 = total, _003 = foreign born.
# ============================================================
nativity_vars <- c(
  nativity_total = "B05012_001",
  foreign_born = "B05012_003"
)
get_tract_nativity_county <- function(county_name) {
  message("Downloading tract nativity for: ", county_name)
  tidycensus::get_acs(
    geography = "tract",
    state = "CO",
    county = county_name,
    year = 2019,
    survey = "acs5",
    variables = nativity_vars,
    geometry = FALSE,
    output = "wide"
  )
}
tract_nativity <- purrr::map_dfr(
  metro_counties,
  get_tract_nativity_county
) %>%
  dplyr::transmute(
    tract_geoid = GEOID,
    nativity_total = nativity_totalE,
    foreign_born = foreign_bornE,
    # Share of non-native (foreign-born) population, percent (tract level).
    pct_non_native = dplyr::if_else(
      !is.na(nativity_total) & nativity_total > 0,
      100 * foreign_born / nativity_total,
      NA_real_
    )
  ) %>%
  dplyr::distinct(tract_geoid, .keep_all = TRUE)
cat("\nTract nativity diagnostics:\n")
cat("Rows:", nrow(tract_nativity), "\n")
cat("Non-missing pct_non_native:", sum(!is.na(tract_nativity$pct_non_native)), "\n")
# ============================================================
# MERGE TO RESPONDENTS
# ============================================================
dat <- dat %>%
  dplyr::select(
    -dplyr::any_of(c(
      "population",
      "housing_units",
      "median_income",
      "median_rent",
      "median_home_value",
      "area_sqkm",
      "pop_density",
      "housing_density",
      "pov_total",
      "pov_under_050",
      "pov_050_099",
      "pct_poverty",
      "nativity_total",
      "foreign_born",
      "pct_non_native"
    ))
  ) %>%
  # Block-group ACS (includes pct_poverty)
  dplyr::left_join(
    bg_acs_final,
    by = c("bg_geoid" = "GEOID")
  ) %>%
  # Tract-level nativity (pct_non_native)
  dplyr::left_join(
    tract_nativity,
    by = "tract_geoid"
  )
# ============================================================
# CREATE STANDARDIZED NEIGHBORHOOD INDICES
# ============================================================
zscore <- function(x) {
  as.numeric(scale(x))
}
dat <- dat %>%
  dplyr::mutate(
    pop_density_z = zscore(pop_density),
    housing_density_z = zscore(housing_density),
    dist_downtown_z = zscore(dist_downtown_km),
    median_income_z = zscore(median_income),
    median_rent_z = zscore(median_rent),
    median_home_value_z = zscore(median_home_value),
    pct_non_native_z = zscore(pct_non_native),
    pct_poverty_z = zscore(pct_poverty),
    urbanity_index = rowMeans(
      cbind(
        pop_density_z,
        housing_density_z,
        -dist_downtown_z
      ),
      na.rm = TRUE
    ),
    neighborhood_ses_index = rowMeans(
      cbind(
        median_income_z,
        median_rent_z,
        median_home_value_z
      ),
      na.rm = TRUE
    )
  )
# ============================================================
# SAVE OUTPUTS
# ============================================================
readr::write_csv(
  dat,
  file.path(out_dir, "respondents_with_acs.csv")
)
readr::write_csv(
  dat %>%
    dplyr::count(
      tract_geoid,
      name = "n_respondents"
    ) %>%
    dplyr::arrange(desc(n_respondents)),
  file.path(out_dir, "respondents_by_tract.csv")
)
readr::write_csv(
  dat %>%
    dplyr::count(
      bg_geoid,
      name = "n_respondents"
    ) %>%
    dplyr::arrange(desc(n_respondents)),
  file.path(out_dir, "respondents_by_blockgroup.csv")
)
# ============================================================
# RESPONDENT DIAGNOSTICS
# ============================================================
cat("\nFinished.\n")
cat("Output:", file.path(out_dir, "respondents_with_acs.csv"), "\n")
cat("Map:", file.path(out_dir, "respondent_map.png"), "\n\n")
cat("Rows:", nrow(dat), "\n")
cat("Valid coordinates:", sum(dat$valid_coords, na.rm = TRUE), "\n")
cat("Matched tracts:", sum(!is.na(dat$tract_geoid)), "\n")
cat("Matched block groups:", sum(!is.na(dat$bg_geoid)), "\n")
cat("\nDistance to downtown Denver, km:\n")
print(summary(dat$dist_downtown_km))
cat("\nPopulation density:\n")
print(summary(dat$pop_density))
cat("\nHousing density:\n")
print(summary(dat$housing_density))
cat("\nMedian household income:\n")
print(summary(dat$median_income))
cat("\nMedian rent:\n")
print(summary(dat$median_rent))
cat("\nMedian home value:\n")
print(summary(dat$median_home_value))
cat("\nShare non-native population (pct):\n")
print(summary(dat$pct_non_native))
cat("\nPercent below poverty (pct):\n")
print(summary(dat$pct_poverty))
cat("\nUrbanity index:\n")
print(summary(dat$urbanity_index))
cat("\nNeighborhood SES index:\n")
print(summary(dat$neighborhood_ses_index))
cat("\nMissingness among geocoded respondents:\n")
dat %>%
  dplyr::filter(valid_coords) %>%
  dplyr::summarise(
    n_geocoded = dplyr::n(),
    missing_pop_density = sum(is.na(pop_density)),
    missing_housing_density = sum(is.na(housing_density)),
    missing_dist_downtown = sum(is.na(dist_downtown_km)),
    missing_median_income = sum(is.na(median_income)),
    missing_median_rent = sum(is.na(median_rent)),
    missing_median_home_value = sum(is.na(median_home_value)),
    missing_pct_non_native = sum(is.na(pct_non_native)),
    missing_pct_poverty = sum(is.na(pct_poverty)),
    missing_urbanity_index = sum(is.na(urbanity_index)),
    missing_neighborhood_ses_index = sum(is.na(neighborhood_ses_index))
  ) %>%
  print()
cat("\nCorrelation among neighborhood variables:\n")
dat %>%
  dplyr::select(
    pop_density,
    housing_density,
    dist_downtown_km,
    median_income,
    median_rent,
    median_home_value,
    pct_non_native,
    pct_poverty,
    urbanity_index,
    neighborhood_ses_index
  ) %>%
  cor(
    use = "pairwise.complete.obs"
  ) %>%
  round(2) %>%
  print()
