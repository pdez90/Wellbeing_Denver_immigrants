# =============================================================================
# wb_domains_v3.R
# Variable registry for the restructured (v3) analysis: two parallel OLS
# outcomes, four NBE domains, no regulatory domain, no mediation.
#
# This file is the single place the v3 specification lives. Scripts 30-33 read
# it; nothing in them names a predictor directly. Decided from the coauthors'
# 26 October 2026 checklist; items the checklist leaves open are marked OPEN and
# are estimated as variants in 31_parallel_models.R so the choice can be made
# with the numbers in view.
# =============================================================================

# ---- Outcomes ---------------------------------------------------------------
# NSC = neighborhood social connectedness, the four-item composite formerly
# called neighborhood belonging. The single belonging item (q9_7_1) is kept as
# a sensitivity outcome.
V3_OUTCOMES <- c(swb = "swb_z", nsc = "nsc_z")
V3_OUTCOME_LABELS <- c(swb = "Subjective well-being", nsc = "Neighborhood social connectedness",
                       belong1 = "Single-item belonging")

# ---- Neighborhood demographic controls --------------------------------------
# Experienced diversity is removed by the checklist. Residential diversity came
# from the same source and is removed with it (OPEN: variant "resdiv" keeps it).
V3_CONTROL_VARS <- c(
  "pct_poverty",             # ACS 2015-2019, block group
  "pct_non_native",          # ACS 2015-2019, tract
  "neighborhood_ses_index",  # composite of income, rent, home value
  "pct_nh_white"             # ACS 2015-2019, block group
)

# ---- NBE domains ------------------------------------------------------------
V3_DOMAIN_VARS <- list(
  density_location = c("pop_density", "pct_multifamily", "dist_downtown_km",
                       "walk_nat_walk_ind"),
  transport_infra  = c("sidewalk_density_800", "bike_facility_density_800",
                       "rail_station_800"),          # NEW: 30_rail_stations.R
  green_space      = c("park_acres_half_mile", "park_nearest_dist_m",
                       "lc_800m_tree_canopy"),
  safety_env       = c("crash_density_800", "lc_800m_impervious_surfaces",
                       "env_pm25", "env_npl_proximity")
)

V3_DOMAIN_LABELS <- c(
  density_location = "Density, location and urban form",
  transport_infra  = "Transportation infrastructure",
  green_space      = "Green space",
  safety_env       = "Safety and environmental quality"
)

# Binary predictors are entered as 0/1, not standardized.
V3_BINARY <- c("rail_station_800", "rail_station_400", "rail_station_1600")

# OPEN items, each estimated as a variant of the main specification:
#   urban_center : adds urban_center_nearest_dist_m to the density domain
#                  (the strongest NSC correlate in v2; not on the retain list)
#   resdiv       : adds div_total_diversity_resi to the controls
#   rail_dist    : replaces the 0/1 station indicator by distance to the
#                  nearest station (km)
#   belong1      : single-item belonging as the NSC outcome
V3_VARIANTS <- c("urban_center", "resdiv", "rail_dist", "belong1")

# Buffer-derived measures that exist at 400 m and 1,600 m as well as 800 m.
V3_BUFFER_VARS <- c(
  sidewalk_density_800        = "sidewalk_density_%d",
  bike_facility_density_800   = "bike_facility_density_%d",
  rail_station_800            = "rail_station_%d",
  lc_800m_tree_canopy         = "lc_%dm_tree_canopy",
  crash_density_800           = "crash_density_%d",
  lc_800m_impervious_surfaces = "lc_%dm_impervious_surfaces"
)

V3_LABELS <- c(
  pop_density                 = "Population density",
  pct_multifamily             = "Multifamily housing share",
  dist_downtown_km            = "Distance to downtown Denver",
  walk_nat_walk_ind           = "EPA National Walkability Index",
  urban_center_nearest_dist_m = "Distance to nearest urban center",
  sidewalk_density_800        = "Sidewalk density",
  bike_facility_density_800   = "Bicycle facility density",
  rail_station_800            = "Rail station within 800 m",
  rail_station_nearest_km     = "Distance to nearest rail station",
  park_acres_half_mile        = "Park acreage within 800 m",
  park_nearest_dist_m         = "Distance to nearest park",
  lc_800m_tree_canopy         = "Tree canopy land cover",
  crash_density_800           = "Crash density",
  lc_800m_impervious_surfaces = "Impervious surface",
  env_pm25                    = "Fine particle pollution (PM2.5)",
  env_npl_proximity           = "Superfund (NPL) site proximity",
  pct_poverty                 = "Percent below poverty",
  pct_non_native              = "Percent foreign-born",
  neighborhood_ses_index      = "Neighborhood SES index",
  pct_nh_white                = "Percent non-Hispanic white",
  div_total_diversity_resi    = "Residential racial-ethnic diversity",
  age = "Age", children = "Number of children", income_hh = "Household income",
  edu_level = "Education", engl_speak = "English proficiency",
  time_live_denver = "Years in Denver metro", time_live_hood = "Years in neighborhood",
  ind_female = "Female", ind_married = "Married",
  ind_prev_married = "Previously married", ind_hispanic = "Hispanic",
  ind_race_white = "Race: White", ind_lpr = "Lawful permanent resident",
  ind_undocumented = "No official status (undocumented)",
  ind_imm_other = "DACA, visa, refugee, or asylee",
  nsc_z = "Neighborhood social connectedness"
)
v3_label <- function(term) {
  base <- sub("_z$", "", term)
  out <- unname(V3_LABELS[base])
  ifelse(is.na(out), term, out)
}
