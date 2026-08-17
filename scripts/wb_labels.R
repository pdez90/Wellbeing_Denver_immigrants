# =============================================================================
# wb_labels.R
# One place where every model term gets its printed label. Sourced by
# 11_manuscript_tables.R, 13_descriptives_all.R and 14_figure_coefficients.R so
# a variable is never called two different things in two different tables.
# =============================================================================

WB_LABELS <- c(
  age = "Age", children = "Number of children", income_hh = "Household income",
  edu_level = "Education", engl_speak = "English proficiency",
  time_live_denver = "Years in Denver metro", time_live_hood = "Years in neighborhood",
  ind_female = "Female", ind_married = "Married", ind_prev_married = "Previously married",
  ind_hispanic = "Hispanic", ind_race_white = "Race: White",
  ind_lpr = "Lawful permanent resident", ind_undocumented = "No official status",
  ind_imm_other = "DACA/visa/refugee/asylee",
  pop_density_z = "Population density", housing_density_z = "Housing density",
  dist_downtown_km_z = "Distance to downtown", pct_poverty_z = "Percent below poverty",
  pct_non_native_z = "Percent foreign-born", neighborhood_ses_index_z = "Neighborhood SES index",
  walk_nat_walk_ind_z = "EPA Walkability Index", street_intdensity_z = "Street intersection density",
  urban_center_nearest_dist_m_z = "Distance to urban center", hudjob_jobs_idx_z = "HUD Jobs Proximity Index",
  sidewalk_density_800_z = "Sidewalk density", bike_facility_density_800_z = "Bicycle facility density",
  active_corridor_density_800_z = "Active corridor density", ht_t_ami_z = "Transportation cost",
  tree_tes_z = "Tree Equity Score", tree_treecanopy_z = "Tree canopy (Tree Equity)",
  park_acres_half_mile_z = "Park acreage within 800 m", park_nearest_dist_m_z = "Distance to nearest park",
  lc_800m_tree_canopy_z = "Tree canopy land cover (800 m)", lc_800m_impervious_surfaces_z = "Impervious surface (800 m)",
  crash_density_800_z = "Crash density", ped_crash_density_800_z = "Pedestrian crash density",
  bike_crash_density_800_z = "Bicycle crash density", short_trip_zone_share_800_z = "Short-trip opportunity zone share",
  div_total_diversity_resi_z = "Residential diversity", div_exposure_mean_z = "Experienced diversity",
  belonging_z = "Neighborhood belonging", zone_adu_yes = "ADU permitted",
  pfa_share_800_z = "Pedestrian focus area share",
  zone_categoryRes_medhigh = "Zoning: residential medium-high",
  zone_categoryMixedUse = "Zoning: mixed use", zone_categoryNonres = "Zoning: nonresidential"
)
