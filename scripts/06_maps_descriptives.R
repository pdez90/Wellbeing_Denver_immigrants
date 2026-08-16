# =============================================================================
# 06_maps_descriptives.R
# Maps of respondents + well-being, block group/tract summaries, descriptive statistics
#
#   INPUT : respondents_with_transport_ht_segpoi_stoz_diversity.csv
#   OUTPUT: maps_outputs/
#
# Split from Wellbeing.Rmd (lines 3413-4099). Body is verbatim from the original
# chunk except where noted; run 00_config.R first or source() it below.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")

# ============================================================
# MAP RESPONDENTS + WELL-BEING + DESCRIPTIVE STATISTICS
# ============================================================

library(tidyverse)
library(sf)
library(tigris)
library(janitor)
library(readr)

options(tigris_use_cache = TRUE)

# out_dir, target_crs, map_dir, table_dir come from 00_config.R -- do not hardcode.
dir.create(map_dir, showWarnings = FALSE)

# ============================================================
# 1. READ FINAL DATA
# ============================================================

dat_path <- file.path(
  out_dir,
  "respondents_with_transport_ht_segpoi_stoz_diversity.csv"
)

dat <- readr::read_csv(dat_path, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  mutate(
    respondent_row_id = as.integer(respondent_row_id),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    bg_geoid = as.character(bg_geoid),
    tract_geoid = as.character(tract_geoid)
  )

# ============================================================
# 2. CREATE SWB AND BELONGING VARIABLES
# ============================================================

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
zscore <- function(x) as.numeric(scale(x))

swb_items <- c(
  "q8_1_1", "q8_1_2", "q8_1_3",
  "q8_1_4", "q8_1_5", "q8_1_6"
)

belonging_items <- c(
  "q9_7_1", "q9_7_2", "q9_7_3", "q9_7_4"
)

dat <- dat %>%
  mutate(
    across(any_of(c(swb_items, belonging_items)), to_num),
    swb_index = rowMeans(across(any_of(swb_items)), na.rm = TRUE),
    belonging_index = rowMeans(across(any_of(belonging_items)), na.rm = TRUE),
    swb_z = zscore(swb_index),
    belonging_z = zscore(belonging_index)
  )

# ============================================================
# 3. CREATE RESPONDENT POINTS
# ============================================================

pts <- dat %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    latitude > 35,
    latitude < 42,
    longitude > -110,
    longitude < -100
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# ============================================================
# 4. READ TRACTS / BLOCK GROUPS / COUNTIES
# ============================================================

metro_counties <- c(
  "Denver", "Adams", "Arapahoe",
  "Jefferson", "Douglas", "Broomfield"
)

counties <- tigris::counties(
  state = "CO",
  cb = TRUE,
  year = 2019
) %>%
  filter(NAME %in% metro_counties) %>%
  st_transform(st_crs(pts))

tracts <- tigris::tracts(
  state = "CO",
  cb = TRUE,
  year = 2019
) %>%
  st_transform(st_crs(pts)) %>%
  semi_join(
    dat %>% distinct(tract_geoid),
    by = c("GEOID" = "tract_geoid")
  )

bgs <- tigris::block_groups(
  state = "CO",
  cb = TRUE,
  year = 2019
) %>%
  st_transform(st_crs(pts)) %>%
  semi_join(
    dat %>% distinct(bg_geoid),
    by = c("GEOID" = "bg_geoid")
  )

# ============================================================
# 5. MAP RESPONDENT LOCATIONS
# ============================================================

p_resp <- ggplot() +
  geom_sf(data = counties, fill = "grey95", color = "grey60") +
  geom_sf(data = pts, alpha = 0.65, size = 1.2) +
  theme_minimal() +
  labs(
    title = "Survey respondent locations",
    subtitle = "Denver metropolitan area"
  )

ggsave(
  file.path(map_dir, "map_respondent_locations.png"),
  p_resp,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 6. MAP SWB AND BELONGING AS POINTS
# ============================================================

p_swb <- ggplot() +
  geom_sf(data = counties, fill = "grey95", color = "grey60") +
  geom_sf(data = pts, aes(color = swb_z), size = 1.5, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Subjective well-being among respondents",
    subtitle = "Standardized SWB index",
    color = "SWB z-score"
  )

ggsave(
  file.path(map_dir, "map_swb_z_points.png"),
  p_swb,
  width = 8,
  height = 6,
  dpi = 300
)

p_belong <- ggplot() +
  geom_sf(data = counties, fill = "grey95", color = "grey60") +
  geom_sf(data = pts, aes(color = belonging_z), size = 1.5, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Neighborhood belonging among respondents",
    subtitle = "Standardized belonging index",
    color = "Belonging z-score"
  )

ggsave(
  file.path(map_dir, "map_belonging_z_points.png"),
  p_belong,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 7. BLOCK-GROUP AND TRACT MEAN MAPS
# ============================================================

bg_summary <- dat %>%
  group_by(bg_geoid) %>%
  summarise(
    n_respondents = n(),
    mean_swb_z = mean(swb_z, na.rm = TRUE),
    mean_belonging_z = mean(belonging_z, na.rm = TRUE),
    .groups = "drop"
  )

tract_summary <- dat %>%
  group_by(tract_geoid) %>%
  summarise(
    n_respondents = n(),
    mean_swb_z = mean(swb_z, na.rm = TRUE),
    mean_belonging_z = mean(belonging_z, na.rm = TRUE),
    .groups = "drop"
  )

bgs_map <- bgs %>%
  left_join(bg_summary, by = c("GEOID" = "bg_geoid"))

tracts_map <- tracts %>%
  left_join(tract_summary, by = c("GEOID" = "tract_geoid"))

p_bg_swb <- ggplot() +
  geom_sf(data = counties, fill = "grey95", color = "grey70") +
  geom_sf(data = bgs_map, aes(fill = mean_swb_z), color = NA) +
  theme_minimal() +
  labs(
    title = "Mean SWB by block group",
    fill = "Mean SWB z"
  )

ggsave(
  file.path(map_dir, "map_bg_mean_swb_z.png"),
  p_bg_swb,
  width = 8,
  height = 6,
  dpi = 300
)

p_tract_swb <- ggplot() +
  geom_sf(data = counties, fill = "grey95", color = "grey70") +
  geom_sf(data = tracts_map, aes(fill = mean_swb_z), color = "white", linewidth = 0.15) +
  theme_minimal() +
  labs(
    title = "Mean SWB by census tract",
    fill = "Mean SWB z"
  )

ggsave(
  file.path(map_dir, "map_tract_mean_swb_z.png"),
  p_tract_swb,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 8. DESCRIPTIVE STATISTICS
# ============================================================

summary_vars <- c(
  "swb_index", "swb_z",
  "belonging_index", "belonging_z",
  "pop_density", "housing_density",
  "dist_downtown_km",
  "median_income", "pct_poverty",
  "walk_nat_walk_ind", "sld_nat_walk_ind",
  "tree_treecanopy", "tree_tes",
  "park_nearest_dist_m", "park_acres_half_mile",
  "street_intdensity",
  "sidewalk_density_800",
  "bike_facility_density_800",
  "active_corridor_density_800",
  "crash_density_800",
  "ped_crash_density_800",
  "bike_crash_density_800",
  "ht_ht_ami", "ht_t_ami",
  "segpoi_all_segregation",
  "short_trip_zone_share_800",
  "div_total_diversity_resi",
  "div_exposure_mean"
)

summary_vars <- summary_vars[summary_vars %in% names(dat)]

descriptive_stats <- dat %>%
  summarise(
    across(
      all_of(summary_vars),
      list(
        n = ~ sum(!is.na(.x)),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        p75 = ~ quantile(.x, 0.75, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_sep = "__"
  )

readr::write_csv(
  descriptive_stats,
  file.path(map_dir, "descriptive_statistics.csv")
)

# ============================================================
# 9. RESPONDENT / GEOGRAPHY SUMMARY
# ============================================================

respondent_summary <- tibble(
  statistic = c(
    "Total respondents",
    "Respondents with valid coordinates",
    "Unique block groups",
    "Unique census tracts",
    "Mean respondents per block group",
    "Median respondents per block group",
    "Mean respondents per tract",
    "Median respondents per tract"
  ),
  value = c(
    nrow(dat),
    nrow(pts),
    n_distinct(dat$bg_geoid[!is.na(dat$bg_geoid)]),
    n_distinct(dat$tract_geoid[!is.na(dat$tract_geoid)]),
    mean(bg_summary$n_respondents, na.rm = TRUE),
    median(bg_summary$n_respondents, na.rm = TRUE),
    mean(tract_summary$n_respondents, na.rm = TRUE),
    median(tract_summary$n_respondents, na.rm = TRUE)
  )
)

readr::write_csv(
  respondent_summary,
  file.path(map_dir, "respondent_geography_summary.csv")
)

readr::write_csv(
  bg_summary,
  file.path(map_dir, "blockgroup_swb_summary.csv")
)

readr::write_csv(
  tract_summary,
  file.path(map_dir, "tract_swb_summary.csv")
)

# ============================================================
# 10. PRINT DIAGNOSTICS
# ============================================================

cat("\nSaved maps and tables to:\n")
cat(map_dir, "\n\n")

cat("Respondent/geography summary:\n")
print(respondent_summary)

cat("\nDescriptive statistics:\n")
print(descriptive_stats)

# ============================================================
# WORD-READY DESCRIPTIVE TABLES
# ============================================================

library(tidyverse)
library(janitor)
library(flextable)
library(officer)

dir.create(table_dir, showWarnings = FALSE)

# ============================================================
# 1. VARIABLE LABELS
# ============================================================

var_labels <- c(
  swb_index = "Subjective well-being index",
  swb_z = "Subjective well-being, standardized",
  belonging_index = "Neighborhood belonging index",
  belonging_z = "Neighborhood belonging, standardized",

  pop_density = "Population density",
  housing_density = "Housing density",
  dist_downtown_km = "Distance to downtown Denver, km",
  median_income = "Median household income",
  pct_poverty = "Percent below poverty",
  pct_non_native = "Percent foreign-born",
  urbanity_index = "Urbanicity index",
  neighborhood_ses_index = "Neighborhood SES index",

  walk_nat_walk_ind = "EPA National Walkability Index",
  sld_nat_walk_ind = "Smart Location Database walkability index",
  street_intdensity = "Street intersection density",
  street_linknoderatio = "Street link-node ratio",

  sidewalk_density_800 = "Sidewalk length density within 800 m",
  bike_facility_density_800 = "Bicycle facility density within 800 m",
  active_corridor_density_800 = "Active transportation corridor density within 800 m",

  park_nearest_dist_m = "Distance to nearest park, meters",
  park_access_800m = "Park within 800 m",
  park_acres_half_mile = "Park acres within 800 m",

  tree_treecanopy = "Tree canopy share",
  tree_tes = "Tree Equity Score",
  lc_800m_tree_canopy = "Tree canopy within 800 m",
  lc_800m_impervious_surfaces = "Impervious surface within 800 m",
  lc_800m_structures = "Structures within 800 m",
  lc_800m_grassland_prairie = "Grassland/prairie within 800 m",
  lc_800m_irrigated_lands_turf = "Irrigated turf within 800 m",

  crash_density_800 = "Crash density within 800 m",
  ped_crash_density_800 = "Pedestrian crash density within 800 m",
  bike_crash_density_800 = "Bicycle crash density within 800 m",

  ht_ht_ami = "Housing + transportation cost index",
  ht_h_ami = "Housing cost index",
  ht_t_ami = "Transportation cost index",
  ht_autos_per_hh_ami = "Autos per household",
  ht_vmt_per_hh_ami = "Vehicle miles traveled per household",
  ht_compact_ndx = "Compact neighborhood index",

  hudjob_jobs_idx = "HUD Jobs Proximity Index",

  # These are visitor-weighted average travel distances from the home block
  # group to visited destinations of each type -- behavioural measures from
  # mobile-device traces, NOT home-to-nearest-facility proximity. Labels name
  # them accordingly so the table cannot be read as an accessibility measure.
  segpoi_all_segregation = "Experienced income segregation across visited destinations",
  segpoi_all_distance = "Mean travel distance to visited destinations (all types)",
  segpoi_grocery_distance = "Mean travel distance to visited groceries",
  segpoi_healthcare_distance = "Mean travel distance to visited healthcare",
  segpoi_park_distance = "Mean travel distance to visited parks",
  segpoi_school_distance = "Mean travel distance to visited schools",

  short_trip_zone_share_800 = "Short-trip opportunity zone share within 800 m",

  div_total_diversity_resi = "Residential diversity index",
  div_exposure_mean = "Experienced diversity index",
  div_diff_mean = "Experienced minus residential diversity"
)

# ============================================================
# 2. FUNCTIONS
# ============================================================

make_desc_table <- function(df, vars) {

  vars <- vars[vars %in% names(df)]

  if (length(vars) == 0) {
    return(tibble())
  }

  df %>%
    summarise(
      across(
        all_of(vars),
        list(
          N = ~ sum(!is.na(.x)),
          Mean = ~ mean(.x, na.rm = TRUE),
          SD = ~ sd(.x, na.rm = TRUE),
          Min = ~ min(.x, na.rm = TRUE),
          P25 = ~ quantile(.x, 0.25, na.rm = TRUE),
          Median = ~ median(.x, na.rm = TRUE),
          P75 = ~ quantile(.x, 0.75, na.rm = TRUE),
          Max = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = c("variable", ".value"),
      names_sep = "__"
    ) %>%
    mutate(
      Variable = dplyr::recode(variable, !!!var_labels, .default = variable)
    ) %>%
    select(
      Variable, N, Mean, SD, Min, P25, Median, P75, Max
    ) %>%
    mutate(
      across(
        c(Mean, SD, Min, P25, Median, P75, Max),
        ~ round(.x, 2)
      )
    )
}

make_ft <- function(df) {

  first_col <- names(df)[1]

  flextable(df) %>%
    theme_booktabs() %>%
    autofit() %>%
    fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    align(j = first_col, align = "left", part = "all")
}

save_word_table <- function(df, file, title) {

  doc <- read_docx() %>%
    body_add_par(title, style = "heading 1") %>%
    body_add_flextable(make_ft(df))

  print(doc, target = file)
}

# ============================================================
# 3. RESPONDENT / GEOGRAPHY SUMMARY
# ============================================================

respondent_summary_clean <- tibble(
  Statistic = c(
    "Total respondents",
    "Respondents with valid coordinates",
    "Unique census block groups",
    "Unique census tracts",
    "Mean respondents per block group",
    "Median respondents per block group",
    "Mean respondents per census tract",
    "Median respondents per census tract"
  ),
  Value = c(
    nrow(dat),
    nrow(pts),
    n_distinct(dat$bg_geoid[!is.na(dat$bg_geoid)]),
    n_distinct(dat$tract_geoid[!is.na(dat$tract_geoid)]),
    mean(bg_summary$n_respondents, na.rm = TRUE),
    median(bg_summary$n_respondents, na.rm = TRUE),
    mean(tract_summary$n_respondents, na.rm = TRUE),
    median(tract_summary$n_respondents, na.rm = TRUE)
  )
) %>%
  mutate(Value = round(Value, 2))

write_csv(
  respondent_summary_clean,
  file.path(table_dir, "Table_Respondent_Geography_Summary.csv")
)

save_word_table(
  respondent_summary_clean,
  file.path(table_dir, "Table_Respondent_Geography_Summary.docx"),
  "Respondent and Geography Summary"
)

# ============================================================
# 4. DESCRIPTIVE TABLES BY DOMAIN
# ============================================================

outcome_vars <- c(
  "swb_index",
  "swb_z",
  "belonging_index",
  "belonging_z"
)

neighborhood_vars <- c(
  "pop_density",
  "housing_density",
  "dist_downtown_km",
  "median_income",
  "pct_poverty",
  "pct_non_native",
  "urbanity_index",
  "neighborhood_ses_index"
)

access_walkability_vars <- c(
  "walk_nat_walk_ind",
  "sld_nat_walk_ind",
  "street_intdensity",
  "street_linknoderatio",
  "sidewalk_density_800",
  "bike_facility_density_800",
  "active_corridor_density_800",
  "hudjob_jobs_idx",
  "ht_ht_ami",
  "ht_h_ami",
  "ht_t_ami",
  "ht_autos_per_hh_ami",
  "ht_vmt_per_hh_ami",
  "ht_compact_ndx"
)

green_park_vars <- c(
  "tree_treecanopy",
  "tree_tes",
  "park_nearest_dist_m",
  "park_access_800m",
  "park_acres_half_mile",
  "lc_800m_tree_canopy",
  "lc_800m_impervious_surfaces",
  "lc_800m_structures",
  "lc_800m_grassland_prairie",
  "lc_800m_irrigated_lands_turf"
)

safety_social_vars <- c(
  "crash_density_800",
  "ped_crash_density_800",
  "bike_crash_density_800",
  "segpoi_all_segregation",
  "segpoi_all_distance",
  "segpoi_grocery_distance",
  "segpoi_healthcare_distance",
  "segpoi_park_distance",
  "segpoi_school_distance",
  "short_trip_zone_share_800",
  "div_total_diversity_resi",
  "div_exposure_mean",
  "div_diff_mean"
)

table_outcomes <- make_desc_table(dat, outcome_vars)
table_neighborhood <- make_desc_table(dat, neighborhood_vars)
table_access <- make_desc_table(dat, access_walkability_vars)
table_green <- make_desc_table(dat, green_park_vars)
table_safety_social <- make_desc_table(dat, safety_social_vars)

# Save CSVs
write_csv(table_outcomes, file.path(table_dir, "Table_Outcomes.csv"))
write_csv(table_neighborhood, file.path(table_dir, "Table_Neighborhood_Context.csv"))
write_csv(table_access, file.path(table_dir, "Table_Access_Walkability_Transport.csv"))
write_csv(table_green, file.path(table_dir, "Table_Greenness_Parks_Landcover.csv"))
write_csv(table_safety_social, file.path(table_dir, "Table_Safety_Social_Exposure.csv"))

# Save individual Word tables
save_word_table(
  table_outcomes,
  file.path(table_dir, "Table_Outcomes.docx"),
  "Outcome Variables"
)

save_word_table(
  table_neighborhood,
  file.path(table_dir, "Table_Neighborhood_Context.docx"),
  "Neighborhood Context Variables"
)

save_word_table(
  table_access,
  file.path(table_dir, "Table_Access_Walkability_Transport.docx"),
  "Accessibility, Walkability, and Transportation Variables"
)

save_word_table(
  table_green,
  file.path(table_dir, "Table_Greenness_Parks_Landcover.docx"),
  "Greenness, Parks, and Land-Cover Variables"
)

save_word_table(
  table_safety_social,
  file.path(table_dir, "Table_Safety_Social_Exposure.docx"),
  "Safety and Social Exposure Variables"
)

# ============================================================
# 5. COMBINED WORD DOCUMENT
# ============================================================

combined_doc <- read_docx() %>%
  body_add_par("Descriptive Statistics", style = "heading 1") %>%

  body_add_par("Respondent and Geography Summary", style = "heading 2") %>%
  body_add_flextable(make_ft(respondent_summary_clean)) %>%

  body_add_par("Outcome Variables", style = "heading 2") %>%
  body_add_flextable(make_ft(table_outcomes)) %>%

  body_add_par("Neighborhood Context Variables", style = "heading 2") %>%
  body_add_flextable(make_ft(table_neighborhood)) %>%

  body_add_par("Accessibility, Walkability, and Transportation Variables", style = "heading 2") %>%
  body_add_flextable(make_ft(table_access)) %>%

  body_add_par("Greenness, Parks, and Land-Cover Variables", style = "heading 2") %>%
  body_add_flextable(make_ft(table_green)) %>%

  body_add_par("Safety and Social Exposure Variables", style = "heading 2") %>%
  body_add_flextable(make_ft(table_safety_social))

print(
  combined_doc,
  target = file.path(table_dir, "All_Word_Ready_Descriptive_Tables.docx")
)

cat("\nSaved Word-ready tables to:\n")
cat(table_dir, "\n")
