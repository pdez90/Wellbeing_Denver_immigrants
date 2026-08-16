# =============================================================================
# 07_tables_descriptive.R
# Word-ready Table 1 (sample characteristics) and Table 2 (neighborhood/built environment)
#
#   INPUT : respondents_with_transport_ht_segpoi_stoz_diversity.csv
#   OUTPUT: word_ready_model_tables/
#
# Split from Wellbeing.Rmd (lines 4102-4367). Body is verbatim from the original
# chunk except where noted; run 00_config.R first or source() it below.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")

# ============================================================
# WORD-READY TABLE 1 AND TABLE 2
# Table 1: Sample + outcomes + individual characteristics
# Table 2: Neighborhood / built environment descriptive stats
# ============================================================

library(tidyverse)
library(janitor)
library(flextable)
library(officer)
library(readr)

# out_dir, target_crs, map_dir, table_dir come from 00_config.R -- do not hardcode.
table_dir <- model_tab_dir
dir.create(table_dir, showWarnings = FALSE)

# ----------------------------
# Read final model data
# ----------------------------

dat <- readr::read_csv(
  file.path(out_dir, "respondents_with_transport_ht_segpoi_stoz_diversity.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names()

# ----------------------------
# Helper functions
# ----------------------------

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

zscore <- function(x) as.numeric(scale(x))

make_desc_table <- function(df, vars, labels) {
  
  vars <- vars[vars %in% names(df)]
  
  df %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars),
        list(
          N = ~ sum(!is.na(.x)),
          Mean = ~ mean(.x, na.rm = TRUE),
          SD = ~ sd(.x, na.rm = TRUE),
          Min = ~ min(.x, na.rm = TRUE),
          Median = ~ median(.x, na.rm = TRUE),
          Max = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("variable", ".value"),
      names_sep = "__"
    ) %>%
    dplyr::mutate(
      Variable = dplyr::recode(variable, !!!labels, .default = variable)
    ) %>%
    dplyr::select(Variable, N, Mean, SD, Min, Median, Max) %>%
    dplyr::mutate(
      dplyr::across(
        c(Mean, SD, Min, Median, Max),
        ~ round(.x, 2)
      )
    )
}

save_docx_table <- function(tbl, title, filename) {
  
  ft <- flextable(tbl) %>%
    theme_booktabs() %>%
    autofit() %>%
    fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    align(j = "Variable", align = "left", part = "all")
  
  doc <- read_docx() %>%
    body_add_par(title, style = "heading 1") %>%
    body_add_flextable(ft)
  
  print(doc, target = file.path(table_dir, filename))
}

# ============================================================
# Create SWB and belonging indices if needed
# ============================================================

rename_if_present <- function(df, old, new) {
  if (old %in% names(df) && !(new %in% names(df))) {
    df <- df %>% dplyr::rename("{new}" := dplyr::all_of(old))
  }
  df
}

rename_map <- c(
  year_born = "q3_1",
  gender = "q3_2",
  marital = "x2",
  children = "q3_4",
  hispanic = "q3_5",
  race = "q3_6",
  imm_status = "q3_7",
  income_hh = "q3_10",
  edu_level = "q3_14",
  engl_speak = "q3_11_1",
  time_live_denver = "q5_9",
  time_live_hood = "q5_10",
  sat_stand_liv = "q8_1_1",
  sat_health = "q8_1_2",
  sat_life_achieve = "q8_1_3",
  feel_safe = "q8_1_4",
  feel_accepted = "q8_1_5",
  feel_conf_finance = "q8_1_6",
  hood_belong = "q9_7_1",
  hood_trust_people = "q9_7_2",
  hood_borrow = "q9_7_3",
  hood_contacts = "q9_7_4"
)

for (new_name in names(rename_map)) {
  dat <- rename_if_present(dat, rename_map[[new_name]], new_name)
}

swb_items <- c(
  "sat_stand_liv", "sat_health", "sat_life_achieve",
  "feel_safe", "feel_accepted", "feel_conf_finance"
)

belonging_items <- c(
  "hood_belong", "hood_trust_people", "hood_borrow", "hood_contacts"
)

dat <- dat %>%
  mutate(
    across(any_of(c(swb_items, belonging_items)), to_num),
    swb_index = rowMeans(across(any_of(swb_items)), na.rm = TRUE),
    belonging_index = rowMeans(across(any_of(belonging_items)), na.rm = TRUE),
    swb_z = zscore(swb_index),
    belonging_z = zscore(belonging_index),
    age = if ("year_born" %in% names(.)) 2019 - to_num(year_born) else NA_real_
  )

# ============================================================
# TABLE 1: Sample, outcomes, and individual characteristics
# ============================================================

table1_vars <- c(
  "swb_index",
  "belonging_index",
  "age",
  "children",
  "income_hh",
  "edu_level",
  "engl_speak",
  "time_live_denver",
  "time_live_hood"
)

table1_labels <- c(
  swb_index = "Subjective well-being index",
  belonging_index = "Neighborhood belonging index",
  age = "Age",
  children = "Number of children",
  income_hh = "Household income",
  edu_level = "Educational attainment",
  engl_speak = "English-speaking proficiency",
  time_live_denver = "Years lived in Denver metro area",
  time_live_hood = "Years lived in current neighborhood"
)

table1 <- make_desc_table(dat, table1_vars, table1_labels)

write_csv(
  table1,
  file.path(table_dir, "Table1_Sample_Characteristics.csv")
)

save_docx_table(
  table1,
  "Table 1. Sample Characteristics and Outcome Variables",
  "Table1_Sample_Characteristics.docx"
)

# ============================================================
# TABLE 2: Neighborhood and built environment characteristics
# ============================================================

table2_vars <- c(
  "pop_density",
  "housing_density",
  "dist_downtown_km",
  "pct_poverty",
  "pct_non_native",
  "neighborhood_ses_index",
  "walk_nat_walk_ind",
  "street_intdensity",
  "urban_center_nearest_dist_m",
  "hudjob_jobs_idx",
  "sidewalk_density_800",
  "bike_facility_density_800",
  "active_corridor_density_800",
  "ht_t_ami",
  "tree_tes",
  "tree_treecanopy",
  "park_acres_half_mile",
  "park_nearest_dist_m",
  "lc_800m_tree_canopy",
  "lc_800m_impervious_surfaces",
  "crash_density_800",
  "ped_crash_density_800",
  "bike_crash_density_800",
  "short_trip_zone_share_800",
  "div_total_diversity_resi",
  "div_exposure_mean"
)

table2_labels <- c(
  pop_density = "Population density",
  housing_density = "Housing density",
  dist_downtown_km = "Distance to downtown Denver, km",
  pct_poverty = "Percent below poverty",
  pct_non_native = "Percent foreign-born",
  neighborhood_ses_index = "Neighborhood SES index",
  walk_nat_walk_ind = "EPA National Walkability Index",
  street_intdensity = "Street intersection density",
  urban_center_nearest_dist_m = "Distance to nearest urban center, m",
  hudjob_jobs_idx = "HUD Jobs Proximity Index",
  sidewalk_density_800 = "Sidewalk density within 800 m",
  bike_facility_density_800 = "Bicycle facility density within 800 m",
  active_corridor_density_800 = "Active transportation corridor density within 800 m",
  ht_t_ami = "Transportation cost index",
  tree_tes = "Tree Equity Score",
  tree_treecanopy = "Tree canopy share",
  park_acres_half_mile = "Park acres within 800 m",
  park_nearest_dist_m = "Distance to nearest park, m",
  lc_800m_tree_canopy = "Tree canopy land cover within 800 m",
  lc_800m_impervious_surfaces = "Impervious surface within 800 m",
  crash_density_800 = "Crash density within 800 m",
  ped_crash_density_800 = "Pedestrian crash density within 800 m",
  bike_crash_density_800 = "Bicycle crash density within 800 m",
  short_trip_zone_share_800 = "Short-trip opportunity zone share within 800 m",
  div_total_diversity_resi = "Residential diversity index",
  div_exposure_mean = "Experienced diversity index"
)

table2 <- make_desc_table(dat, table2_vars, table2_labels)

write_csv(
  table2,
  file.path(table_dir, "Table2_Neighborhood_Built_Environment.csv")
)

save_docx_table(
  table2,
  "Table 2. Neighborhood and Built Environment Characteristics",
  "Table2_Neighborhood_Built_Environment.docx"
)

cat("\nSaved Word-ready tables to:\n")
cat(table_dir, "\n")
