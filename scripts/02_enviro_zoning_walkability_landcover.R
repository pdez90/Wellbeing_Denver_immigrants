# =============================================================================
# 02_enviro_zoning_walkability_landcover.R
# Join EnviroScreen, ZONING, EPA Smart Location Database, National Walkability Index, HUD Jobs Proximity, DRCOG landcover buffers
#
#   INPUT : respondents_with_acs.csv
#   OUTPUT: respondents_with_acs_enviro_zoning_sld_walkability_landcover.csv
#
# Split from Wellbeing.Rmd (lines 1339-1961). Body is verbatim from the original
# chunk except where noted; run 00_config.R first or source() it below.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")

# ============================================================
# JOIN ENVIROSCREEN, ZONING, EPA SLD, WALKABILITY,
# HUD JOB PROXIMITY, AND CLEAN DRCOG LANDCOVER BUFFERS
# ============================================================

library(tidyverse)
library(sf)
library(janitor)
library(readr)
library(terra)
library(foreign)

# out_dir, target_crs, map_dir, table_dir come from 00_config.R -- do not hardcode.

# ----------------------------
# Helper functions
# ----------------------------

clean_geoid <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\.0$", "")
  x <- stringr::str_replace_all(x, "[^0-9]", "")
  stringr::str_pad(x, width = 12, side = "left", pad = "0")
}

prefix_except <- function(df, prefix, keep = "geoid") {
  names(df) <- ifelse(
    names(df) %in% keep,
    names(df),
    paste0(prefix, names(df))
  )
  df
}

find_geoid_col <- function(df) {
  possible <- c(
    "geoid", "geoid10", "geoid20", "geoid_10", "geoid_20",
    "bg_geoid", "block_group", "blockgroup", "fips", "gisjoin"
  )

  hit <- possible[possible %in% names(df)]

  if (length(hit) == 0) {
    stop("No GEOID-like column found. Check names(df).")
  }

  hit[1]
}

read_first_gdb_layer <- function(gdb_path) {
  layers <- sf::st_layers(gdb_path)$name
  print(layers)

  sf::st_read(
    gdb_path,
    layer = layers[1],
    quiet = FALSE
  )
}

summary_table <- function(df, vars) {
  vars <- vars[vars %in% names(df)]

  df %>%
    summarise(
      across(
        all_of(vars),
        list(
          n_nonmissing = ~ sum(!is.na(.x)),
          n_missing = ~ sum(is.na(.x)),
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
}

# ============================================================
# 1. READ RESPONDENT FILE
# ============================================================

dat <- readr::read_csv(
  file.path(out_dir, "respondents_with_acs.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  mutate(
    bg_geoid = clean_geoid(bg_geoid),
    tract_geoid = as.character(tract_geoid),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude))
  )

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

cat("Respondent rows:", nrow(dat), "\n")
cat("Respondent points:", nrow(pts), "\n")

# ============================================================
# 2. JOIN ENVIROSCREEN CSV BY BLOCK GROUP GEOID
# ============================================================

enviro <- readr::read_csv(
  file.path(out_dir, "Enviroscreen.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names()

enviro_clean <- enviro %>%
  rename(geoid = all_of(find_geoid_col(enviro))) %>%
  mutate(geoid = clean_geoid(geoid)) %>%
  distinct(geoid, .keep_all = TRUE) %>%
  prefix_except("env_", keep = "geoid")

dat <- dat %>%
  left_join(enviro_clean, by = c("bg_geoid" = "geoid"))

cat("EnviroScreen joined. Columns added:", ncol(enviro_clean) - 1, "\n")

# ============================================================
# 3. JOIN ZONING SHAPEFILE BY SPATIAL OVERLAY
# ============================================================

zoning <- sf::st_read(
  file.path(out_dir, "Zoning", "ALL_DenverMSA_10.3.23.shp"),
  quiet = FALSE
) %>%
  janitor::clean_names() %>%
  st_make_valid() %>%
  st_transform(st_crs(pts))

pts_zoning <- st_join(
  pts,
  zoning,
  join = st_intersects,
  left = TRUE
) %>%
  st_drop_geometry()

zoning_join <- pts_zoning %>%
  group_by(respondent_row_id) %>%
  slice(1) %>%
  ungroup()

zoning_cols <- setdiff(names(zoning_join), names(dat))

zoning_join <- zoning_join %>%
  select(respondent_row_id, all_of(zoning_cols)) %>%
  prefix_except("zone_", keep = "respondent_row_id")

dat <- dat %>%
  left_join(zoning_join, by = "respondent_row_id")

cat("Zoning joined. Columns added:", ncol(zoning_join) - 1, "\n")

# ============================================================
# 4. JOIN EPA SMART LOCATION DATABASE BY BLOCK GROUP GEOID
# ============================================================

sld_raw <- read_first_gdb_layer(
  file.path(out_dir, "SmartLocationDatabaseV3", "SmartLocationDatabase.gdb")
) %>%
  st_drop_geometry() %>%
  janitor::clean_names()

sld_clean <- sld_raw %>%
  rename(geoid = all_of(find_geoid_col(sld_raw))) %>%
  mutate(geoid = clean_geoid(geoid)) %>%
  distinct(geoid, .keep_all = TRUE) %>%
  prefix_except("sld_", keep = "geoid")

dat <- dat %>%
  left_join(sld_clean, by = c("bg_geoid" = "geoid"))

cat("SLD joined. Columns added:", ncol(sld_clean) - 1, "\n")

# ============================================================
# 5. JOIN EPA NATIONAL WALKABILITY INDEX BY BLOCK GROUP GEOID
# ============================================================

walk_raw <- read_first_gdb_layer(
  file.path(out_dir, "WalkabilityIndex", "Natl_WI.gdb")
) %>%
  st_drop_geometry() %>%
  janitor::clean_names()

walk_clean <- walk_raw %>%
  rename(geoid = all_of(find_geoid_col(walk_raw))) %>%
  mutate(geoid = clean_geoid(geoid)) %>%
  distinct(geoid, .keep_all = TRUE) %>%
  prefix_except("walk_", keep = "geoid")

dat <- dat %>%
  left_join(walk_clean, by = c("bg_geoid" = "geoid"))

cat("Walkability joined. Columns added:", ncol(walk_clean) - 1, "\n")

# ============================================================
# 6. JOIN HUD JOB PROXIMITY INDEX BY BLOCK GROUP GEOID
# ============================================================

# HUD Jobs Proximity Index. Pinned by name rather than globbed, so a stray
# CSV with "jobs proximity" in its filename cannot silently change the results.
job_preferred <- list.files(
  out_dir,
  pattern = "^Jobs_Proximity_Index.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(job_preferred) == 0) {
  stop("Could not find Jobs_Proximity_Index_*.csv under: ", out_dir,
       "\nSee DATA_MANIFEST.md for the download link.")
}

if (length(job_preferred) > 1) {
  warning("Multiple Jobs Proximity files found; using the first:\n  ",
          paste(job_preferred, collapse = "\n  "))
}

job_path <- job_preferred[1]
cat("\nUsing HUD Jobs Proximity source:", job_path, "\n")

cat("\nUsing HUD Job Proximity file:\n")
cat(job_path, "\n")

jobs <- readr::read_csv(
  job_path,
  show_col_types = FALSE,
  col_types = cols(
    GEOID = col_character(),
    .default = col_guess()
  )
) %>%
  janitor::clean_names()

jobs_clean <- jobs %>%
  rename(geoid = all_of(find_geoid_col(jobs))) %>%
  mutate(geoid = clean_geoid(geoid)) %>%
  distinct(geoid, .keep_all = TRUE) %>%
  prefix_except("hudjob_", keep = "geoid")

dat <- dat %>%
  left_join(jobs_clean, by = c("bg_geoid" = "geoid"))

cat("HUD Job Proximity joined. Columns added:", ncol(jobs_clean) - 1, "\n")

# ============================================================
# 7. DRCOG LANDCOVER SHARES IN 400M, 800M, 1600M BUFFERS
# FINAL CLEAN VERSION
# ============================================================

# Critical: remove all old landcover columns before creating clean ones
dat <- dat %>%
  select(-matches("^lc_"))

landcover_path <- file.path(
  out_dir,
  "DRCOG_2020_Landcover",
  "LULC_2020_DRCOG.tif"
)

landcover_vat_path <- file.path(
  out_dir,
  "DRCOG_2020_Landcover",
  "LULC_2020_DRCOG.tif.vat.dbf"
)

if (!file.exists(landcover_path)) {
  stop("Landcover raster not found at: ", landcover_path)
}

if (!file.exists(landcover_vat_path)) {
  stop("Landcover VAT not found at: ", landcover_vat_path)
}

lc <- terra::rast(landcover_path)

lc_vat <- foreign::read.dbf(landcover_vat_path) %>%
  janitor::clean_names()

lc_lookup <- lc_vat %>%
  transmute(
    lc_value = as.integer(value),
    lc_label = as.character(class),
    lc_group_clean = case_when(
      str_detect(str_to_lower(lc_label), "structure") ~ "structures",
      str_detect(str_to_lower(lc_label), "impervious") ~ "impervious_surfaces",
      str_detect(str_to_lower(lc_label), "tree") ~ "tree_canopy",
      str_detect(str_to_lower(lc_label), "irrigated|turf") ~ "irrigated_lands_turf",
      str_detect(str_to_lower(lc_label), "grass|prairie") ~ "grassland_prairie",
      str_detect(str_to_lower(lc_label), "water") ~ "water",
      str_detect(str_to_lower(lc_label), "barren|rock") ~ "barren_rock",
      str_detect(str_to_lower(lc_label), "shrub|scrub") ~ "scrubland_shrubland",
      str_detect(str_to_lower(lc_label), "crop") ~ "cropland",
      TRUE ~ "other"
    )
  ) %>%
  distinct(lc_value, .keep_all = TRUE)

cat("\nLandcover lookup:\n")
print(lc_lookup)

readr::write_csv(
  lc_lookup,
  file.path(out_dir, "landcover_lookup_grouped.csv")
)

pts_lc <- pts %>%
  st_transform(sf::st_crs(terra::crs(lc)))

extract_lc_one <- function(one_pt, buffer_m) {

  respondent_id <- one_pt$respondent_row_id[1]

  one_buf <- one_pt %>%
    st_buffer(buffer_m) %>%
    select(respondent_row_id)

  vals <- terra::extract(
    lc,
    terra::vect(one_buf),
    ID = FALSE,
    raw = TRUE
  ) %>%
    as_tibble()

  if (nrow(vals) == 0) {
    return(tibble())
  }

  lc_col <- names(vals)[1]

  vals %>%
    rename(lc_value = all_of(lc_col)) %>%
    filter(!is.na(lc_value)) %>%
    mutate(
      lc_value = as.integer(lc_value),
      respondent_row_id = respondent_id
    ) %>%
    count(respondent_row_id, lc_value, name = "n_cells")
}

extract_lc_props_clean <- function(buffer_m) {

  cat("\nExtracting landcover shares for", buffer_m, "m buffers...\n")

  lc_long <- purrr::map_dfr(
    seq_len(nrow(pts_lc)),
    function(i) {
      if (i %% 25 == 0) {
        cat("  processed", i, "of", nrow(pts_lc), "\n")
      }

      extract_lc_one(pts_lc[i, ], buffer_m)
    }
  )

  lc_grouped <- lc_long %>%
    left_join(lc_lookup, by = "lc_value") %>%
    mutate(
      lc_group_clean = if_else(
        is.na(lc_group_clean),
        paste0("class_", lc_value),
        lc_group_clean
      )
    ) %>%
    group_by(respondent_row_id, lc_group_clean) %>%
    summarise(
      n_cells = sum(n_cells, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(respondent_row_id) %>%
    mutate(
      total_cells = sum(n_cells, na.rm = TRUE),
      prop = n_cells / total_cells
    ) %>%
    ungroup()

  readr::write_csv(
    lc_grouped,
    file.path(out_dir, paste0("landcover_", buffer_m, "m_grouped_long.csv"))
  )

  lc_wide <- lc_grouped %>%
    mutate(
      varname = paste0("lc_", buffer_m, "m_", lc_group_clean)
    ) %>%
    select(respondent_row_id, varname, prop) %>%
    pivot_wider(
      names_from = varname,
      values_from = prop,
      values_fill = 0,
      values_fn = sum
    )

  return(lc_wide)
}

lc_400 <- extract_lc_props_clean(400)
lc_800 <- extract_lc_props_clean(800)
lc_1600 <- extract_lc_props_clean(1600)

dat <- dat %>%
  left_join(lc_400, by = "respondent_row_id") %>%
  left_join(lc_800, by = "respondent_row_id") %>%
  left_join(lc_1600, by = "respondent_row_id")

for (buffer_m in c(400, 800, 1600)) {

  lc_buffer_cols <- names(dat)[
    str_detect(names(dat), paste0("^lc_", buffer_m, "m_"))
  ]

  dat <- dat %>%
    mutate(
      "{paste0('lc_', buffer_m, 'm_sum')}" :=
        rowSums(across(all_of(lc_buffer_cols)), na.rm = TRUE)
    )

  cat("\n", buffer_m, "m landcover columns:\n", sep = "")
  print(lc_buffer_cols)

  cat("\n", buffer_m, "m landcover sum:\n", sep = "")
  print(summary(dat[[paste0("lc_", buffer_m, "m_sum")]]))
}

lc_cols <- names(dat)[str_detect(names(dat), "^lc_")]

cat("\nTotal clean landcover columns:\n")
print(length(lc_cols))
print(lc_cols)

# ============================================================
# 8. SAVE FINAL DATA
# ============================================================

final_out <- file.path(
  out_dir,
  "respondents_with_acs_enviro_zoning_sld_walkability_jobs_landcover.csv"
)

readr::write_csv(dat, final_out)

# ============================================================
# 9. DIAGNOSTICS + SUMMARY TABLES
# ============================================================

env_cols <- names(dat)[str_detect(names(dat), "^env_")]
zone_cols <- names(dat)[str_detect(names(dat), "^zone_")]
sld_cols <- names(dat)[str_detect(names(dat), "^sld_")]
walk_cols <- names(dat)[str_detect(names(dat), "^walk_")]
job_cols <- names(dat)[str_detect(names(dat), "^hudjob_")]
lc_cols <- names(dat)[str_detect(names(dat), "^lc_")]

diagnostics <- tibble(
  item = c(
    "rows_total",
    "valid_coordinates",
    "matched_block_groups",
    "env_cols",
    "zoning_cols",
    "sld_cols",
    "walkability_cols",
    "hud_jobs_cols",
    "landcover_cols"
  ),
  value = c(
    nrow(dat),
    sum(dat$valid_coords, na.rm = TRUE),
    sum(!is.na(dat$bg_geoid)),
    length(env_cols),
    length(zone_cols),
    length(sld_cols),
    length(walk_cols),
    length(job_cols),
    length(lc_cols)
  )
)

print(diagnostics)

readr::write_csv(
  diagnostics,
  file.path(out_dir, "join_diagnostics_summary.csv")
)

key_summary_vars <- c(
  "env_enviro_screen_score",
  "env_enviro_screen_percentile_score",
  "walk_nat_walk_ind",
  "sld_nat_walk_ind",
  "hudjob_jobs_idx",
  "lc_400m_structures",
  "lc_400m_impervious_surfaces",
  "lc_400m_tree_canopy",
  "lc_400m_grassland_prairie",
  "lc_400m_irrigated_lands_turf",
  "lc_400m_barren_rock",
  "lc_400m_water",
  "lc_400m_cropland",
  "lc_400m_scrubland_shrubland",
  "lc_800m_structures",
  "lc_800m_impervious_surfaces",
  "lc_800m_tree_canopy",
  "lc_800m_grassland_prairie",
  "lc_800m_irrigated_lands_turf",
  "lc_800m_barren_rock",
  "lc_800m_water",
  "lc_800m_cropland",
  "lc_800m_scrubland_shrubland",
  "lc_1600m_structures",
  "lc_1600m_impervious_surfaces",
  "lc_1600m_tree_canopy",
  "lc_1600m_grassland_prairie",
  "lc_1600m_irrigated_lands_turf",
  "lc_1600m_barren_rock",
  "lc_1600m_water",
  "lc_1600m_cropland",
  "lc_1600m_scrubland_shrubland"
)

key_summary <- summary_table(dat, key_summary_vars)

print(key_summary)

readr::write_csv(
  key_summary,
  file.path(out_dir, "built_environment_key_summary_table.csv")
)

landcover_summary <- summary_table(dat, lc_cols)

readr::write_csv(
  landcover_summary,
  file.path(out_dir, "landcover_summary_table.csv")
)

missingness_summary <- dat %>%
  filter(valid_coords) %>%
  summarise(
    n_geocoded = n(),
    missing_env_score = if ("env_enviro_screen_score" %in% names(dat)) {
      sum(is.na(env_enviro_screen_score))
    } else {
      NA_integer_
    },
    missing_walk_index = if ("walk_nat_walk_ind" %in% names(dat)) {
      sum(is.na(walk_nat_walk_ind))
    } else {
      NA_integer_
    },
    missing_sld_walk_index = if ("sld_nat_walk_ind" %in% names(dat)) {
      sum(is.na(sld_nat_walk_ind))
    } else {
      NA_integer_
    },
    missing_jobs_idx = if ("hudjob_jobs_idx" %in% names(dat)) {
      sum(is.na(hudjob_jobs_idx))
    } else {
      NA_integer_
    },
    missing_landcover_any =
      sum(if_any(starts_with("lc_"), ~ !is.na(.x)) == FALSE)
  )

print(missingness_summary)

readr::write_csv(
  missingness_summary,
  file.path(out_dir, "built_environment_missingness_summary.csv")
)

if ("zone_gen_zone2" %in% names(dat)) {
  zoning_summary <- dat %>%
    count(zone_gen_zone2, name = "n") %>%
    mutate(percent = n / sum(n) * 100) %>%
    arrange(desc(n))

  print(zoning_summary)

  readr::write_csv(
    zoning_summary,
    file.path(out_dir, "zoning_category_summary.csv")
  )
}

cat("\nSaved final data:\n")
cat(final_out, "\n")

cat("\nSaved summary tables:\n")
cat(file.path(out_dir, "join_diagnostics_summary.csv"), "\n")
cat(file.path(out_dir, "built_environment_key_summary_table.csv"), "\n")
cat(file.path(out_dir, "landcover_summary_table.csv"), "\n")
cat(file.path(out_dir, "built_environment_missingness_summary.csv"), "\n")
cat(file.path(out_dir, "zoning_category_summary.csv"), "\n")
