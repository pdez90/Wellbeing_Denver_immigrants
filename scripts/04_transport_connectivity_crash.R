# =============================================================================
# 04_transport_connectivity_crash.R
# Add street connectivity, sidewalk/bike/active-corridor densities, crash densities, urban centers
#
#   INPUT : respondents_with_all_built_environment_tree_parks.csv
#   OUTPUT: respondents_with_full_transport_connectivity_crash_urban_density.csv
#
# Split from Wellbeing.Rmd (lines 2389-2958). Body is verbatim from the original
# chunk except where noted; run 00_config.R first or source() it below.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")

# ============================================================
# ADD STREET CONNECTIVITY + TRANSPORTATION / SAFETY MEASURES
# WITH DENSITIES
# ============================================================

library(tidyverse)
library(sf)
library(janitor)
library(readr)
library(haven)

# out_dir, target_crs, map_dir, table_dir come from 00_config.R -- do not hardcode.

# ----------------------------
# Helpers
# ----------------------------

clean_geoid <- function(x, width = 12) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\.0$", "")
  x <- stringr::str_replace_all(x, "[^0-9]", "")
  stringr::str_pad(x, width = width, side = "left", pad = "0")
}

buffer_area_km2 <- function(buffer_m) {
  pi * buffer_m^2 / 1e6
}

make_pts <- function(dat) {
  dat %>%
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
}

line_length_in_buffers <- function(lines_sf, pts_proj, buffer_m, prefix) {
  buffers <- pts_proj %>%
    select(respondent_row_id) %>%
    st_buffer(buffer_m)

  inter <- suppressWarnings(
    st_intersection(
      buffers,
      lines_sf %>% select()
    )
  )

  if (nrow(inter) == 0) {
    return(tibble(
      respondent_row_id = pts_proj$respondent_row_id,
      "{prefix}_length_m_{buffer_m}" := 0
    ))
  }

  out <- inter %>%
    mutate(length_m = as.numeric(st_length(.))) %>%
    st_drop_geometry() %>%
    group_by(respondent_row_id) %>%
    summarise(
      "{prefix}_length_m_{buffer_m}" := sum(length_m, na.rm = TRUE),
      .groups = "drop"
    )

  pts_proj %>%
    st_drop_geometry() %>%
    select(respondent_row_id) %>%
    left_join(out, by = "respondent_row_id") %>%
    mutate(across(starts_with(prefix), ~ replace_na(.x, 0)))
}

feature_count_in_buffers <- function(features_sf, pts_proj, buffer_m, prefix) {
  features_sf <- features_sf %>%
    mutate(feature_id_tmp = row_number())

  buffers <- pts_proj %>%
    select(respondent_row_id) %>%
    st_buffer(buffer_m)

  joined <- suppressWarnings(
    st_join(
      buffers,
      features_sf %>% select(feature_id_tmp),
      join = st_intersects,
      left = TRUE
    )
  ) %>%
    st_drop_geometry()

  out <- joined %>%
    filter(!is.na(feature_id_tmp)) %>%
    distinct(respondent_row_id, feature_id_tmp) %>%
    count(respondent_row_id, name = paste0(prefix, "_count_", buffer_m))

  pts_proj %>%
    st_drop_geometry() %>%
    select(respondent_row_id) %>%
    left_join(out, by = "respondent_row_id") %>%
    mutate(across(starts_with(prefix), ~ replace_na(.x, 0)))
}

polygon_area_share_in_buffers <- function(poly_sf, pts_proj, buffer_m, prefix) {
  buffers <- pts_proj %>%
    select(respondent_row_id) %>%
    st_buffer(buffer_m)

  inter <- suppressWarnings(
    st_intersection(
      buffers,
      poly_sf %>% select()
    )
  )

  if (nrow(inter) == 0) {
    return(tibble(
      respondent_row_id = pts_proj$respondent_row_id,
      "{prefix}_area_sqm_{buffer_m}" := 0,
      "{prefix}_share_{buffer_m}" := 0
    ))
  }

  out <- inter %>%
    mutate(area_sqm = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    group_by(respondent_row_id) %>%
    summarise(
      "{prefix}_area_sqm_{buffer_m}" := sum(area_sqm, na.rm = TRUE),
      .groups = "drop"
    )

  pts_proj %>%
    st_drop_geometry() %>%
    select(respondent_row_id) %>%
    left_join(out, by = "respondent_row_id") %>%
    mutate(
      across(starts_with(prefix), ~ replace_na(.x, 0)),
      "{prefix}_share_{buffer_m}" :=
        .data[[paste0(prefix, "_area_sqm_", buffer_m)]] /
        (pi * buffer_m^2)
    )
}

nearest_distance <- function(features_sf, pts_proj, prefix) {
  idx <- st_nearest_feature(pts_proj, features_sf)

  tibble(
    respondent_row_id = pts_proj$respondent_row_id,
    "{prefix}_nearest_dist_m" :=
      as.numeric(st_distance(pts_proj, features_sf[idx, ], by_element = TRUE))
  )
}

# ============================================================
# 1. READ CURRENT DATA
# ============================================================

dat_path <- file.path(
  out_dir,
  "respondents_with_all_built_environment_tree_parks.csv"
)

if (!file.exists(dat_path)) {
  dat_path <- file.path(
    out_dir,
    "respondents_with_acs_enviro_zoning_sld_walkability_jobs_landcover.csv"
  )
}

dat <- read_csv(dat_path, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    respondent_row_id = as.integer(respondent_row_id),
    bg_geoid = clean_geoid(bg_geoid, 12),
    tract_geoid = clean_geoid(tract_geoid, 11),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

pts <- make_pts(dat)
pts_proj <- st_transform(pts, target_crs)

# ============================================================
# 2. STREET CONNECTIVITY: NaNDA 2020 TRACT DATA
# ============================================================

street_rda <- file.path(
  out_dir,
  "ICPSR_38580",
  "DS0003",
  "38580-0003-Data.rda"
)

e <- new.env()
load(street_rda, envir = e)

street_obj <- e[[ls(e)[1]]] %>%
  as_tibble() %>%
  clean_names()

cat("\nStreet connectivity columns:\n")
print(names(street_obj))

tract_col <- names(street_obj)[
  str_detect(names(street_obj), "tract|geoid|fips|gisjoin")
][1]

street_clean <- street_obj %>%
  rename(tract_geoid = all_of(tract_col)) %>%
  mutate(tract_geoid = clean_geoid(tract_geoid, 11)) %>%
  distinct(tract_geoid, .keep_all = TRUE) %>%
  rename_with(~ paste0("street_", .x), -tract_geoid)

dat <- dat %>%
  left_join(street_clean, by = "tract_geoid")

# ============================================================
# 3. ACTIVE TRANSPORTATION CORRIDORS
# ============================================================

active <- st_read(
  file.path(out_dir, "active_transportation_corridors", "active_transportation_corridors.shp"),
  quiet = FALSE
) %>%
  clean_names() %>%
  st_make_valid() %>%
  st_transform(target_crs)

active_dist <- nearest_distance(active, pts_proj, "active_corridor")

active_400 <- line_length_in_buffers(active, pts_proj, 400, "active_corridor")
active_800 <- line_length_in_buffers(active, pts_proj, 800, "active_corridor")
active_1600 <- line_length_in_buffers(active, pts_proj, 1600, "active_corridor")

dat <- dat %>%
  left_join(active_dist, by = "respondent_row_id") %>%
  left_join(active_400, by = "respondent_row_id") %>%
  left_join(active_800, by = "respondent_row_id") %>%
  left_join(active_1600, by = "respondent_row_id")

# ============================================================
# 4. PEDESTRIAN FOCUS AREAS
# ============================================================

pfa <- st_read(
  file.path(out_dir, "pedestrian_focus_areas", "pedestrian_focus_areas.shp"),
  quiet = FALSE
) %>%
  clean_names() %>%
  st_make_valid() %>%
  st_transform(target_crs)

pfa_join <- st_join(
  pts_proj,
  pfa %>% mutate(pfa_present = 1) %>% select(pfa_present),
  join = st_intersects,
  left = TRUE
) %>%
  st_drop_geometry() %>%
  group_by(respondent_row_id) %>%
  summarise(
    in_pedestrian_focus_area = as.integer(any(pfa_present == 1, na.rm = TRUE)),
    .groups = "drop"
  )

pfa_400 <- polygon_area_share_in_buffers(pfa, pts_proj, 400, "pfa")
pfa_800 <- polygon_area_share_in_buffers(pfa, pts_proj, 800, "pfa")
pfa_1600 <- polygon_area_share_in_buffers(pfa, pts_proj, 1600, "pfa")

dat <- dat %>%
  left_join(pfa_join, by = "respondent_row_id") %>%
  left_join(pfa_400, by = "respondent_row_id") %>%
  left_join(pfa_800, by = "respondent_row_id") %>%
  left_join(pfa_1600, by = "respondent_row_id")

# ============================================================
# 5. SIDEWALK LENGTH
# ============================================================

sidewalks <- st_read(
  file.path(
    out_dir,
    "planimetrics_2022_centerline_sidewalks_total",
    "planimetrics_2022_centerline_sidewalks_total.shp"
  ),
  quiet = FALSE
) %>%
  clean_names() %>%
  st_make_valid() %>%
  st_transform(target_crs)

sidewalk_400 <- line_length_in_buffers(sidewalks, pts_proj, 400, "sidewalk")
sidewalk_800 <- line_length_in_buffers(sidewalks, pts_proj, 800, "sidewalk")
sidewalk_1600 <- line_length_in_buffers(sidewalks, pts_proj, 1600, "sidewalk")

dat <- dat %>%
  left_join(sidewalk_400, by = "respondent_row_id") %>%
  left_join(sidewalk_800, by = "respondent_row_id") %>%
  left_join(sidewalk_1600, by = "respondent_row_id")

# ============================================================
# 6. BICYCLE FACILITIES
# ============================================================

bike <- st_read(
  file.path(out_dir, "bicycle_facility_inventory", "bicycle_facility_inventory.shp"),
  quiet = FALSE
) %>%
  clean_names() %>%
  st_make_valid() %>%
  st_transform(target_crs)

bike_dist <- nearest_distance(bike, pts_proj, "bike_facility")

bike_count_400 <- feature_count_in_buffers(bike, pts_proj, 400, "bike_facility")
bike_count_800 <- feature_count_in_buffers(bike, pts_proj, 800, "bike_facility")
bike_count_1600 <- feature_count_in_buffers(bike, pts_proj, 1600, "bike_facility")

bike_len_400 <- line_length_in_buffers(bike, pts_proj, 400, "bike_facility")
bike_len_800 <- line_length_in_buffers(bike, pts_proj, 800, "bike_facility")
bike_len_1600 <- line_length_in_buffers(bike, pts_proj, 1600, "bike_facility")

dat <- dat %>%
  left_join(bike_dist, by = "respondent_row_id") %>%
  left_join(bike_count_400, by = "respondent_row_id") %>%
  left_join(bike_count_800, by = "respondent_row_id") %>%
  left_join(bike_count_1600, by = "respondent_row_id") %>%
  left_join(bike_len_400, by = "respondent_row_id") %>%
  left_join(bike_len_800, by = "respondent_row_id") %>%
  left_join(bike_len_1600, by = "respondent_row_id")

# ============================================================
# 7. CRASH COUNTS IN BUFFERS
# ============================================================

# Crash source. Pinned by name so the same file is used on every machine.
# The original code globbed for /crash/ and took the first hit, which depends
# on filesystem ordering -- crash_2019.csv and crash_2019/drcog_crash_2019.shp
# both match, and they are not interchangeable.
crash_preferred <- c(
  file.path(out_dir, "crash_2019.csv"),
  file.path(out_dir, "crash_2019", "drcog_crash_2019.shp")
)

crash_file <- crash_preferred[file.exists(crash_preferred)][1]

cat("\nUsing crash source:", crash_file, "\n")

if (!is.na(crash_file)) {

  if (str_detect(crash_file, "\\.csv$")) {
    crashes <- read_csv(crash_file, show_col_types = FALSE) %>%
      clean_names() %>%
      filter(!is.na(longitude), !is.na(latitude)) %>%
      st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
      )
  } else {
    crashes <- st_read(crash_file, quiet = FALSE) %>%
      clean_names()
  }

  crashes <- crashes %>%
    st_transform(target_crs) %>%
    mutate(
      crash_total = 1,
      crash_injury = as.integer(coalesce(as.numeric(injured), 0) > 0),
      crash_fatal = as.integer(coalesce(as.numeric(killed), 0) > 0),

      crash_bike = as.integer(
        coalesce(as.numeric(acctype), -999) == 15 |
          coalesce(as.numeric(mhe), -999) == 15 |
          coalesce(as.numeric(vt1), -999) == 13 |
          coalesce(as.numeric(vt2), -999) == 13 |
          coalesce(as.numeric(vt3), -999) == 13
      ),

      crash_ped = as.integer(
        coalesce(as.numeric(acctype), -999) %in% c(4, 5) |
          coalesce(as.numeric(mhe), -999) %in% c(4, 5) |
          coalesce(as.numeric(ped_act1), -999) > 0 |
          coalesce(as.numeric(ped_act2), -999) > 0 |
          coalesce(as.numeric(ped_act3), -999) > 0
      )
    )

  cat("\nCrash duplicate check:\n")
  print(
    crashes %>%
      st_drop_geometry() %>%
      summarise(
        n_rows = n(),
        unique_gid = if ("gid" %in% names(.)) n_distinct(gid) else NA_integer_,
        unique_fid = if ("fid" %in% names(.)) n_distinct(fid) else NA_integer_
      )
  )

  crash_counts <- function(buffer_m) {
    buffers <- pts_proj %>%
      select(respondent_row_id) %>%
      st_buffer(buffer_m)

    joined <- st_join(
      buffers,
      crashes %>%
        select(
          crash_total,
          crash_injury,
          crash_fatal,
          crash_bike,
          crash_ped
        ),
      join = st_intersects,
      left = TRUE
    ) %>%
      st_drop_geometry()

    joined %>%
      group_by(respondent_row_id) %>%
      summarise(
        "crash_total_{buffer_m}" := sum(crash_total, na.rm = TRUE),
        "crash_injury_{buffer_m}" := sum(crash_injury, na.rm = TRUE),
        "crash_fatal_{buffer_m}" := sum(crash_fatal, na.rm = TRUE),
        "crash_bike_{buffer_m}" := sum(crash_bike, na.rm = TRUE),
        "crash_ped_{buffer_m}" := sum(crash_ped, na.rm = TRUE),
        .groups = "drop"
      )
  }

  dat <- dat %>%
    left_join(crash_counts(400), by = "respondent_row_id") %>%
    left_join(crash_counts(800), by = "respondent_row_id") %>%
    left_join(crash_counts(1600), by = "respondent_row_id")
}

# ============================================================
# 8. CREATE DENSITY MEASURES
# ============================================================

dat <- dat %>%
  mutate(
    crash_density_400 = crash_total_400 / buffer_area_km2(400),
    crash_density_800 = crash_total_800 / buffer_area_km2(800),
    crash_density_1600 = crash_total_1600 / buffer_area_km2(1600),

    injury_crash_density_400 = crash_injury_400 / buffer_area_km2(400),
    injury_crash_density_800 = crash_injury_800 / buffer_area_km2(800),
    injury_crash_density_1600 = crash_injury_1600 / buffer_area_km2(1600),

    ped_crash_density_400 = crash_ped_400 / buffer_area_km2(400),
    ped_crash_density_800 = crash_ped_800 / buffer_area_km2(800),
    ped_crash_density_1600 = crash_ped_1600 / buffer_area_km2(1600),

    bike_crash_density_400 = crash_bike_400 / buffer_area_km2(400),
    bike_crash_density_800 = crash_bike_800 / buffer_area_km2(800),
    bike_crash_density_1600 = crash_bike_1600 / buffer_area_km2(1600),

    sidewalk_density_400 = sidewalk_length_m_400 / buffer_area_km2(400),
    sidewalk_density_800 = sidewalk_length_m_800 / buffer_area_km2(800),
    sidewalk_density_1600 = sidewalk_length_m_1600 / buffer_area_km2(1600),

    bike_facility_density_400 = bike_facility_length_m_400 / buffer_area_km2(400),
    bike_facility_density_800 = bike_facility_length_m_800 / buffer_area_km2(800),
    bike_facility_density_1600 = bike_facility_length_m_1600 / buffer_area_km2(1600),

    active_corridor_density_400 = active_corridor_length_m_400 / buffer_area_km2(400),
    active_corridor_density_800 = active_corridor_length_m_800 / buffer_area_km2(800),
    active_corridor_density_1600 = active_corridor_length_m_1600 / buffer_area_km2(1600)
  )

# ============================================================
# 9. URBAN CENTERS
# ============================================================

urban_centers <- st_read(
  file.path(out_dir, "urban_centers_2019", "urban_centers_2019.shp"),
  quiet = FALSE
) %>%
  clean_names() %>%
  st_make_valid() %>%
  st_transform(target_crs)

urban_join <- st_join(
  pts_proj,
  urban_centers %>% mutate(urban_center_present = 1) %>% select(urban_center_present),
  join = st_intersects,
  left = TRUE
) %>%
  st_drop_geometry() %>%
  group_by(respondent_row_id) %>%
  summarise(
    in_urban_center = as.integer(any(urban_center_present == 1, na.rm = TRUE)),
    .groups = "drop"
  )

urban_dist <- nearest_distance(urban_centers, pts_proj, "urban_center")

dat <- dat %>%
  left_join(urban_join, by = "respondent_row_id") %>%
  left_join(urban_dist, by = "respondent_row_id")

# ============================================================
# 10. SAVE
# ============================================================

final_out <- file.path(
  out_dir,
  "respondents_with_full_transport_connectivity_crash_urban_density.csv"
)

write_csv(dat, final_out)

# ============================================================
# 11. DIAGNOSTICS
# ============================================================

cat("\nSaved:\n", final_out, "\n")

cat("\nStreet connectivity columns added:\n")
print(names(dat)[str_detect(names(dat), "^street_")])

cat("\nActive corridor summaries:\n")
print(summary(dat$active_corridor_nearest_dist_m))
print(summary(dat$active_corridor_length_m_800))
print(summary(dat$active_corridor_density_800))

cat("\nPedestrian focus area:\n")
print(table(dat$in_pedestrian_focus_area, useNA = "ifany"))
print(summary(dat$pfa_share_800))

cat("\nSidewalk 800m:\n")
print(summary(dat$sidewalk_length_m_800))
print(summary(dat$sidewalk_density_800))

cat("\nBike facility 800m:\n")
print(summary(dat$bike_facility_count_800))
print(summary(dat$bike_facility_length_m_800))
print(summary(dat$bike_facility_density_800))

if ("crash_total_800" %in% names(dat)) {
  cat("\nCrash counts 800m:\n")
  print(summary(dat$crash_total_800))
  print(summary(dat$crash_injury_800))
  print(summary(dat$crash_ped_800))
  print(summary(dat$crash_bike_800))

  cat("\nCrash densities 800m, per km2:\n")
  print(summary(dat$crash_density_800))
  print(summary(dat$injury_crash_density_800))
  print(summary(dat$ped_crash_density_800))
  print(summary(dat$bike_crash_density_800))
}

cat("\nUrban center:\n")
print(table(dat$in_urban_center, useNA = "ifany"))
print(summary(dat$urban_center_nearest_dist_m))
