# =============================================================================
# 05_ht_poi_stoz_diversity.R
# Add H+T Affordability Index, SafeGraph POI, short-trip opportunity zones, diversity/exposure
#
#   INPUT : respondents_with_full_transport_connectivity_crash_urban_density.csv
#   OUTPUT: respondents_with_transport_ht_segpoi_stoz_diversity.csv  <-- FINAL ANALYSIS FILE
#
# Split from Wellbeing.Rmd (lines 2961-3410). Body is verbatim from the original
# chunk except where noted; run 00_config.R first or source() it below.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")

# ============================================================
# ADD H+T INDEX, SEG POI, SHORT-TRIP OPPORTUNITY ZONES,
# AND DIVERSITY / EXPOSURE DATA
# ============================================================

library(tidyverse)
library(sf)
library(janitor)
library(readr)

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

prefix_except <- function(df, prefix, keep = "geoid") {
  names(df) <- ifelse(
    names(df) %in% keep,
    names(df),
    paste0(prefix, names(df))
  )
  df
}

polygon_area_share_in_buffers <- function(poly_sf, pts_proj, buffer_m, prefix) {
  buffers <- pts_proj %>%
    dplyr::select(respondent_row_id) %>%
    sf::st_buffer(buffer_m)

  inter <- suppressWarnings(
    sf::st_intersection(
      buffers,
      poly_sf %>% dplyr::select()
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
    mutate(area_sqm = as.numeric(sf::st_area(.))) %>%
    sf::st_drop_geometry() %>%
    group_by(respondent_row_id) %>%
    summarise(
      "{prefix}_area_sqm_{buffer_m}" := sum(area_sqm, na.rm = TRUE),
      .groups = "drop"
    )

  pts_proj %>%
    sf::st_drop_geometry() %>%
    dplyr::select(respondent_row_id) %>%
    left_join(out, by = "respondent_row_id") %>%
    mutate(
      across(starts_with(prefix), ~ replace_na(.x, 0)),
      "{prefix}_share_{buffer_m}" :=
        .data[[paste0(prefix, "_area_sqm_", buffer_m)]] /
        (pi * buffer_m^2)
    )
}

# ============================================================
# 1. READ CURRENT RESPONDENT DATA
# ============================================================

dat_path <- file.path(
  out_dir,
  "respondents_with_full_transport_connectivity_crash_urban_density.csv"
)

dat <- readr::read_csv(dat_path, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  mutate(
    respondent_row_id = as.integer(respondent_row_id),
    bg_geoid = clean_geoid(bg_geoid, 12),
    tract_geoid = clean_geoid(tract_geoid, 11),
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

pts_proj <- pts %>%
  st_transform(target_crs)

cat("Rows:", nrow(dat), "\n")
cat("Geocoded respondent points:", nrow(pts), "\n")

# ============================================================
# 2. JOIN H+T INDEX BY BLOCK GROUP
# ============================================================

ht_path <- file.path(
  out_dir,
  "htaindex2019_data_blkgrps_08.csv"
)

ht <- readr::read_csv(
  ht_path,
  show_col_types = FALSE,
  col_types = cols(
    blkgrp = col_character(),
    .default = col_guess()
  )
) %>%
  janitor::clean_names() %>%
  mutate(
    geoid = clean_geoid(blkgrp, 12)
  ) %>%
  distinct(geoid, .keep_all = TRUE) %>%
  prefix_except("ht_", keep = "geoid")

dat <- dat %>%
  select(-any_of(names(ht)[names(ht) != "geoid"])) %>%
  left_join(
    ht,
    by = c("bg_geoid" = "geoid")
  )

cat("\nH+T Index joined. Columns added:", ncol(ht) - 1, "\n")

# ============================================================
# 3. JOIN SEG POI BY BLOCK GROUP
# ============================================================

seg_path <- file.path(
  out_dir,
  "seg_poi.csv"
)

if (file.exists(seg_path)) {

  seg <- readr::read_csv(
    seg_path,
    show_col_types = FALSE,
    col_types = cols(
      cbgid = col_character(),
      .default = col_guess()
    )
  ) %>%
    janitor::clean_names() %>%
    mutate(
      geoid = clean_geoid(cbgid, 12)
    ) %>%
    distinct(geoid, .keep_all = TRUE) %>%
    prefix_except("segpoi_", keep = "geoid")

  dat <- dat %>%
    select(-any_of(names(seg)[names(seg) != "geoid"])) %>%
    left_join(
      seg,
      by = c("bg_geoid" = "geoid")
    )

  cat("\nSEG POI joined. Columns added:", ncol(seg) - 1, "\n")

} else {
  warning("seg_poi.csv not found at: ", seg_path)
}

# ============================================================
# 4. SHORT-TRIP OPPORTUNITY ZONES
#    Binary: respondent inside zone
#    Area share: 400m, 800m, 1600m buffers
# ============================================================

stoz_path <- file.path(
  out_dir,
  "short_trip_opportunity_zones",
  "short_trip_opportunity_zones.shp"
)

stoz <- sf::st_read(
  stoz_path,
  quiet = FALSE
) %>%
  janitor::clean_names() %>%
  sf::st_make_valid() %>%
  sf::st_transform(target_crs)

stoz_join <- sf::st_join(
  pts_proj,
  stoz %>%
    mutate(short_trip_zone_present = 1) %>%
    select(short_trip_zone_present),
  join = sf::st_intersects,
  left = TRUE
) %>%
  sf::st_drop_geometry() %>%
  group_by(respondent_row_id) %>%
  summarise(
    in_short_trip_opportunity_zone =
      as.integer(any(short_trip_zone_present == 1, na.rm = TRUE)),
    .groups = "drop"
  )

stoz_400 <- polygon_area_share_in_buffers(stoz, pts_proj, 400, "short_trip_zone")
stoz_800 <- polygon_area_share_in_buffers(stoz, pts_proj, 800, "short_trip_zone")
stoz_1600 <- polygon_area_share_in_buffers(stoz, pts_proj, 1600, "short_trip_zone")

dat <- dat %>%
  select(
    -any_of(c(
      "in_short_trip_opportunity_zone",
      "short_trip_zone_area_sqm_400",
      "short_trip_zone_share_400",
      "short_trip_zone_area_sqm_800",
      "short_trip_zone_share_800",
      "short_trip_zone_area_sqm_1600",
      "short_trip_zone_share_1600"
    ))
  ) %>%
  left_join(stoz_join, by = "respondent_row_id") %>%
  left_join(stoz_400, by = "respondent_row_id") %>%
  left_join(stoz_800, by = "respondent_row_id") %>%
  left_join(stoz_1600, by = "respondent_row_id")

cat("\nShort-trip opportunity zones joined.\n")

# ============================================================
# 5. JOIN DIVERSITY / EXPOSURE DATA BY TRACT
# ============================================================

div_path <- file.path(
  out_dir,
  "diversity_CO.csv"
)

div_raw <- readr::read_csv(
  div_path,
  show_col_types = FALSE,
  col_types = cols(
    GEOID10 = col_character(),
    .default = col_guess()
  )
) %>%
  janitor::clean_names() %>%
  mutate(
    tract_geoid = clean_geoid(geoid10, 11),
    interval = as.character(interval),
    weekday = as.character(weekday)
  )

# Static tract characteristics: one row per tract
div_static <- div_raw %>%
  group_by(tract_geoid) %>%
  summarise(
    div_total_pop = first(na.omit(total_pop)),
    div_white_perc = first(na.omit(white_perc)),
    div_black_perc = first(na.omit(black_perc)),
    div_indigenous_perc = first(na.omit(indigenous_perc)),
    div_asian_perc = first(na.omit(asian_perc)),
    div_hispanic_perc = first(na.omit(hispanic_perc)),
    div_ba_higher_perc = first(na.omit(ba_higher_perc)),
    div_median_inc = first(na.omit(median_inc)),
    div_total_diversity_resi = first(na.omit(total_diversity_resi)),
    .groups = "drop"
  )

# Dynamic diversity exposure: summarize across interval/day
div_exposure_summary <- div_raw %>%
  group_by(tract_geoid) %>%
  summarise(
    div_exposure_mean = mean(total_diversity_exp, na.rm = TRUE),
    div_exposure_morning_weekday = mean(
      total_diversity_exp[
        interval == "morning" & weekday == "weekday"
      ],
      na.rm = TRUE
    ),
    div_exposure_afternoon_weekday = mean(
      total_diversity_exp[
        interval == "afternoon" & weekday == "weekday"
      ],
      na.rm = TRUE
    ),
    div_exposure_evening_weekday = mean(
      total_diversity_exp[
        interval == "evening" & weekday == "weekday"
      ],
      na.rm = TRUE
    ),
    div_exposure_late_evening_weekday = mean(
      total_diversity_exp[
        interval == "late evening" & weekday == "weekday"
      ],
      na.rm = TRUE
    ),
    div_exposure_late_night_weekday = mean(
      total_diversity_exp[
        interval == "late night" & weekday == "weekday"
      ],
      na.rm = TRUE
    ),
    div_diff_mean = mean(diff, na.rm = TRUE),
    .groups = "drop"
  )

div_clean <- div_static %>%
  left_join(div_exposure_summary, by = "tract_geoid")

dat <- dat %>%
  select(-any_of(names(div_clean)[names(div_clean) != "tract_geoid"])) %>%
  left_join(
    div_clean,
    by = "tract_geoid"
  )

cat("\nDiversity / exposure data joined. Columns added:", ncol(div_clean) - 1, "\n")

# ============================================================
# 6. OPTIONAL: CREATE SIMPLE STANDARDIZED VARIABLES
# ============================================================

zscore <- function(x) as.numeric(scale(x))

vars_to_z <- c(
  "ht_ht_ami",
  "ht_h_ami",
  "ht_t_ami",
  "ht_vmt_per_hh_ami",
  "ht_autos_per_hh_ami",
  "ht_compact_ndx",
  "ht_intersection_density",
  "segpoi_all_segregation",
  "segpoi_all_distance",
  "segpoi_grocery_distance",
  "segpoi_healthcare_distance",
  "segpoi_park_distance",
  "segpoi_school_distance",
  "short_trip_zone_share_800",
  "div_exposure_mean",
  "div_total_diversity_resi",
  "div_diff_mean"
)

vars_to_z <- vars_to_z[vars_to_z %in% names(dat)]

dat <- dat %>%
  mutate(
    across(
      all_of(vars_to_z),
      zscore,
      .names = "{.col}_z"
    )
  )

# ============================================================
# 7. SAVE
# ============================================================

final_out <- file.path(
  out_dir,
  "respondents_with_transport_ht_segpoi_stoz_diversity.csv"
)

readr::write_csv(
  dat,
  final_out
)

# ============================================================
# 8. DIAGNOSTICS
# ============================================================

cat("\nSaved:\n", final_out, "\n")

cat("\nH+T key variables:\n")
ht_key <- c(
  "ht_ht_ami",
  "ht_h_ami",
  "ht_t_ami",
  "ht_autos_per_hh_ami",
  "ht_vmt_per_hh_ami",
  "ht_compact_ndx",
  "ht_intersection_density"
)
print(summary(dat[ht_key[ht_key %in% names(dat)]]))

if ("segpoi_all_segregation" %in% names(dat)) {
  cat("\nSEG POI key variables:\n")
  print(summary(dat$segpoi_all_segregation))
  print(summary(dat$segpoi_all_distance))
  print(summary(dat$segpoi_grocery_distance))
  print(summary(dat$segpoi_park_distance))
}

cat("\nShort-trip opportunity zone:\n")
print(table(dat$in_short_trip_opportunity_zone, useNA = "ifany"))
print(summary(dat$short_trip_zone_share_400))
print(summary(dat$short_trip_zone_share_800))
print(summary(dat$short_trip_zone_share_1600))

cat("\nDiversity / exposure variables:\n")
print(summary(dat$div_total_diversity_resi))
print(summary(dat$div_exposure_mean))
print(summary(dat$div_diff_mean))

cat("\nMissingness among geocoded respondents:\n")

dat %>%
  filter(valid_coords) %>%
  summarise(
    n_geocoded = n(),

    missing_ht = if ("ht_ht_ami" %in% names(dat)) {
      sum(is.na(ht_ht_ami))
    } else NA_integer_,

    missing_segpoi = if ("segpoi_all_segregation" %in% names(dat)) {
      sum(is.na(segpoi_all_segregation))
    } else NA_integer_,

    missing_short_trip_zone =
      sum(is.na(in_short_trip_opportunity_zone)),

    missing_diversity = if ("div_exposure_mean" %in% names(dat)) {
      sum(is.na(div_exposure_mean))
    } else NA_integer_
  ) %>%
  print()
