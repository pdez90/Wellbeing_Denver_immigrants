# =============================================================================
# 03_tree_parks.R
# Add Tree Equity Score and park access measures
#
#   INPUT : respondents_with_acs_enviro_zoning_sld_walkability_landcover.csv
#   OUTPUT: respondents_with_all_built_environment_tree_parks.csv
#
# Split from Wellbeing.Rmd (lines 1965-2386). Body is verbatim from the original
# chunk except where noted; run 00_config.R first or source() it below.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")

# ============================================================
# ADD TREE EQUITY SCORE + PARK ACCESS MEASURES
# ============================================================

library(tidyverse)
library(sf)
library(janitor)
library(readr)

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

# ============================================================
# 1. READ CURRENT JOINED DATA IF NEEDED
# ============================================================

if (!exists("dat")) {

  candidate_files <- c(
    file.path(out_dir, "respondents_with_acs_enviro_zoning_sld_walkability_jobs_landcover.csv"),
    file.path(out_dir, "respondents_with_acs_enviro_zoning_sld_walkability.csv"),
    file.path(out_dir, "respondents_with_acs.csv")
  )

  dat_path <- candidate_files[file.exists(candidate_files)][1]

  if (is.na(dat_path)) {
    stop("Could not find a respondent data file in: ", out_dir)
  }

  cat("Reading respondent data from:\n", dat_path, "\n")

  dat <- readr::read_csv(
    dat_path,
    show_col_types = FALSE
  ) %>%
    janitor::clean_names()
}

dat <- dat %>%
  mutate(
    bg_geoid = clean_geoid(bg_geoid),
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

cat("Rows in dat:", nrow(dat), "\n")
cat("Respondent points:", nrow(pts), "\n")

# ============================================================
# 2. JOIN TREE EQUITY SCORE BY BLOCK GROUP GEOID
# ============================================================

tree_path <- file.path(
  out_dir,
  "co_shp",
  "co_tes.shp"
)

tree <- sf::st_read(
  tree_path,
  quiet = FALSE
) %>%
  janitor::clean_names()

cat("\nTree Equity columns:\n")
print(names(tree))

tree_geoid_col <- find_geoid_col(tree)

tree_clean <- tree %>%
  st_drop_geometry() %>%
  rename(geoid = all_of(tree_geoid_col)) %>%
  mutate(geoid = clean_geoid(geoid)) %>%
  distinct(geoid, .keep_all = TRUE) %>%
  prefix_except("tree_", keep = "geoid")

dat <- dat %>%
  select(
    -any_of(names(tree_clean)[names(tree_clean) != "geoid"])
  ) %>%
  left_join(
    tree_clean,
    by = c("bg_geoid" = "geoid")
  )

cat("\nTree Equity joined. Columns added:", ncol(tree_clean) - 1, "\n")

if ("tree_tes" %in% names(dat)) {
  cat("\nTree Equity Score summary:\n")
  print(summary(dat$tree_tes))
}

if ("tree_treecanopy" %in% names(dat)) {
  cat("\nTree canopy summary:\n")
  print(summary(dat$tree_treecanopy))
}

# ============================================================
# 3. READ PARKS / OPEN SPACE DATA
# ============================================================

parks_path <- file.path(
  out_dir,
  "pros_2024",
  "pros_2024.shp"
)

parks <- sf::st_read(
  parks_path,
  quiet = FALSE
) %>%
  janitor::clean_names() %>%
  st_make_valid()

cat("\nPark columns:\n")
print(names(parks))

# Use UTM Zone 13N for Denver-area distance/area calculations

pts_proj <- pts %>%
  st_transform(target_crs)

parks_proj <- parks %>%
  st_transform(target_crs) %>%
  filter(!st_is_empty(geometry))

# Optional: print park geometry type
cat("\nPark geometry types:\n")
print(table(as.character(st_geometry_type(parks_proj))))

# ============================================================
# 4. NEAREST PARK DISTANCE
# ============================================================

nearest_idx <- st_nearest_feature(
  pts_proj,
  parks_proj
)

nearest_dist_m <- as.numeric(
  st_distance(
    pts_proj,
    parks_proj[nearest_idx, ],
    by_element = TRUE
  )
)

nearest_park <- parks_proj[nearest_idx, ] %>%
  st_drop_geometry()

nearest_park_names <- names(nearest_park)

# Try to identify a useful park name column
park_name_col <- nearest_park_names[
  stringr::str_detect(
    nearest_park_names,
    regex("name|park|site|facility", ignore_case = TRUE)
  )
][1]

nearest_df <- pts_proj %>%
  st_drop_geometry() %>%
  transmute(
    respondent_row_id,
    park_nearest_dist_m = nearest_dist_m,
    park_nearest_dist_mi = nearest_dist_m / 1609.344,
    park_access_400m = as.integer(nearest_dist_m <= 402.336),  # quarter mile
    park_access_800m = as.integer(nearest_dist_m <= 804.672)   # half mile
  )

if (!is.na(park_name_col)) {
  nearest_df$park_nearest_name <- nearest_park[[park_name_col]]
}

dat <- dat %>%
  select(
    -any_of(c(
      "park_nearest_dist_m",
      "park_nearest_dist_mi",
      "park_access_400m",
      "park_access_800m",
      "park_nearest_name"
    ))
  ) %>%
  left_join(
    nearest_df,
    by = "respondent_row_id"
  )

# ============================================================
# 5. PARK ACRES WITHIN 1/4 MILE AND 1/2 MILE
# ============================================================

calc_park_acres <- function(buffer_m, suffix) {

  cat("\nCalculating park acres within", buffer_m, "meters...\n")

  buffers <- pts_proj %>%
    select(respondent_row_id) %>%
    st_buffer(buffer_m)

  # Intersect buffers with parks
  inter <- suppressWarnings(
    st_intersection(
      buffers,
      parks_proj %>%
        select()
    )
  )

  if (nrow(inter) == 0) {
    return(
      tibble(
        respondent_row_id = pts_proj$respondent_row_id,
        !!paste0("park_acres_", suffix) := 0,
        !!paste0("park_area_sqm_", suffix) := 0
      )
    )
  }

  inter_area <- inter %>%
    mutate(
      park_area_sqm = as.numeric(st_area(.)),
      park_acres = park_area_sqm / 4046.8564224
    ) %>%
    st_drop_geometry() %>%
    group_by(respondent_row_id) %>%
    summarise(
      !!paste0("park_area_sqm_", suffix) := sum(park_area_sqm, na.rm = TRUE),
      !!paste0("park_acres_", suffix) := sum(park_acres, na.rm = TRUE),
      .groups = "drop"
    )

  pts_proj %>%
    st_drop_geometry() %>%
    select(respondent_row_id) %>%
    left_join(
      inter_area,
      by = "respondent_row_id"
    ) %>%
    mutate(
      across(
        c(
          paste0("park_area_sqm_", suffix),
          paste0("park_acres_", suffix)
        ),
        ~ replace_na(.x, 0)
      )
    )
}

parks_400 <- calc_park_acres(
  buffer_m = 402.336,
  suffix = "quarter_mile"
)

parks_800 <- calc_park_acres(
  buffer_m = 804.672,
  suffix = "half_mile"
)

dat <- dat %>%
  select(
    -any_of(c(
      "park_area_sqm_quarter_mile",
      "park_acres_quarter_mile",
      "park_area_sqm_half_mile",
      "park_acres_half_mile"
    ))
  ) %>%
  left_join(parks_400, by = "respondent_row_id") %>%
  left_join(parks_800, by = "respondent_row_id")

# ============================================================
# 6. SAVE UPDATED DATA
# ============================================================

final_out <- file.path(
  out_dir,
  "respondents_with_all_built_environment_tree_parks.csv"
)

readr::write_csv(
  dat,
  final_out
)

# ============================================================
# 7. DIAGNOSTICS
# ============================================================

cat("\nFinal rows:", nrow(dat), "\n")

cat("\nTree Equity variables matched:\n")
tree_cols <- names(dat)[stringr::str_detect(names(dat), "^tree_")]
print(length(tree_cols))

cat("\nPark variables added:\n")
park_cols <- names(dat)[stringr::str_detect(names(dat), "^park_")]
print(park_cols)

cat("\nMissingness among geocoded respondents:\n")

dat %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    latitude > 35,
    latitude < 42,
    longitude > -110,
    longitude < -100
  ) %>%
  summarise(
    n_geocoded = n(),

    missing_tree_tes = if ("tree_tes" %in% names(dat)) {
      sum(is.na(tree_tes))
    } else {
      NA_integer_
    },

    missing_tree_canopy = if ("tree_treecanopy" %in% names(dat)) {
      sum(is.na(tree_treecanopy))
    } else {
      NA_integer_
    },

    missing_nearest_park_dist = sum(is.na(park_nearest_dist_m)),
    mean_nearest_park_m = mean(park_nearest_dist_m, na.rm = TRUE),
    median_nearest_park_m = median(park_nearest_dist_m, na.rm = TRUE),

    pct_within_quarter_mile_park = mean(park_access_400m == 1, na.rm = TRUE),
    pct_within_half_mile_park = mean(park_access_800m == 1, na.rm = TRUE),

    mean_park_acres_quarter_mile =
      mean(park_acres_quarter_mile, na.rm = TRUE),

    mean_park_acres_half_mile =
      mean(park_acres_half_mile, na.rm = TRUE)
  ) %>%
  print()

cat("\nTree Equity Score:\n")
if ("tree_tes" %in% names(dat)) {
  print(summary(dat$tree_tes))
}

cat("\nTree canopy:\n")
if ("tree_treecanopy" %in% names(dat)) {
  print(summary(dat$tree_treecanopy))
}

cat("\nNearest park distance, meters:\n")
print(summary(dat$park_nearest_dist_m))

cat("\nPark access within 1/4 mile:\n")
print(table(dat$park_access_400m, useNA = "ifany"))

cat("\nPark access within 1/2 mile:\n")
print(table(dat$park_access_800m, useNA = "ifany"))

cat("\nPark acres within 1/4 mile:\n")
print(summary(dat$park_acres_quarter_mile))

cat("\nPark acres within 1/2 mile:\n")
print(summary(dat$park_acres_half_mile))

cat("\nSaved:\n")
cat(final_out, "\n")
