# =============================================================================
# 21_ndvi.R
# NDVI within each respondent's buffers.
#
#   INPUT : respondents_with_transport_ht_segpoi_stoz_diversity.csv (script 05)
#           optionally, a local NDVI raster at out_dir/ndvi_raster.tif
#   OUTPUT: ndvi_by_respondent.csv   ndvi_400, ndvi_800, ndvi_1600 per respondent
#           ndvi_cache/              one .rds per unique location (resumable)
#
# Why this script exists: the co-authors asked for NDVI because "greenness" is
# conventionally measured that way, and the paper currently has only canopy
# share and the Tree Equity Score. NDVI is the one requested measure that is not
# already on disk, so it needs imagery.
#
# TWO WAYS TO RUN IT
#
#   A. Local raster (preferred if you have one). Put any NDVI GeoTIFF covering
#      the metro area at out_dir/ndvi_raster.tif -- 30 m Landsat or 10 m
#      Sentinel-2 growing-season composite is ideal -- and this script extracts
#      buffer means from it. Fast, high resolution, nothing downloaded here.
#
#   B. MODIS via the ORNL DAAC web service (the default when no raster is
#      present). Downloads a MOD13Q1 NDVI subset around each respondent, at
#      250 m and 16-day composites, and averages the growing season. No
#      account, no key, no GDAL remote reads -- just HTTP -- which is why it is
#      the fallback: it will run anywhere. The cost is resolution. 250 m pixels
#      are coarse for an 800 m circle (about 20 pixels), so report NDVI as a
#      neighbourhood-scale measure, not a doorstep one.
#
# The MODIS route makes one request per unique rounded location and caches each
# result, so it can be interrupted and re-run without repeating work. Expect
# roughly 5 to 20 seconds per location on a first run.
#
# Run this BEFORE 20_new_variables.R (which joins the output), or run 20 twice.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
wb_require(c("tidyverse", "janitor", "readr", "sf", "terra"))

# CRS_M is only used by the local-raster path; the MODIS path needs no
# projected CRS of its own.

RADII   <- c(400, 800, 1600)
GS_FROM <- "2019-05-01"   # growing season; MOD13Q1 composites are 16-day
GS_TO   <- "2019-09-30"

src_file <- wb_path("s05_final")
dat <- readr::read_csv(src_file, show_col_types = FALSE) %>% janitor::clean_names()

lat_col <- intersect(c("latitude", "location_latitude"), names(dat))[1]
lon_col <- intersect(c("longitude", "location_longitude"), names(dat))[1]
if (is.na(lat_col) || is.na(lon_col)) {
  stop("21_ndvi.R: no latitude/longitude columns found in ", basename(src_file))
}
if (!"respondent_row_id" %in% names(dat)) {
  stop("21_ndvi.R: respondent_row_id is missing; re-run 01_geocode_acs.R.")
}

pts <- dat %>%
  dplyr::transmute(respondent_row_id,
                   lat = to_num(.data[[lat_col]]),
                   lon = to_num(.data[[lon_col]])) %>%
  dplyr::filter(!is.na(lat), !is.na(lon))

cat("21_ndvi.R:", nrow(pts), "geocoded respondents\n")

# Unique locations, rounded to about 10 m. Respondents at the same address
# should not cost two downloads.
pts <- pts %>% dplyr::mutate(loc_key = sprintf("%.4f_%.4f", lat, lon))
# distinct() on all three columns kept two rows whenever coordinates that
# round to the same key differ in the fifth decimal, which duplicated the
# key and turned the join back onto respondents into a many-to-many one.
locs <- pts %>% dplyr::distinct(loc_key, .keep_all = TRUE) %>%
  dplyr::select(loc_key, lat, lon)
cat("  ", nrow(locs), "unique locations\n")

# -----------------------------------------------------------------------------
# Buffer means
# -----------------------------------------------------------------------------
# The circles are built in the raster's own coordinate system and never
# reprojected, for a specific reason. MODIS grids are defined on a sphere with
# no datum, and PROJ cannot always transform a datum-bearing CRS such as
# NAD83 / UTM 13N onto it: it goes looking for a transformation grid, and if it
# cannot fetch one it silently returns NA coordinates and every extraction comes
# back NaN. Placing the points analytically avoids the question entirely.
#
# Sinusoidal is true to scale along parallels and along the central meridian,
# and the distortion over a 1,600 m circle at Denver's latitude is far below
# the 250 m pixel size, so a circle drawn in projected metres is a faithful
# stand-in for a circle on the ground at this resolution.

MODIS_R <- 6371007.181            # radius of the MODIS sphere, metres

lonlat_to_sinu <- function(lon, lat) {
  cbind(x = MODIS_R * (lon * pi / 180) * cos(lat * pi / 180),
        y = MODIS_R * (lat * pi / 180))
}

extract_buffers <- function(r, loc_df, sinusoidal = FALSE) {
  if (sinusoidal) {
    xy <- lonlat_to_sinu(loc_df$lon, loc_df$lat)
    v  <- terra::vect(xy, crs = terra::crs(r))
  } else {
    p <- sf::st_as_sf(loc_df, coords = c("lon", "lat"), crs = 4326)
    p <- sf::st_transform(p, terra::crs(r))
    if (any(!is.finite(sf::st_coordinates(p)))) {
      stop("21_ndvi.R: could not place the respondent points in the raster's ",
           "coordinate system (the transform returned missing coordinates). ",
           "Reproject ndvi_raster.tif to UTM 13N (EPSG:26913) and try again.")
    }
    v <- terra::vect(p)
  }

  out <- data.frame(loc_key = loc_df$loc_key, stringsAsFactors = FALSE)
  for (rad in RADII) {
    buf <- terra::buffer(v, width = rad)
    # touches = TRUE counts any pixel the circle overlaps, not only those whose
    # centre falls inside. At 250 m a 400 m radius circle contains few centres,
    # and without this the smallest buffer would be noisy or empty.
    vals <- terra::extract(r, buf, fun = mean, na.rm = TRUE, ID = FALSE,
                           touches = TRUE)
    out[[paste0("ndvi_", rad)]] <- as.numeric(vals[[1]])
  }
  out
}

ndvi_raster_path <- file.path(out_dir, "ndvi_raster.tif")

if (file.exists(ndvi_raster_path)) {

  # ---------------------------------------------------------------------------
  # A. Local raster
  # ---------------------------------------------------------------------------
  cat("Using local raster:", ndvi_raster_path, "\n")
  r <- terra::rast(ndvi_raster_path)
  if (terra::nlyr(r) > 1) {
    cat("  raster has", terra::nlyr(r), "layers; averaging them\n")
    r <- terra::app(r, fun = mean, na.rm = TRUE)
  }
  rng <- terra::global(r, range, na.rm = TRUE)
  cat("  value range:", sprintf("%.3f to %.3f", rng[[1]], rng[[2]]), "\n")
  if (rng[[2]] > 1.5) {
    cat("  values exceed 1, so this looks like scaled integer NDVI; dividing by 10000\n")
    r <- r / 10000
  }
  loc_ndvi <- extract_buffers(r, locs, sinusoidal = FALSE)

} else {

  # ---------------------------------------------------------------------------
  # B. MODIS MOD13Q1 through the ORNL DAAC
  # ---------------------------------------------------------------------------
  cat("No local raster at", ndvi_raster_path, "\n")
  cat("Falling back to MODIS MOD13Q1 (250 m) via the ORNL DAAC web service.\n")
  wb_require("MODISTools")

  cat("\nThis makes one request per unique location and caches each result, so\n")
  cat("it can be interrupted and restarted. Budget roughly 5 to 20 seconds per\n")
  cat("location on a first run; cached locations are instant.\n\n")

  cache_dir <- file.path(out_dir, "ndvi_cache")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # 2 km either side of the point covers the 1,600 m circle with room to spare.
  KM_BOX <- 2

  fetch_one <- function(i) {
    key <- locs$loc_key[i]
    f <- file.path(cache_dir, paste0(key, ".rds"))
    if (file.exists(f)) return(readRDS(f))
    one_try <- function() {
      MODISTools::mt_subset(
        product = "MOD13Q1", band = "250m_16_days_NDVI",
        lat = locs$lat[i], lon = locs$lon[i],
        start = GS_FROM, end = GS_TO,
        km_lr = KM_BOX, km_ab = KM_BOX,
        site_name = key, internal = TRUE, progress = FALSE)
    }
    res <- try(one_try(), silent = TRUE)
    if (inherits(res, "try-error")) {
      # one retry: the ORNL service drops the occasional request under load
      Sys.sleep(5)
      res <- try(one_try(), silent = TRUE)
    }
    if (inherits(res, "try-error")) {
      warning("21_ndvi.R: MODIS request failed twice for ", key, ": ",
              conditionMessage(attr(res, "condition")))
      return(NULL)
    }
    Sys.sleep(0.4)   # be a good citizen of a free public service
    saveRDS(res, f)
    res
  }

  subset_to_raster <- function(sub) {
    # mt_to_terra() returns one layer per composite date, already georeferenced.
    # reproject = FALSE keeps the native MODIS sinusoidal grid, so no
    # resampling happens; extract_buffers() moves the buffers onto that grid
    # instead. Resampling a 250 m NDVI field before averaging it would blur the
    # very thing being measured.
    r <- MODISTools::mt_to_terra(sub, reproject = FALSE)

    # MOD13Q1 stores NDVI as integers with a scale factor of 1e-4, but whether
    # MODISTools has already applied it depends on the version. Applying it a
    # second time is silent: the values stay perfectly correlated with the truth
    # and every correlation still looks right, while the numbers themselves come
    # out around 5e-5. Decide from the data instead of assuming.
    mx <- suppressWarnings(max(unlist(terra::global(r, "max", na.rm = TRUE)),
                               na.rm = TRUE))
    if (is.finite(mx) && mx > 1.5) r <- r * 0.0001
    r[r < -0.2] <- NA                     # water and fill
    terra::app(r, fun = mean, na.rm = TRUE)
  }

  rows <- vector("list", nrow(locs))
  t0 <- Sys.time()
  for (i in seq_len(nrow(locs))) {
    sub <- fetch_one(i)
    if (is.null(sub)) next
    r <- try(subset_to_raster(sub), silent = TRUE)
    if (inherits(r, "try-error")) next
    rows[[i]] <- extract_buffers(r, locs[i, , drop = FALSE], sinusoidal = TRUE)
    if (i %% 10 == 0 || i == nrow(locs)) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      cat(sprintf("  %d/%d locations  (%.1f min elapsed, ~%.0f min left)\n",
                  i, nrow(locs), el, el / i * (nrow(locs) - i)))
    }
  }
  loc_ndvi <- dplyr::bind_rows(rows)
  cat("MODIS extraction complete for", nrow(loc_ndvi), "of", nrow(locs), "locations\n")
}

# -----------------------------------------------------------------------------
# Back to respondents, and write
# -----------------------------------------------------------------------------
loc_ndvi <- dplyr::distinct(loc_ndvi, loc_key, .keep_all = TRUE)

out <- pts %>%
  dplyr::left_join(loc_ndvi, by = "loc_key", relationship = "many-to-one") %>%
  dplyr::select(respondent_row_id, dplyr::starts_with("ndvi_"))

if (nrow(out) != nrow(pts)) {
  stop("21_ndvi.R: the join changed the number of respondents (", nrow(pts),
       " -> ", nrow(out), "). Each location must appear once.")
}

readr::write_csv(out, file.path(out_dir, "ndvi_by_respondent.csv"))

# ---- the check that catches a scaling or join mistake before anything else --
v800 <- out$ndvi_800
if (all(is.na(v800))) stop("21_ndvi.R: every NDVI value is missing.")
if (stats::sd(v800, na.rm = TRUE) < 0.005 ||
    mean(v800, na.rm = TRUE) < 0.02 || mean(v800, na.rm = TRUE) > 1) {
  stop("21_ndvi.R: the NDVI values are not on the NDVI scale (mean ",
       sprintf("%.6f", mean(v800, na.rm = TRUE)), ", sd ",
       sprintf("%.6f", stats::sd(v800, na.rm = TRUE)), "). Vegetated urban ",
       "buffers should average roughly 0.2 to 0.6. Do not use this file.")
}

cat("\nWrote", file.path(out_dir, "ndvi_by_respondent.csv"), "\n")
for (rad in RADII) {
  v <- out[[paste0("ndvi_", rad)]]
  cat(sprintf("  ndvi_%-4d n = %3d  mean = %6.3f  sd = %5.3f  range %.3f to %.3f\n",
              rad, sum(!is.na(v)), mean(v, na.rm = TRUE), stats::sd(v, na.rm = TRUE),
              suppressWarnings(min(v, na.rm = TRUE)), suppressWarnings(max(v, na.rm = TRUE))))
}
# -----------------------------------------------------------------------------
# Sanity check: NDVI should agree with the canopy measures we already have
# -----------------------------------------------------------------------------
# This is the check that catches a georeferencing or scaling mistake. If NDVI
# were extracted at the wrong place, or the scale factor were wrong, it would
# show no relationship with tree canopy in the same buffer.
chk <- dat %>%
  dplyr::select(dplyr::any_of(c("respondent_row_id", "lc_800m_tree_canopy",
                                "tree_treecanopy", "lc_800m_impervious_surfaces"))) %>%
  dplyr::left_join(out, by = "respondent_row_id")

if ("ndvi_800" %in% names(chk)) {
  cat("\nSanity check, correlation of ndvi_800 with measures we already have:\n")
  for (v in c("lc_800m_tree_canopy", "tree_treecanopy", "lc_800m_impervious_surfaces")) {
    if (v %in% names(chk)) {
      r <- suppressWarnings(stats::cor(to_num(chk[[v]]), chk$ndvi_800,
                                       use = "complete.obs"))
      cat(sprintf("  %-30s r = %+.3f\n", v, r))
    }
  }
  cat("  Expect positive correlations with the two canopy measures and a\n")
  cat("  negative one with impervious surface. If the signs are wrong, stop\n")
  cat("  and tell me before running anything downstream.\n")
}

cat("\nNow re-run 20_new_variables.R so NDVI enters the analysis file.\n")
