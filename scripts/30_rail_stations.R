# =============================================================================
# 30_rail_stations.R
# Two spatial additions for the v3 analysis.
#
#   1. Rail station proximity (v3 transportation infrastructure domain).
#   2. EnviroScreen re-joined on 2020 block-group geometry.
#
#   INPUT : respondents_with_new_variables.csv          (20_new_variables.R)
#           rtd_rail_stations/rtd_rail_stations.csv     (see its README.md)
#           Enviroscreen.csv                            (Colorado EnviroScreen 2.0)
#           2020 block-group geometry (tigris; falls back to app/data/geo.rds)
#   OUTPUT: respondents_v3_analysis.csv  = input
#             + rail_station_{400,800,1600}, rail_station_nearest_km
#             + env_pm25, env_npl_proximity re-joined (v2 values kept as *_v2)
#
# RAIL. Stations are RTD light-rail and commuter-rail stations open on or before
# 31 December 2019, the survey year. rail_station_800 is 1 when at least one
# station lies within 800 m of the geocoded residence (the radius of the other
# buffer measures), 0 otherwise; 400 m and 1,600 m versions feed the
# buffer-radius check. Distances are computed in the project CRS (NAD83 / UTM
# 13N), like every other distance in the pipeline.
#
# ENVIROSCREEN. Colorado EnviroScreen 2.0 is published on 2020 census block
# groups. 02_enviro_zoning_walkability_landcover.R joined it on the respondent's
# 2019 (2010-vintage) block-group GEOID, which matches only where the 2020
# boundaries kept the 2010 identifier (278 of 358 geocoded respondents in the
# October 2026 run). Joining each residence to the 2020 block group it falls in
# recovers the measure for every respondent inside the six-county area; the
# counts are printed below and written to v3_summary.csv by 31. The two PM2.5 and
# NPL measures are therefore recomputed here; the previous values are kept as
# env_pm25_v2 and env_npl_proximity_v2 for comparison.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
wb_require(c("dplyr", "readr", "sf"))

in_file  <- file.path(out_dir, "respondents_with_new_variables.csv")
st_file  <- file.path(out_dir, "rtd_rail_stations", "rtd_rail_stations.csv")
es_file  <- file.path(out_dir, "Enviroscreen.csv")
out_file <- file.path(out_dir, "respondents_v3_analysis.csv")
for (f in c(in_file, st_file, es_file)) if (!file.exists(f)) stop("30_rail_stations.R: missing ", f)

dat <- readr::read_csv(in_file, show_col_types = FALSE, guess_max = 5000)
has_xy <- !is.na(dat$longitude) & !is.na(dat$latitude)
pts_r  <- sf::st_as_sf(dat[has_xy, c("longitude", "latitude")],
                       coords = c("longitude", "latitude"), crs = 4326) |>
  sf::st_transform(target_crs)

# ---- 1. Rail stations --------------------------------------------------------
st <- readr::read_csv(st_file, show_col_types = FALSE)
CUTOFF <- as.Date("2019-12-31")
st$opened <- as.Date(st$opened)
if (anyNA(st$opened)) stop("30_rail_stations.R: unparseable 'opened' date for: ",
                           paste(st$station[is.na(st$opened)], collapse = ", "))
st_open <- st[st$opened <= CUTOFF, ]
cat(sprintf("Stations in file: %d; open by %s: %d\n", nrow(st), CUTOFF, nrow(st_open)))

pts_st <- sf::st_as_sf(st_open, coords = c("lon", "lat"), crs = 4326) |>
  sf::st_transform(target_crs)
D <- sf::st_distance(pts_r, pts_st)
nearest_m <- as.numeric(apply(D, 1, min))

dat$rail_station_nearest_km <- NA_real_
dat$rail_station_nearest_km[has_xy] <- nearest_m / 1000
for (r in buffers_m) {
  col <- sprintf("rail_station_%d", r)
  dat[[col]] <- NA_real_
  dat[[col]][has_xy] <- as.numeric(nearest_m <= r)
}
cat("\nRespondents with a station within:\n")
for (r in buffers_m) {
  col <- sprintf("rail_station_%d", r)
  cat(sprintf("  %5d m: %3d of %d geocoded (%.1f%%)\n", r, sum(dat[[col]], na.rm = TRUE),
              sum(has_xy), 100 * mean(dat[[col]], na.rm = TRUE)))
}
cat(sprintf("Distance to nearest station: median %.2f km, IQR %.2f-%.2f km\n",
            median(dat$rail_station_nearest_km, na.rm = TRUE),
            quantile(dat$rail_station_nearest_km, .25, na.rm = TRUE),
            quantile(dat$rail_station_nearest_km, .75, na.rm = TRUE)))

# ---- 2. EnviroScreen on 2020 block groups ------------------------------------
pad12 <- function(x) { x <- gsub("\\.0+$", "", as.character(x)); ok <- !is.na(x)
  x[ok] <- paste0(strrep("0", pmax(0, 12 - nchar(x[ok]))), x[ok]); x }
es <- readr::read_csv(es_file, col_types = readr::cols(GEOID = "c", .default = "?"),
                      show_col_types = FALSE)
pm_col  <- grep("^Fine Particle Pollution \\((\u00b5|u)g/m3\\)$", names(es), value = TRUE)
npl_col <- grep("^Proximity to National Priorities List Sites$", names(es), value = TRUE)
if (length(pm_col) != 1 || length(npl_col) != 1)
  stop("30_rail_stations.R: could not identify the PM2.5 concentration / NPL proximity columns in Enviroscreen.csv")
es <- data.frame(GEOID = pad12(es$GEOID), env_pm25 = as.numeric(es[[pm_col]]),
                 env_npl_proximity = as.numeric(es[[npl_col]]))

# Geometry source, in order of preference, so that every machine uses the same
# polygons once the cached file exists:
#   1. data/census_bg_2020_six_counties.gpkg  (written by this script on first use)
#   2. tigris download of the 2020 TIGER/Line block groups, restricted to the six
#      metro counties, then cached as (1)
#   3. app/data/geo.rds, the simplified six-county polygons the Shiny app uses
#      (boundary residences can land in a different block group than under 1-2)
METRO_FIPS <- c("08001", "08005", "08014", "08031", "08035", "08059")  # Adams, Arapahoe, Broomfield, Denver, Douglas, Jefferson
bg_cache <- file.path(out_dir, "census_bg_2020_six_counties.gpkg")
bg20 <- NULL; bg20_source <- NA_character_
if (file.exists(bg_cache)) {
  bg20 <- sf::st_read(bg_cache, quiet = TRUE); bg20_source <- "cached TIGER/Line 2020 (census_bg_2020_six_counties.gpkg)"
} else if (requireNamespace("tigris", quietly = TRUE)) {
  bg20 <- tryCatch(tigris::block_groups(state = "CO", cb = FALSE, year = 2020, progress_bar = FALSE),
                   error = function(e) NULL)
  if (!is.null(bg20)) {
    bg20 <- bg20[substr(bg20$GEOID, 1, 5) %in% METRO_FIPS, "GEOID"]
    sf::st_write(bg20, bg_cache, quiet = TRUE)
    bg20_source <- "tigris TIGER/Line 2020, six counties (now cached)"
  }
}
if (is.null(bg20)) {
  alt <- file.path(dirname(out_dir), "Wellbeing_Denver_immigrants", "app", "data", "geo.rds")
  if (!file.exists(alt)) alt <- file.path("..", "app", "data", "geo.rds")
  if (!file.exists(alt)) stop("30_rail_stations.R: need census_bg_2020_six_counties.gpkg, tigris (with internet), or app/data/geo.rds for 2020 block groups")
  bg20 <- readRDS(alt)$layers$bg20[, "GEOID"]
  bg20_source <- "app/data/geo.rds (six counties, simplified geometry)"
}
cat("2020 block groups:", bg20_source, "\n")
writeLines(c(sprintf("bg20_source: %s", bg20_source), sprintf("run: %s", format(Sys.time()))),
           file.path(out_dir, "v3_geometry_source.txt"))
bg20 <- sf::st_transform(bg20, target_crs)
sf::sf_use_s2(FALSE)
hit <- sf::st_within(pts_r, bg20)
# a point on a shared (simplified) boundary can match two polygons; take the nearest
bg_idx <- vapply(seq_along(hit), function(i) {
  h <- hit[[i]]
  if (length(h) == 1) return(h)
  if (length(h) == 0) return(NA_integer_)
  h[which.min(as.numeric(sf::st_distance(pts_r[i, ], suppressWarnings(sf::st_centroid(bg20[h, ])))))]
}, integer(1))
bg20_geoid <- bg20$GEOID[bg_idx]
m <- match(bg20_geoid, es$GEOID)

dat$env_pm25_v2          <- dat$env_pm25
dat$env_npl_proximity_v2 <- dat$env_npl_proximity
dat$bg_geoid_2020 <- NA_character_; dat$bg_geoid_2020[has_xy] <- bg20_geoid
dat$env_pm25 <- NA_real_;          dat$env_pm25[has_xy]          <- es$env_pm25[m]
dat$env_npl_proximity <- NA_real_; dat$env_npl_proximity[has_xy] <- es$env_npl_proximity[m]

cat(sprintf("\nEnviroScreen: 2019-GEOID join had %d of %d geocoded respondents; 2020 spatial join has %d\n",
            sum(!is.na(dat$env_pm25_v2[has_xy])), sum(has_xy), sum(!is.na(dat$env_pm25[has_xy]))))
both <- !is.na(dat$env_pm25_v2) & !is.na(dat$env_pm25)
cat(sprintf("  where both are present (n = %d): PM2.5 identical for %d, median |diff| %.3f, max %.3f\n",
            sum(both), sum(abs(dat$env_pm25_v2[both] - dat$env_pm25[both]) < 1e-9),
            median(abs(dat$env_pm25_v2[both] - dat$env_pm25[both])),
            max(abs(dat$env_pm25_v2[both] - dat$env_pm25[both]))))

readr::write_csv(dat, out_file)
cat("\nWrote", out_file, "\n")
