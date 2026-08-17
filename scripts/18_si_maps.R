# =============================================================================
# 18_si_maps.R
# The three maps the Supplementary Information needs: where respondents live,
# how the study area is zoned, and how the area-level model measures vary.
#
#   INPUT : respondents_with_transport_ht_segpoi_stoz_diversity.csv  (05)
#           Zoning/ALL_DenverMSA_10.3.23.shp
#           census geography from tigris (needs internet the first time;
#           options(tigris_use_cache = TRUE) caches it afterwards)
#   OUTPUT: figures/Figure_S10_respondents.png / .pdf
#           figures/Figure_S11_zoning.png / .pdf
#           figures/Figure_S12_measures.png / .pdf
#           figures/si_map_counts_by_tract.csv
#
# DISCLOSURE. Individual residential locations are never plotted. The survey
# records approximate home coordinates for 381 foreign-born adults, a quarter of
# whom report no official immigration status, and a dot map of those homes would
# be re-identifiable in a metropolitan area this size. Respondents are shown only
# as binned counts per census tract, and tracts with one or two respondents are
# pooled into a single lowest class rather than given an exact count.
#
# Note that 06_maps_descriptives.R writes point maps for internal checking
# (map_respondent_locations.png and the two point maps of outcome scores). Those
# are working files. Do not put them in a manuscript or the SI.
#
# Drawn with sf geometry and base graphics, like the other figures here.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
wb_require(c("dplyr", "readr", "sf", "tigris"))

options(tigris_use_cache = TRUE)
sf::sf_use_s2(FALSE)

fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

dat <- readr::read_csv(analysis_file, show_col_types = FALSE)
if (!all(c("tract_geoid", "bg_geoid") %in% names(dat))) {
  stop("18_si_maps.R: tract_geoid and bg_geoid are needed; re-run 01_geocode_acs.R.")
}
dat$tract_geoid <- clean_geoid(dat$tract_geoid, 11)
dat$bg_geoid    <- clean_geoid(dat$bg_geoid, 12)

METRO <- c("Denver", "Adams", "Arapahoe", "Jefferson", "Douglas", "Broomfield")

# -----------------------------------------------------------------------------
# Geography
# -----------------------------------------------------------------------------
co_counties <- tigris::counties(state = "CO", cb = TRUE, year = 2019, progress_bar = FALSE)
metro <- co_counties[co_counties$NAME %in% METRO, ]

tr <- tigris::tracts(state = "CO", cb = TRUE, year = 2019, progress_bar = FALSE)
bg <- tigris::block_groups(state = "CO", cb = TRUE, year = 2019, progress_bar = FALSE)

CRS <- 26913                                   # NAD83 / UTM 13N, metres
metro <- sf::st_transform(metro, CRS)
tr    <- sf::st_transform(tr, CRS)
bg    <- sf::st_transform(bg, CRS)
co    <- sf::st_transform(co_counties, CRS)

# tracts containing at least one respondent define the study area
tr_study <- tr[tr$GEOID %in% unique(dat$tract_geoid), ]
if (!nrow(tr_study)) stop("18_si_maps.R: no tracts matched tract_geoid; check the join.")

# -----------------------------------------------------------------------------
# Palettes and helpers
# -----------------------------------------------------------------------------
SEQ  <- c("#F2F0EA", "#D6DFE4", "#A9C0CC", "#6F97AA", "#3D6E86", "#1F4E5F")
ZCOL <- c("Residential, low density"        = "#CFE0CC",
          "Residential, medium-high density"= "#8FB79A",
          "Mixed use"                       = "#E2C391",
          "Nonresidential"                  = "#B9A6B7")
INK <- "#1A1A1A"; MUTE <- "#6B6B6B"; EDGE <- "#8E8E8E"

frame <- function(x, main = NULL, sub = NULL) {
  plot(sf::st_geometry(x), col = NA, border = NA, reset = FALSE)
  if (!is.null(main)) title(main = main, adj = 0, cex.main = 1.0, font.main = 2, col.main = INK)
  if (!is.null(sub))  mtext(sub, side = 3, adj = 0, line = 0.1, cex = 0.7, col = MUTE)
}

scalebar <- function(x, len_km = 10) {
  bb <- sf::st_bbox(x)
  x0 <- bb["xmin"] + 0.06 * (bb["xmax"] - bb["xmin"])
  y0 <- bb["ymin"] + 0.06 * (bb["ymax"] - bb["ymin"])
  segments(x0, y0, x0 + len_km * 1000, y0, lwd = 3, lend = 1, col = INK)
  text(x0 + len_km * 500, y0, sprintf("%d km", len_km), pos = 3, cex = 0.6, col = INK)
}

legend_box <- function(labels, fills, title, cex = 0.62) {
  legend("bottomright", legend = labels, fill = fills, border = EDGE,
         bty = "n", cex = cex, title = title, title.adj = 0, inset = c(0.01, 0.02))
}

# =============================================================================
# Figure S10: study area and respondent counts per tract
# =============================================================================
cnt <- dat %>%
  dplyr::filter(!is.na(tract_geoid)) %>%
  dplyr::count(tract_geoid, name = "respondents")

tr_study$respondents <- cnt$respondents[match(tr_study$GEOID, cnt$tract_geoid)]
tr_study$respondents[is.na(tr_study$respondents)] <- 0

# counts of one and two are pooled, so no tract is published with an exact
# small-cell count
brk_lab <- c("1-2", "3-5", "6-10", "more than 10")
cls <- cut(tr_study$respondents, breaks = c(0, 2, 5, 10, Inf), labels = brk_lab)
fills <- c("#DCE6EA", "#A9C0CC", "#4E7D93", "#1F4E5F")
tr_study$fill <- ifelse(is.na(cls), "#FFFFFF", fills[as.integer(cls)])

readr::write_csv(
  data.frame(class = brk_lab,
             tracts = as.integer(table(factor(cls, levels = brk_lab))),
             respondents = as.integer(tapply(tr_study$respondents,
                                             factor(cls, levels = brk_lab), sum))),
  file.path(fig_dir, "si_map_counts_by_tract.csv"))

draw_s10 <- function() {
  par(mfrow = c(1, 2), mar = c(0.5, 0.5, 3.2, 0.5), family = "serif")

  # (a) Colorado, metro highlighted
  frame(co, "a.  Study area", "six-county Denver metropolitan area within Colorado")
  plot(sf::st_geometry(co),    col = "#F4F2ED", border = "#D8D4CC", lwd = 0.4, add = TRUE)
  plot(sf::st_geometry(metro), col = "#C8D8DF", border = "#5B7185", lwd = 0.8, add = TRUE)

  # (b) respondents per tract
  frame(tr_study, "b.  Survey respondents per census tract",
        sprintf("%d respondents across %d tracts; individual homes are not mapped",
                sum(tr_study$respondents), sum(tr_study$respondents > 0)))
  plot(sf::st_geometry(metro), col = "#FBFAF8", border = "#C9C4BC", lwd = 0.5, add = TRUE)
  plot(sf::st_geometry(tr_study), col = tr_study$fill, border = "#FFFFFF", lwd = 0.25, add = TRUE)
  plot(sf::st_geometry(metro), col = NA, border = "#7C7C7C", lwd = 0.8, add = TRUE)
  legend_box(brk_lab, fills, "Respondents")
  scalebar(tr_study)
}

grDevices::png(file.path(fig_dir, "Figure_S10_respondents.png"),
               width = 10.4, height = 5.6, units = "in", res = 400, type = "cairo", bg = "white")
draw_s10(); grDevices::dev.off()
grDevices::pdf(file.path(fig_dir, "Figure_S10_respondents.pdf"), width = 10.4, height = 5.6)
draw_s10(); grDevices::dev.off()

# =============================================================================
# Figure S11: generalized zoning
# =============================================================================
zon_path <- file.path(out_dir, "Zoning", "ALL_DenverMSA_10.3.23.shp")
if (!file.exists(zon_path)) stop("18_si_maps.R: zoning shapefile not found at ", zon_path)

zon <- sf::st_read(zon_path, quiet = TRUE)
# The shapefile spells it GenZone2; 02_enviro_zoning_walkability_landcover.R
# only sees gen_zone2 because janitor::clean_names() runs there and not here.
zcol <- grep("gen[_]?zone[_]?2", names(zon), ignore.case = TRUE, value = TRUE)[1]
if (is.na(zcol)) stop("18_si_maps.R: no generalized-zoning column in the zoning file; ",
                      "columns are: ", paste(names(zon), collapse = ", "))
cat("18_si_maps.R: using zoning column '", zcol, "'\n", sep = "")
zon <- sf::st_transform(zon, CRS)

# the same four categories the land-use models use
collapse_zone <- function(x) {
  x <- as.character(x); out <- rep(NA_character_, length(x))
  out[x == "Residential_Low"] <- "Residential, low density"
  out[x %in% c("Residential_Med", "Residential_MedHigh", "Residential_High")] <-
    "Residential, medium-high density"
  out[grepl("^MixedUse", x)] <- "Mixed use"
  out[x %in% c("Commercial", "Industrial", "Civic", "OpenSpace")] <- "Nonresidential"
  factor(out, levels = names(ZCOL))
}
zon$cat <- collapse_zone(zon[[zcol]])
zon <- sf::st_make_valid(zon)
zon_study <- suppressWarnings(sf::st_crop(zon, sf::st_bbox(sf::st_buffer(tr_study, 1500))))

draw_s11 <- function() {
  par(mar = c(0.5, 0.5, 3.2, 0.5), family = "serif")
  frame(tr_study, "Generalized zoning across the study area",
        "parcel zoning collapsed into the four categories entered in the land use models")
  plot(sf::st_geometry(metro), col = "#FBFAF8", border = "#C9C4BC", lwd = 0.5, add = TRUE)
  plot(sf::st_geometry(zon_study), col = ifelse(is.na(zon_study$cat), "#EFEFEF",
                                                ZCOL[as.character(zon_study$cat)]),
       border = NA, add = TRUE)
  plot(sf::st_geometry(tr_study), col = NA, border = "#FFFFFF", lwd = 0.2, add = TRUE)
  plot(sf::st_geometry(metro), col = NA, border = "#7C7C7C", lwd = 0.8, add = TRUE)
  legend_box(c(names(ZCOL), "not classified"), c(unname(ZCOL), "#EFEFEF"), "Zoning category")
  scalebar(tr_study)
}

grDevices::png(file.path(fig_dir, "Figure_S11_zoning.png"),
               width = 8.2, height = 8.0, units = "in", res = 400, type = "cairo", bg = "white")
draw_s11(); grDevices::dev.off()
grDevices::pdf(file.path(fig_dir, "Figure_S11_zoning.pdf"), width = 8.2, height = 8.0)
draw_s11(); grDevices::dev.off()

# =============================================================================
# Figure S12: the area-level measures that enter the models
# =============================================================================
# Only measures defined on a census geography can be mapped as a surface. The
# buffer-derived exposures are specific to each respondent's home and have no
# areal footprint; they are described in Tables S2 to S12 instead.
AREA_VARS <- list(
  list(col = "pop_density",       geo = "tract", lab = "Population density",        unit = "persons/km2"),
  list(col = "pct_poverty",       geo = "tract", lab = "Population below poverty",  unit = "%"),
  list(col = "pct_non_native",    geo = "tract", lab = "Foreign-born population",   unit = "%"),
  list(col = "walk_nat_walk_ind", geo = "bg",    lab = "EPA National Walkability Index", unit = "index"),
  list(col = "tree_tes",          geo = "bg",    lab = "Tree Equity Score",         unit = "score"),
  list(col = "hudjob_jobs_idx",   geo = "bg",    lab = "HUD Jobs Proximity Index",  unit = "index")
)
AREA_VARS <- Filter(function(v) v$col %in% names(dat), AREA_VARS)
if (!length(AREA_VARS)) stop("18_si_maps.R: none of the mapped measures are in the analysis file.")

bg_study <- bg[bg$GEOID %in% unique(dat$bg_geoid), ]

quantile_fill <- function(v, n = 5) {
  qs <- unique(stats::quantile(v, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE))
  if (length(qs) < 3) return(list(fill = rep(SEQ[3], length(v)), labs = "constant"))
  cls <- cut(v, breaks = qs, include.lowest = TRUE, labels = FALSE)
  pal <- SEQ[seq(2, length(SEQ), length.out = length(qs) - 1)]
  labs <- sprintf("%.0f to %.0f", qs[-length(qs)], qs[-1])
  list(fill = ifelse(is.na(cls), "#FFFFFF", pal[cls]), labs = labs, pal = pal)
}

draw_s12 <- function() {
  par(mfrow = c(2, 3), mar = c(0.4, 0.4, 3.0, 0.4), family = "serif")
  for (v in AREA_VARS) {
    g   <- if (v$geo == "tract") tr_study else bg_study
    key <- if (v$geo == "tract") "tract_geoid" else "bg_geoid"
    val <- tapply(suppressWarnings(as.numeric(dat[[v$col]])), dat[[key]], mean, na.rm = TRUE)
    g$val <- as.numeric(val[match(g$GEOID, names(val))])
    q <- quantile_fill(g$val)
    frame(g, v$lab, sprintf("%s, by census %s", v$unit,
                            ifelse(v$geo == "tract", "tract", "block group")))
    plot(sf::st_geometry(metro), col = "#FBFAF8", border = "#C9C4BC", lwd = 0.4, add = TRUE)
    plot(sf::st_geometry(g), col = q$fill, border = "#FFFFFF", lwd = 0.2, add = TRUE)
    plot(sf::st_geometry(metro), col = NA, border = "#7C7C7C", lwd = 0.6, add = TRUE)
    legend("bottomright", legend = q$labs, fill = q$pal, border = EDGE,
           bty = "n", cex = 0.55, inset = c(0.01, 0.02))
  }
}

grDevices::png(file.path(fig_dir, "Figure_S12_measures.png"),
               width = 12.6, height = 8.6, units = "in", res = 400, type = "cairo", bg = "white")
draw_s12(); grDevices::dev.off()
grDevices::pdf(file.path(fig_dir, "Figure_S12_measures.pdf"), width = 12.6, height = 8.6)
draw_s12(); grDevices::dev.off()

cat("\nSI maps written to", fig_dir, "\n")
cat(sprintf("  Figure S10: %d respondents across %d tracts (counts of 1-2 pooled)\n",
            sum(tr_study$respondents), sum(tr_study$respondents > 0)))
cat(sprintf("  Figure S11: %d zoning polygons in the study extent\n", nrow(zon_study)))
cat(sprintf("  Figure S12: %d area-level measures mapped\n", length(AREA_VARS)))
