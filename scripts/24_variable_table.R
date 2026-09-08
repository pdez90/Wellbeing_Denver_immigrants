# =============================================================================
# 24_variable_table.R
# Every variable in the analysis, with its definition, units, geography, vintage
# and source, laid out in the order the categories are defined.
#
#   INPUT : model_objects.rds  (from 08_domain_models.R)
#   OUTPUT: variable_table.csv          the table as it appears in the SI
#           variable_table_check.csv    coverage and completeness audit
#
# The table answers a question the descriptive appendix does not: where each
# measure came from and what vintage it is. Section S1 says how the variables
# are distributed; this says what they are.
#
# The registry below is the single description of each variable. Like the one in
# 13_descriptives_all.R, it is checked against the models: if a variable enters a
# model and is not described here, the script stops rather than printing an
# incomplete table.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
if (!exists(".wb_domains_loaded")) source("wb_domains.R")
wb_require(c("tidyverse", "readr"))

obj <- readRDS(file.path(out_dir, "model_objects.rds"))
dat <- obj$dat_derived

V <- function(name, label, definition, unit, geo, vintage, source) {
  data.frame(name = name, label = label, definition = definition, unit = unit,
             geo = geo, vintage = vintage, source = source,
             stringsAsFactors = FALSE)
}

ACS   <- "American Community Survey 5-year estimates"
DRCOG <- "Denver Regional Council of Governments"

# -----------------------------------------------------------------------------
# The registry, in the order the table prints
# -----------------------------------------------------------------------------
SECTIONS <- list(

  "OUTCOME VARIABLES" = list(
    "_" = rbind(
      V("swb_index", "Subjective well-being",
        "Mean of six items on satisfaction with standard of living, health, life achievements, personal safety, acceptance in the community and future financial security",
        "1-5 scale", "respondent", "2019", "Survey, items adapted from the Personal Well-Being Index"),
      V("belonging_index", "Neighborhood belonging",
        "Mean of four items on feeling one belongs, trusting neighbors, exchanging favors and having regular contact with neighbors",
        "1-5 scale", "respondent", "2019", "Survey")
    )),

  "CONTROL VARIABLES: NEIGHBORHOOD DEMOGRAPHICS" = list(
    "_" = rbind(
      V("pct_poverty", "Population below poverty",
        "Share of residents with an income-to-poverty ratio below 1.00 (C17002)",
        "%", "block group", "2015-2019", ACS),
      V("pct_non_native", "Foreign-born population",
        "Share of residents born outside the United States (B05012); not published below tract level",
        "%", "tract", "2015-2019", ACS),
      V("neighborhood_ses_index", "Neighborhood SES index",
        "Mean of standardized median household income, median rent and median home value",
        "index", "block group", "2015-2019", ACS),
      V("pct_nh_white", "Non-Hispanic white population",
        "Non-Hispanic white residents as a share of the total population (B03002)",
        "%", "block group", "2015-2019", ACS),
      V("div_total_diversity_resi", "Residential diversity",
        "Probability that two residents drawn at random differ in race or ethnicity",
        "index, 0-1", "tract", "2020 Census", "Xu et al. (2024), doi:10.1038/s41597-024-03490-y"),
      V("div_exposure_mean", "Experienced diversity",
        "The same probability computed over people present in the tract, from mobile-device records, averaged across five time-of-day intervals and weekday/weekend",
        "index, 0-1", "tract", "2022", "Xu et al. (2024), doi:10.1038/s41597-024-03490-y")
    )),

  "NEIGHBORHOOD BUILT ENVIRONMENT (NBE) VARIABLES" = list(

    "CATEGORY 1: Density, location and urban form" = rbind(
      V("dist_downtown_km", "Distance to downtown Denver",
        "Straight-line distance from the geocoded residence to downtown Denver",
        "km", "residence", "2019", "Computed from geocoded coordinates"),
      V("short_trip_zone_share_800", "Short-trip opportunity zone share",
        "Share of the 800 m buffer inside a designated short-trip opportunity zone",
        "share of buffer", "800 m buffer", "2019", paste(DRCOG, "short-trip opportunity zones")),
      V("walk_nat_walk_ind", "Walkability",
        "EPA National Walkability Index, combining intersection density, land-use mix, transit proximity and employment mix",
        "index, 1-20", "block group", "2021", "U.S. EPA National Walkability Index"),
      V("street_intdensity", "Street intersection density",
        "Real intersection nodes divided by tract land area; reported per square mile, following the source definition",
        "per sq mile", "tract", "2020", "National Neighborhood Data Archive (ICPSR 38580, DS0003)"),
      V("urban_center_nearest_dist_m", "Distance to nearest urban center",
        "Straight-line distance to the nearest regionally designated urban center",
        "m", "residence", "2019", paste(DRCOG, "urban centers")),
      V("pop_density", "Population density",
        "Residents divided by block-group land area",
        "persons/sq km", "block group", "2015-2019", ACS),
      V("housing_density", "Housing density",
        "Housing units divided by block-group land area",
        "units/sq km", "block group", "2015-2019", ACS),
      V("pct_multifamily", "Share of multifamily housing",
        "Housing units in structures of two or more units as a share of all units (B25024)",
        "% of units", "block group", "2015-2019", ACS)
    ),

    "CATEGORY 2: Transportation and accessibility" = rbind(
      V("hudjob_jobs_idx", "Jobs proximity",
        "HUD Jobs Proximity Index, higher values indicating better access to employment",
        "index, 0-100", "block group", "2020", "HUD Jobs Proximity Index"),
      V("sidewalk_density_800", "Sidewalk density",
        "Sidewalk centerline length within the 800 m buffer divided by buffer area",
        "m/sq km", "800 m buffer", "2022", paste(DRCOG, "planimetric sidewalk centerlines")),
      V("bike_facility_density_800", "Bicycle facility density",
        "Bicycle facility length within the 800 m buffer divided by buffer area",
        "m/sq km", "800 m buffer", "2023", paste(DRCOG, "bicycle facility inventory")),
      V("active_corridor_density_800", "Active corridor density",
        "Active transportation corridor length within the 800 m buffer divided by buffer area",
        "m/sq km", "800 m buffer", "2023", paste(DRCOG, "active transportation corridors")),
      V("ht_t_ami", "Transportation cost at AMI",
        "Modelled transportation costs as a share of income for a household at the area median income",
        "% of income", "block group", "2019",
        "Center for Neighborhood Technology Housing + Transportation Affordability Index")
    ),

    "CATEGORY 3: Green space" = rbind(
      V("tree_tes", "Tree equity score",
        "American Forests Tree Equity Score, combining canopy cover with demographic and heat indicators",
        "score, 0-100", "block group", "2023", "American Forests Tree Equity Score"),
      V("tree_treecanopy", "Tree canopy share",
        "Block-group tree canopy share, the measure the Tree Equity Score is derived from",
        "share, 0-1", "block group", "2023", "American Forests Tree Equity Score"),
      V("park_acres_half_mile", "Park acreage within 800 m",
        "Total park area within the 800 m buffer",
        "acres", "800 m buffer", "2024", paste(DRCOG, "parks and open space")),
      V("park_nearest_dist_m", "Distance to nearest park",
        "Straight-line distance from the residence to the nearest park polygon",
        "m", "residence", "2024", paste(DRCOG, "parks and open space")),
      V("lc_800m_tree_canopy", "Tree canopy land cover",
        "Share of the 800 m buffer classified as tree canopy",
        "share of buffer", "800 m buffer", "2020", paste(DRCOG, "regional land cover")),
      V("ndvi_800", "NDVI",
        "Mean normalized difference vegetation index within the 800 m buffer, averaged over growing-season composites",
        "index, -1 to 1", "800 m buffer", "2019 growing season",
        "MODIS MOD13Q1 (250 m, 16-day) via the ORNL DAAC")
    ),

    "CATEGORY 4: Safety and environmental quality" = rbind(
      V("crash_density_800", "Crash density",
        "All reported crashes within the 800 m buffer divided by buffer area",
        "crashes/sq km", "800 m buffer", "2019", paste(DRCOG, "regional crash records")),
      V("ped_crash_density_800", "Pedestrian crash density",
        "Crashes involving a pedestrian within the 800 m buffer divided by buffer area",
        "crashes/sq km", "800 m buffer", "2019", paste(DRCOG, "regional crash records")),
      V("bike_crash_density_800", "Bicycle crash density",
        "Crashes involving a cyclist within the 800 m buffer divided by buffer area",
        "crashes/sq km", "800 m buffer", "2019", paste(DRCOG, "regional crash records")),
      V("lc_800m_impervious_surfaces", "Impervious surface land cover",
        "Share of the 800 m buffer classified as impervious surface",
        "share of buffer", "800 m buffer", "2020", paste(DRCOG, "regional land cover")),
      V("env_pm25", "Air quality: fine particle pollution (PM2.5)",
        "Modelled annual mean fine particulate concentration",
        "micrograms/cu m", "block group", "2022", "Colorado EnviroScreen v2"),
      V("env_npl_proximity", "Proximity to Superfund (NPL) sites",
        "Proximity index rising with the number of National Priorities List sites nearby and falling with distance from them; not a distance",
        "index", "block group", "2022", "Colorado EnviroScreen v2")
    )),

  "LAND USE AND REGULATION VARIABLES" = list(
    "_" = rbind(
      V("zone_category", "Zoning category",
        "Generalized zoning at the parcel: residential low density (reference), residential medium-to-high density, mixed use, nonresidential",
        "4 categories", "parcel", "2023", "Harmonized Denver MSA parcel zoning"),
      V("zone_adu_yes", "ADUs permitted",
        "Whether accessory dwelling units are permitted in the parcel's zone",
        "0/1", "parcel", "2023", "Harmonized Denver MSA parcel zoning"),
      V("pfa_share_800", "Pedestrian focus area share",
        "Share of the 800 m buffer inside a designated pedestrian focus area",
        "share of buffer", "800 m buffer", "2023", paste(DRCOG, "pedestrian focus areas"))
    ))
)

# -----------------------------------------------------------------------------
# Check the registry against the models before printing anything
# -----------------------------------------------------------------------------
described <- unlist(lapply(SECTIONS, function(sec)
  unlist(lapply(sec, function(tbl) tbl$name))), use.names = FALSE)

# zone_category is a factor and reaches the models through domain_z rather than
# domain_vars, so both lists are consulted before deciding a variable is unused.
modelled <- unique(c(obj$context_vars, unlist(obj$domain_vars), unlist(obj$domain_z)))
modelled <- unique(sub('_z$', '', modelled))
missing  <- setdiff(modelled, described)
if (length(missing)) {
  stop("24_variable_table.R: these variables enter a model but are not described:\n  ",
       paste(missing, collapse = ", "))
}
extra <- setdiff(described, c(modelled, "swb_index", "belonging_index"))
if (length(extra)) {
  message("24_variable_table.R: described but not in any model (fine if intentional): ",
          paste(extra, collapse = ", "))
}

n_obs <- function(v) {
  if (!v %in% names(dat)) return(NA_integer_)
  x <- dat[[v]]
  if (is.factor(x) || is.character(x)) sum(!is.na(x)) else sum(!is.na(to_num(x)))
}

# -----------------------------------------------------------------------------
# Lay the table out
# -----------------------------------------------------------------------------
rows <- list()
add <- function(...) rows[[length(rows) + 1]] <<- c(...)

add("Variable", "Definition", "Unit", "Geography", "Vintage", "Source", "N")
for (sec_name in names(SECTIONS)) {
  add(sec_name, "", "", "", "", "", "")
  for (cat_name in names(SECTIONS[[sec_name]])) {
    if (cat_name != "_") add(cat_name, "", "", "", "", "", "")
    tbl <- SECTIONS[[sec_name]][[cat_name]]
    for (i in seq_len(nrow(tbl))) {
      r <- tbl[i, ]
      add(r$label, r$definition, r$unit, r$geo, r$vintage, r$source,
          as.character(n_obs(r$name)))
    }
  }
}
OUT <- do.call(rbind, lapply(rows, function(r) as.data.frame(t(r), stringsAsFactors = FALSE)))
names(OUT) <- OUT[1, ]
OUT <- OUT[-1, ]
readr::write_csv(OUT, file.path(out_dir, "variable_table.csv"), col_names = TRUE)

audit <- do.call(rbind, lapply(names(SECTIONS), function(sn)
  do.call(rbind, lapply(names(SECTIONS[[sn]]), function(cn) {
    tbl <- SECTIONS[[sn]][[cn]]
    data.frame(section = sn, category = cn, variable = tbl$name,
               label = tbl$label, n_observed = vapply(tbl$name, n_obs, integer(1)),
               in_model = tbl$name %in% modelled, stringsAsFactors = FALSE)
  }))))
readr::write_csv(audit, file.path(out_dir, "variable_table_check.csv"))

cat("\n24_variable_table.R: described", nrow(audit), "variables in",
    length(SECTIONS), "sections\n")
cat("  every modelled variable is described:", length(missing) == 0, "\n")
cat("  coverage ranges from", min(audit$n_observed, na.rm = TRUE), "to",
    max(audit$n_observed, na.rm = TRUE), "respondents\n")
cat("  ->", file.path(out_dir, "variable_table.csv"), "\n")
