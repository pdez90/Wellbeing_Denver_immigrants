# =============================================================================
# wb_domains.R
# The variable categories, in one place.
#
# Sourced by 08, 09, 11, 13, 14, 15, 16, 17 and 19. Before this file existed,
# each of those scripts carried its own copy of the domain lists, so a change of
# category meant eight edits and eight chances to disagree. Change a category
# here and the models, the tables, the figures and the appendix all move
# together.
#
# The structure below is the one agreed by the co-authors on the variable
# one-pager (Sep 2026):
#
#   CONTROLS            neighborhood demographics, entered in every model
#   1 density_location  density, location and urban form
#   2 access_transport  transportation and accessibility
#   3 green_space       green space
#   4 safety_env        safety and environmental quality
#   5 land_use          land use and regulation
#
# What moved, relative to the earlier structure:
#   - population density, housing density and distance to downtown were
#     controls; they are now built-environment measures in domain 1.
#   - residential and experienced diversity were in the safety domain; they are
#     now controls.
#   - the short-trip opportunity zone share was in the safety domain; it is now
#     in domain 1.
#   - impervious surface land cover was in the greenness domain; it is now in
#     the safety and environmental quality domain.
#   - new variables: share of multifamily housing and percent non-Hispanic
#     white (both ACS), NDVI, PM2.5 and proximity to National Priorities List
#     (Superfund) sites.
# =============================================================================

# -----------------------------------------------------------------------------
# Controls: neighborhood demographics, in every model, never a domain
# -----------------------------------------------------------------------------
WB_CONTROL_VARS <- c(
  "pct_poverty",              # ACS 2015-2019, block group
  "pct_non_native",           # ACS 2015-2019, tract
  "neighborhood_ses_index",   # composite of income, rent, home value
  "pct_nh_white",             # NEW: ACS 2015-2019 B03002, block group
  "div_total_diversity_resi", # Xu et al. 2024, tract
  "div_exposure_mean"         # Xu et al. 2024, tract, averaged over intervals
)

# -----------------------------------------------------------------------------
# The five built-environment domains
# -----------------------------------------------------------------------------
WB_DOMAIN_VARS <- list(
  density_location = c(
    "pop_density",
    "housing_density",
    "pct_multifamily",              # NEW: ACS 2015-2019 B25024, block group
    "dist_downtown_km",
    "urban_center_nearest_dist_m",
    "walk_nat_walk_ind",
    "street_intdensity",
    "short_trip_zone_share_800"
  ),
  access_transport = c(
    "hudjob_jobs_idx",
    "sidewalk_density_800",
    "bike_facility_density_800",
    "active_corridor_density_800",
    "ht_t_ami"
  ),
  green_space = c(
    "tree_tes",
    "tree_treecanopy",
    "park_acres_half_mile",
    "park_nearest_dist_m",
    "lc_800m_tree_canopy",
    "ndvi_800"                      # NEW: 21_ndvi.R
  ),
  safety_env = c(
    "crash_density_800",
    "ped_crash_density_800",
    "bike_crash_density_800",
    "lc_800m_impervious_surfaces",
    "env_pm25",                     # NEW: EnviroScreen fine particle pollution
    "env_npl_proximity"             # NEW: EnviroScreen NPL site proximity
  ),
  land_use = c(
    "zone_adu_yes",
    "pfa_share_800"
  )
)

WB_DOMAIN_LABELS <- c(
  density_location = "Density, location and urban form",
  access_transport = "Transportation and accessibility",
  green_space      = "Green space",
  safety_env       = "Safety and environmental quality",
  land_use         = "Land use and regulation"
)

# Short forms, for figure panels and table headers where the full label does not
# fit on one line.
WB_DOMAIN_LABELS_SHORT <- c(
  density_location = "Density and location",
  access_transport = "Transportation",
  green_space      = "Green space",
  safety_env       = "Safety and environment",
  land_use         = "Land use"
)

# -----------------------------------------------------------------------------
# The integrated model: which term represents each domain
# -----------------------------------------------------------------------------
# This used to be a hand-written list of seven terms, chosen after reading the
# domain results. That is a defensible way to build a synthesis model but it is
# not reproducible: nothing in the code said how the seven were picked, so a
# re-run with different categories could not repeat the choice.
#
# The rule below states it. From each domain, take the term with the smallest
# p-value across the two domain models (well-being and belonging), keeping it
# only if that p-value is below `WB_FINAL_ALPHA`. Zoning is excluded because it
# is a four-category factor and cannot be summarised by one coefficient.
#
# `select_final_be()` returns the selected terms and prints what it chose, so
# the integrated model in the paper can be traced to the domain models.

WB_FINAL_ALPHA <- 0.10

select_final_be <- function(model_dat, individual_controls, context_z, domain_z,
                            alpha = WB_FINAL_ALPHA, verbose = TRUE) {
  picked <- character(0)
  report <- list()
  for (d in names(domain_z)) {
    terms <- setdiff(domain_z[[d]], "zone_category")
    if (!length(terms)) next
    rhs <- unique(c(individual_controls, context_z, domain_z[[d]]))
    best_p <- rep(NA_real_, length(terms)); names(best_p) <- terms
    for (outcome in c("swb_z", "belonging_z")) {
      m <- make_lm(outcome, rhs, model_dat)
      s <- summary(m)$coefficients
      for (t in terms) {
        if (t %in% rownames(s)) {
          best_p[[t]] <- min(best_p[[t]], s[t, 4], na.rm = TRUE)
        }
      }
    }
    if (all(is.na(best_p))) next
    winner <- names(best_p)[which.min(best_p)]
    report[[d]] <- data.frame(domain = d, term = winner, p = best_p[[winner]],
                              kept = best_p[[winner]] < alpha,
                              stringsAsFactors = FALSE)
    if (best_p[[winner]] < alpha) picked <- c(picked, winner)
  }
  rep_df <- do.call(rbind, report)
  if (verbose && !is.null(rep_df)) {
    cat("\nIntegrated model: strongest term per domain (p < ",
        format(alpha), " to be kept)\n", sep = "")
    print(rep_df, row.names = FALSE)
  }
  attr(picked, "report") <- rep_df
  picked
}

# -----------------------------------------------------------------------------
# Mediation
# -----------------------------------------------------------------------------
# Every term that enters the integrated model, plus the two regulatory measures,
# which are of interest whether or not they reach significance in their domain.
# 09_mediation.R applies a Holm correction across however many are tested.
wb_mediation_candidates <- function(final_be, model_dat) {
  cand <- unique(c(final_be, "pfa_share_800_z", "zone_adu_yes"))
  cand[cand %in% names(model_dat)]
}

# Which domain a term belongs to, for labelling a mediation or coefficient row.
wb_domain_of <- function(term) {
  bare <- sub("_z$", "", term)
  for (d in names(WB_DOMAIN_VARS)) {
    if (bare %in% WB_DOMAIN_VARS[[d]] || term %in% WB_DOMAIN_VARS[[d]]) return(d)
  }
  NA_character_
}

.wb_domains_loaded <- TRUE
