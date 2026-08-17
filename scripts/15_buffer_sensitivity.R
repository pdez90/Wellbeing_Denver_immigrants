# =============================================================================
# 15_buffer_sensitivity.R
# Does the choice of buffer radius drive the results?
#
#   INPUT : model_objects.rds  (from 08_domain_models.R)
#   OUTPUT: buffer_sensitivity.csv          every coefficient at all three radii
#           buffer_sensitivity_wide.csv     the table as it appears in the SI
#
# Buffer-derived exposures were computed at 400 m, 800 m and 1,600 m around each
# residence. The paper reports the 800 m radius, roughly a ten-minute walk. This
# script refits the three domains that contain buffer measures -- transportation,
# greenness, and safety -- plus the pedestrian focus area share, substituting the
# 400 m and 1,600 m versions of each measure and changing nothing else.
#
# Two design choices matter for interpretation:
#
#   1. The analytic sample is FIXED at the respondents who are complete on the
#      800 m specification. Radius is then the only thing that varies, so a
#      coefficient that moves is telling you about scale rather than about which
#      respondents happen to be included.
#   2. Measures that have no radius -- park acreage within half a mile, distance
#      to the nearest park, the Tree Equity measures, the jobs index, transport
#      cost, the diversity indices -- are held at their single available version
#      in every model. They are covariates here, not the object of the test.
#
# Densities at 400 m and 1,600 m are built the same way script 04 builds the
# 800 m ones: facility length or crash count divided by the buffer area.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
if (!exists("WB_LABELS")) source("wb_labels.R")
wb_require(c("tidyverse", "readr", "broom"))

obj <- readRDS(file.path(out_dir, "model_objects.rds"))
if (is.null(obj$dat_derived)) {
  stop("model_objects.rds predates this script. Re-run 08_domain_models.R.")
}
dat <- obj$dat_derived
individual_controls <- obj$individual_controls
context_z           <- obj$context_z

RADII <- c(400, 800, 1600)
area_km2 <- function(r) pi * (r / 1000)^2

# -----------------------------------------------------------------------------
# 1. Build the radius-specific exposures
# -----------------------------------------------------------------------------
# Each entry is one exposure, with the source column pattern it is built from and
# how to turn that source into the modelled quantity. `{r}` is the radius.

EXPOSURES <- list(
  sidewalk_density        = list(src = "sidewalk_length_m_{r}",        how = "per_area"),
  bike_facility_density   = list(src = "bike_facility_length_m_{r}",   how = "per_area"),
  active_corridor_density = list(src = "active_corridor_length_m_{r}", how = "per_area"),
  crash_density           = list(src = "crash_total_{r}",              how = "per_area"),
  ped_crash_density       = list(src = "crash_ped_{r}",                how = "per_area"),
  bike_crash_density      = list(src = "crash_bike_{r}",               how = "per_area"),
  lc_tree_canopy          = list(src = "lc_{r}m_tree_canopy",          how = "as_is"),
  lc_impervious           = list(src = "lc_{r}m_impervious_surfaces",  how = "as_is"),
  short_trip_zone_share   = list(src = "short_trip_zone_share_{r}",    how = "as_is"),
  pfa_share               = list(src = "pfa_share_{r}",                how = "as_is")
)

build_radius <- function(r) {
  out <- list()
  for (nm in names(EXPOSURES)) {
    spec <- EXPOSURES[[nm]]
    col  <- gsub("\\{r\\}", r, spec$src)
    if (!col %in% names(dat)) {
      stop("15_buffer_sensitivity.R: column '", col, "' is missing, so the ",
           r, " m buffer cannot be built. Re-run scripts 02-05.")
    }
    x <- to_num(dat[[col]])
    out[[nm]] <- if (spec$how == "per_area") x / area_km2(r) else x
  }
  as.data.frame(out)
}

RAD <- lapply(stats::setNames(RADII, RADII), build_radius)

# Sanity check: the 800 m densities rebuilt here must match the ones script 04
# wrote, or the comparison across radii is not like for like.
CHECKS <- c(sidewalk_density = "sidewalk_density_800",
            bike_facility_density = "bike_facility_density_800",
            crash_density = "crash_density_800")
for (i in seq_along(CHECKS)) {
  nm  <- names(CHECKS)[i]
  stored <- to_num(dat[[CHECKS[[i]]]]); rebuilt <- RAD[["800"]][[nm]]
  ok  <- stats::complete.cases(stored, rebuilt)
  if (sum(ok) > 0) {
    worst <- max(abs(stored[ok] - rebuilt[ok]) / pmax(1e-9, abs(stored[ok])))
    if (worst > 0.01) {
      stop("15_buffer_sensitivity.R: rebuilding ", nm, " at 800 m does not ",
           "reproduce the stored column (worst relative difference ",
           sprintf("%.3f", worst), "). The density definition has drifted.")
    }
  }
}

# -----------------------------------------------------------------------------
# 2. Specifications
# -----------------------------------------------------------------------------
# `varying` are the exposures swapped across radii; `fixed` are model terms with
# no radius, carried through unchanged.

SPECS <- list(
  access_transport = list(
    label   = "Transportation and accessibility",
    varying = c("sidewalk_density", "bike_facility_density", "active_corridor_density"),
    fixed   = c("hudjob_jobs_idx_z", "ht_t_ami_z"),
    stored  = c("sidewalk_density_800", "bike_facility_density_800",
                "active_corridor_density_800")),
  green_parks = list(
    label   = "Greenness and parks",
    varying = c("lc_tree_canopy", "lc_impervious"),
    fixed   = c("tree_tes_z", "tree_treecanopy_z", "park_acres_half_mile_z",
                "park_nearest_dist_m_z"),
    stored  = c("lc_800m_tree_canopy", "lc_800m_impervious_surfaces")),
  safety_social = list(
    label   = "Safety and social environment",
    varying = c("crash_density", "ped_crash_density", "bike_crash_density",
                "short_trip_zone_share"),
    fixed   = c("div_total_diversity_resi_z", "div_exposure_mean_z"),
    stored  = c("crash_density_800", "ped_crash_density_800", "bike_crash_density_800",
                "short_trip_zone_share_800")),
  land_use = list(
    label   = "Land use and regulation",
    varying = c("pfa_share"),
    fixed   = c("zone_category", "zone_adu_yes"),
    stored  = c("pfa_share_800"))
)

OUTCOMES <- c(swb_z = "Subjective well-being", belonging_z = "Neighborhood belonging")

# -----------------------------------------------------------------------------
# 3. Fit, holding the sample fixed at the 800 m analytic sample
# -----------------------------------------------------------------------------
results <- list()

for (sp_name in names(SPECS)) {
  sp <- SPECS[[sp_name]]

  # frame with every term, at every radius, plus outcomes and covariates
  base <- dat[, unique(c("swb_z", "belonging_z", individual_controls, context_z,
                         sp$fixed[sp$fixed %in% names(dat)],
                         sp$stored[sp$stored %in% names(dat)])), drop = FALSE]
  for (r in RADII) {
    add <- RAD[[as.character(r)]][, sp$varying, drop = FALSE]
    names(add) <- paste0(sp$varying, "_r", r)
    base <- cbind(base, add)
  }

  # z-score each exposure on the full sample, exactly as 08 does
  for (nm in grep("_r(400|800|1600)$", names(base), value = TRUE)) {
    base[[paste0(nm, "_z")]] <- zscore(base[[nm]])
  }

  for (outcome in names(OUTCOMES)) {
    # The sample is the one the corresponding domain model in the SI uses: the
    # respondents complete on that model's own 800 m columns. Fixing it here
    # means a coefficient that moves across radii is telling you about scale,
    # not about a changed set of respondents.
    # Both outcomes are required, not just the one being modelled, because
    # 08_domain_models.R builds its analytic frame that way; without this the
    # samples here would be three or four respondents larger than the domain
    # models they are meant to be compared against.
    rhs_800 <- c(individual_controls, context_z, sp$fixed[sp$fixed %in% names(base)],
                 paste0(sp$varying, "_r800_z"), sp$stored[sp$stored %in% names(base)])
    keep <- stats::complete.cases(base[, c("swb_z", "belonging_z", rhs_800), drop = FALSE])
    n_fixed <- sum(keep)

    for (r in RADII) {
      rhs <- c(individual_controls, context_z, sp$fixed[sp$fixed %in% names(base)],
               paste0(sp$varying, "_r", r, "_z"))
      fit <- stats::lm(stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + "))),
                       data = base[keep, , drop = FALSE])
      td  <- broom::tidy(fit)
      for (v in sp$varying) {
        term <- paste0(v, "_r", r, "_z")
        row  <- td[td$term == term, ]
        if (!nrow(row)) next
        results[[length(results) + 1]] <- data.frame(
          domain = sp$label, exposure = v, outcome = unname(OUTCOMES[[outcome]]),
          radius_m = r, n = n_fixed,
          estimate = row$estimate[1], std_error = row$std.error[1], p_value = row$p.value[1],
          adj_r2 = summary(fit)$adj.r.squared, stringsAsFactors = FALSE)
      }
    }
  }
}

R <- do.call(rbind, results)
readr::write_csv(R, file.path(out_dir, "buffer_sensitivity.csv"))

# -----------------------------------------------------------------------------
# 4. The SI table: one row per exposure and outcome, one column per radius
# -----------------------------------------------------------------------------
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**",
                    ifelse(p < .05, "*", ifelse(p < .10, "+", "")))))

LABEL <- c(sidewalk_density = "Sidewalk density",
           bike_facility_density = "Bicycle facility density",
           active_corridor_density = "Active corridor density",
           crash_density = "Crash density",
           ped_crash_density = "Pedestrian crash density",
           bike_crash_density = "Bicycle crash density",
           lc_tree_canopy = "Tree canopy land cover",
           lc_impervious = "Impervious surface",
           short_trip_zone_share = "Short-trip opportunity zone share",
           pfa_share = "Pedestrian focus area share")

cellfmt <- function(d) sprintf("%.3f%s (%.3f)", d$estimate, stars(d$p_value), d$std_error)

wide <- R %>%
  dplyr::mutate(cell = cellfmt(.), radius = paste0(radius_m, " m")) %>%
  dplyr::select(domain, exposure, outcome, n, radius, cell) %>%
  tidyr::pivot_wider(names_from = radius, values_from = cell) %>%
  dplyr::mutate(Predictor = unname(LABEL[exposure])) %>%
  dplyr::select(Domain = domain, Predictor, Outcome = outcome, N = n,
                `400 m`, `800 m`, `1600 m`) %>%
  dplyr::arrange(factor(Domain, levels = vapply(SPECS, function(x) x$label, character(1))),
                 Outcome, Predictor)

readr::write_csv(wide, file.path(out_dir, "buffer_sensitivity_wide.csv"))

# -----------------------------------------------------------------------------
# 5. What changed?
# -----------------------------------------------------------------------------
flip <- R %>%
  dplyr::group_by(domain, exposure, outcome) %>%
  dplyr::summarise(
    sig_at = paste(radius_m[p_value < .05], collapse = ", "),
    range  = sprintf("%.3f to %.3f", min(estimate), max(estimate)),
    .groups = "drop") %>%
  dplyr::filter(sig_at != "")

cat("\nBuffer sensitivity complete.\n")
cat("  ", nrow(R), "coefficients across", length(RADII), "radii\n")
cat("  Exposures significant at p < .05 in at least one radius:\n")
if (nrow(flip)) {
  for (i in seq_len(nrow(flip))) {
    cat(sprintf("    %-28s %-24s significant at: %-14s estimate range %s\n",
                LABEL[flip$exposure[i]], flip$outcome[i], flip$sig_at[i], flip$range[i]))
  }
} else {
  cat("    none\n")
}
cat("  ->", file.path(out_dir, "buffer_sensitivity_wide.csv"), "\n")
