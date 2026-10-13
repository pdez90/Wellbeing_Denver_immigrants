# =============================================================================
# 25_verify_rebuild.R
# Does a rebuilt pipeline reproduce the numbers the manuscript reports?
#
#   INPUT : the rebuilt analysis file (via 00_config.R), plus whichever of
#           formal_mediation_results_domain_models.csv and
#           mediation_sensitivity.csv the rebuild has produced
#           reference/ref_*.csv  -- the published values, shipped in the repo
#   OUTPUT: rebuild_verification.csv  and a printed report
#
# WHY THIS EXISTS
#
# The analysis was rebuilt on a new machine after the original working copy was
# lost. The survey is byte-identical to the original, but every third-party
# geospatial input had to be re-downloaded, and several of those sources have
# since been revised: the Urban
# Centers layer no longer publishes the 2019 vintage, American Forests replaced
# Tree Equity Score 1.0 with 2.0, CNT withdrew the 2019 H+T release, and MODIS
# retired collection 6.0 in favour of 6.1.
#
# A re-download therefore does not guarantee the same data. This script exists
# to make any substitution visible rather than silent.
#
# THE ORDER OF CHECKS IS DELIBERATE
#
# Check 1 compares the DESCRIPTIVE STATISTICS of each built-environment variable
# against the published Table 2. This runs before any model is fit, and it is
# the sensitive test: if a data source changed vintage, the variable's mean,
# SD and range move immediately, whereas a coefficient can absorb a moderate
# change in an input and still look plausible. A drift flagged here is a data
# problem; a coefficient that moves while the descriptives match is a modelling
# problem. Distinguishing those two is the whole point of running this first.
#
# Check 2 compares the mediation table, which is where the paper's claims live.
# Check 3 compares the sensitivity thresholds.
#
# WHAT A FAILURE MEANS
#
# Nothing here is a pass/fail gate on the science. A flagged row means the
# rebuilt input differs from the one the manuscript describes, and the correct
# response is to find out which vintage you have and say so in the paper, not
# to widen the tolerance until the check passes.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
if (!exists(".wb_domains_loaded")) source("wb_domains.R")
if (!exists("WB_LABELS")) source("wb_labels.R")
wb_require(c("tidyverse", "readr"))

ref_dir <- file.path(dirname(getwd()), "reference")
if (!dir.exists(ref_dir)) ref_dir <- file.path(getwd(), "..", "reference")
if (!dir.exists(ref_dir)) stop(
  "Cannot find the reference/ folder. It ships in the repository alongside\n",
  "scripts/. Run this from the scripts/ directory.")

ref_path <- function(f) file.path(ref_dir, f)

# Tolerances. Means are compared as a fraction of the published standard
# deviation rather than of the published mean: several variables are centred at
# or near zero, and a relative-to-mean test on those is meaningless -- the
# neighborhood SES index has a published mean of 0.02 and an SD of 0.90, so a
# shift of 0.001 would read as a 7 per cent error. Spreads and sample sizes are
# compared in relative terms, and effects in absolute terms because they are
# already standardised.
TOL_DESC_REL <- 0.02     # 2 per cent, for spreads and sample sizes
TOL_DESC_SD  <- 0.05     # 5 per cent of a standard deviation, for means
TOL_EFFECT   <- 0.02     # 0.02 SD on an ACME or a coefficient
TOL_RHO      <- 0.03
TOL_P        <- 0.02     # bootstrap p-values move with the resampling; 0.02 is the resolution of 1,000 draws x 20

report <- list()
# A published figure carries only the precision it was printed with. The
# reference tables are read as character so that precision is taken from the
# printed string ("0.10" has two decimals; parsing it to 0.1 would lose one),
# and a rebuilt value that rounds to the published figure at that precision is
# an exact reproduction. The tolerance test below is applied only to values
# that do not reproduce exactly, and the verdict records which case applied.
printed_dp <- function(s) {
  s <- trimws(as.character(s))
  if (grepl("\\.", s)) nchar(sub(".*\\.", "", s)) else 0L
}
num <- function(s) suppressWarnings(as.numeric(as.character(s)))
add <- function(check, item, published, rebuilt, tol, kind, scale = NA_real_) {
  pub_dp  <- printed_dp(published)
  pub     <- num(published)
  scale   <- num(scale)
  verdict <- if (is.na(pub) || is.na(rebuilt)) "MISSING" else {
    exact <- abs(round(rebuilt, pub_dp) - pub) < 1e-9
    within <- if (kind == "rel")      abs(rebuilt - pub) <= tol * max(abs(pub), 1e-9)
              else if (kind == "sd")  abs(rebuilt - pub) <= tol * scale
              else                    abs(rebuilt - pub) <= tol
    if (exact) "match" else if (isTRUE(within)) "within tolerance" else "DRIFT"
  }
  report[[length(report) + 1]] <<- data.frame(
    check = check, item = item, published = pub, rebuilt = rebuilt,
    difference = rebuilt - pub, verdict = verdict, stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# The variables in published Table 2, keyed by the name they carry in the data.
# The label strings must match reference/ref_T2_*.csv exactly.
# -----------------------------------------------------------------------------
T2_MAP <- c(
  pop_density                 = "Population density",
  housing_density             = "Housing density",
  dist_downtown_km            = "Distance to downtown Denver, km",
  pct_poverty                 = "Percent below poverty",
  pct_non_native              = "Percent foreign-born",
  neighborhood_ses_index      = "Neighborhood SES index",
  walk_nat_walk_ind           = "EPA National Walkability Index",
  street_intdensity           = "Street intersection density (per sq mile)",
  urban_center_nearest_dist_m = "Distance to nearest urban center, m",
  hudjob_jobs_idx             = "HUD Jobs Proximity Index",
  sidewalk_density_800        = "Sidewalk density within 800 m",
  bike_facility_density_800   = "Bicycle facility density within 800 m",
  active_corridor_density_800 = "Active transportation corridor density within 800 m",
  ht_t_ami                    = "Transportation cost index",
  tree_tes                    = "Tree Equity Score",
  tree_treecanopy             = "Tree canopy share",
  park_acres_half_mile        = "Park acres within 800 m",
  park_nearest_dist_m         = "Distance to nearest park, m",
  lc_800m_tree_canopy         = "Tree canopy land cover within 800 m",
  lc_800m_impervious_surfaces = "Impervious surface within 800 m",
  crash_density_800           = "Crash density within 800 m",
  ped_crash_density_800       = "Pedestrian crash density within 800 m",
  bike_crash_density_800      = "Bicycle crash density within 800 m",
  short_trip_zone_share_800   = "Short-trip opportunity zone share within 800 m",
  div_total_diversity_resi    = "Residential diversity index",
  div_exposure_mean           = "Experienced diversity index")

# Which upstream source each variable comes from, so a drift points at a file
# rather than at a number. Only the ones with a known re-download risk are
# annotated; the rest are left blank.
AT_RISK <- c(
  urban_center_nearest_dist_m = "DRCOG Urban Centers -- 2019 vintage no longer published",
  bike_facility_density_800   = "DRCOG Bicycle Facility Inventory -- undated, continuously updated",
  park_acres_half_mile        = "DRCOG Parks and Open Space -- 2024 vintage unconfirmed",
  park_nearest_dist_m         = "DRCOG Parks and Open Space -- 2024 vintage unconfirmed",
  tree_tes                    = "Tree Equity Score -- 1.0 replaced by 2.0, Colorado biome targets changed",
  tree_treecanopy             = "Tree Equity Score -- 1.0 replaced by 2.0",
  ht_t_ami                    = "CNT H+T -- 2019 release withdrawn, current release uses 2022 ACS",
  hudjob_jobs_idx             = "HUD Jobs Proximity -- two block-group layers now published",
  street_intdensity           = "ICPSR 38580 -- now at V3",
  ndvi_800                    = "MODIS -- collection 6.0 decommissioned, 6.1 only")

# -----------------------------------------------------------------------------
# CHECK 1  Descriptive statistics of the built environment
# -----------------------------------------------------------------------------
cat("\n=== Check 1: built-environment descriptives against published Table 2 ===\n")
cat("This runs before any model. A drift here is a changed input, not a changed\n")
cat("estimate, and it is the reason to look at the source rather than the model.\n\n")

t2 <- readr::read_csv(ref_path("ref_T2_built_environment_descriptives.csv"),
                      show_col_types = FALSE, col_types = readr::cols(.default = "c"))
dat <- readr::read_csv(analysis_file, show_col_types = FALSE)

for (v in names(T2_MAP)) {
  lab <- T2_MAP[[v]]
  rw  <- t2[t2$Variable == lab, ]
  if (!nrow(rw)) { cat("  (no published row for", lab, ")\n"); next }
  if (!v %in% names(dat)) {
    add("descriptives", paste0(lab, " [mean]"), rw$Mean[1], NA_real_, TOL_DESC_REL, "rel")
    next
  }
  x <- suppressWarnings(as.numeric(dat[[v]]))
  add("descriptives", paste0(lab, " [n]"),    rw$N[1],    sum(!is.na(x)),        0.02, "rel")
  add("descriptives", paste0(lab, " [mean]"), rw$Mean[1], mean(x, na.rm = TRUE),
      TOL_DESC_SD, "sd", scale = rw$SD[1])
  add("descriptives", paste0(lab, " [sd]"),   rw$SD[1],   sd(x,   na.rm = TRUE), TOL_DESC_REL, "rel")
}

# -----------------------------------------------------------------------------
# CHECK 2  The mediation table
# -----------------------------------------------------------------------------
cat("=== Check 2: mediation results against published Table 9 ===\n\n")
med_new <- file.path(out_dir, "formal_mediation_results_domain_models.csv")
if (file.exists(med_new)) {
  ref9 <- readr::read_csv(ref_path("ref_T9_mediation.csv"), show_col_types = FALSE,
                          col_types = readr::cols(.default = "c"))
  got  <- readr::read_csv(med_new, show_col_types = FALSE)
  got$label <- vapply(got$predictor, function(p) {
    k <- paste0(p, "_z"); if (!is.na(WB_LABELS[k])) unname(WB_LABELS[k])
    else if (!is.na(WB_LABELS[p])) unname(WB_LABELS[p]) else p
  }, character(1))
  for (i in seq_len(nrow(ref9))) {
    lab <- ref9$Predictor[i]
    g <- got[got$label == lab, ]
    add("mediation", paste0(lab, " [ACME]"), ref9$`Indirect effect (ACME)`[i],
        if (nrow(g)) g$acme[1] else NA_real_, TOL_EFFECT, "abs")
    add("mediation", paste0(lab, " [n]"), ref9$N[i],
        if (nrow(g)) g$n[1] else NA_real_, 0.02, "rel")
    add("mediation", paste0(lab, " [ADE]"), ref9$`Direct effect (ADE)`[i],
        if (nrow(g)) g$ade[1] else NA_real_, TOL_EFFECT, "abs")
    add("mediation", paste0(lab, " [total]"), ref9$`Total effect`[i],
        if (nrow(g)) g$total_effect[1] else NA_real_, TOL_EFFECT, "abs")
    add("mediation", paste0(lab, " [ACME p]"), ref9$`ACME p`[i],
        if (nrow(g)) g$acme_p[1] else NA_real_, TOL_P, "abs")
    add("mediation", paste0(lab, " [ACME p Holm]"), ref9$`ACME p (Holm)`[i],
        if (nrow(g)) g$acme_p_holm[1] else NA_real_, TOL_P, "abs")
  }
} else {
  cat("  formal_mediation_results_domain_models.csv not found -- run 09 first.\n\n")
  ref9 <- readr::read_csv(ref_path("ref_T9_mediation.csv"), show_col_types = FALSE,
                          col_types = readr::cols(.default = "c"))
  for (i in seq_len(nrow(ref9)))
    add("mediation", paste0(ref9$Predictor[i], " [ACME]"), ref9$`Indirect effect (ACME)`[i], NA_real_, TOL_EFFECT, "abs")
}

# -----------------------------------------------------------------------------
# CHECK 3  The sensitivity thresholds
# -----------------------------------------------------------------------------
cat("=== Check 3: mediation sensitivity (rho*) ===\n\n")
sens_new <- file.path(out_dir, "mediation_sensitivity.csv")
if (file.exists(sens_new)) {
  refs <- readr::read_csv(ref_path("ref_mediation_sensitivity.csv"), show_col_types = FALSE,
                          col_types = readr::cols(.default = "c"))
  gots <- readr::read_csv(sens_new, show_col_types = FALSE)
  for (i in seq_len(nrow(refs))) {
    g <- gots[gots$label == refs$Exposure[i], ]
    add("sensitivity", paste0(refs$Exposure[i], " [rho*]"), refs$`rho*`[i],
        if (nrow(g)) g$rho_star[1] else NA_real_, TOL_RHO, "abs")
  }
} else {
  cat("  mediation_sensitivity.csv not found -- run 22 first.\n\n")
  refs <- readr::read_csv(ref_path("ref_mediation_sensitivity.csv"), show_col_types = FALSE,
                          col_types = readr::cols(.default = "c"))
  for (i in seq_len(nrow(refs)))
    add("sensitivity", paste0(refs$Exposure[i], " [rho*]"), refs$`rho*`[i], NA_real_, TOL_RHO, "abs")
}

# -----------------------------------------------------------------------------
# Report
# -----------------------------------------------------------------------------
R <- do.call(rbind, report)
readr::write_csv(R, file.path(out_dir, "rebuild_verification.csv"))

drift   <- R[R$verdict == "DRIFT", ]
missing <- R[R$verdict == "MISSING", ]

cat("\n========================================================================\n")
cat("VERIFICATION SUMMARY\n")
cat("========================================================================\n")
within <- R[R$verdict == "within tolerance", ]
cat(sprintf("  %d checks: %d reproduce the published figure exactly, %d within tolerance, %d drift, %d missing\n",
            nrow(R), sum(R$verdict == "match"), nrow(within), nrow(drift), nrow(missing)))
if (nrow(within)) {
  cat("\n-- Within tolerance but not an exact reproduction ------------------------\n")
  w <- within
  w$published <- sprintf("%.4g", w$published); w$rebuilt <- sprintf("%.4g", w$rebuilt)
  print(w[, c("check", "item", "published", "rebuilt")], row.names = FALSE)
}

if (nrow(drift)) {
  cat("\n-- Drifted --------------------------------------------------------------\n")
  d <- drift
  d$published <- sprintf("%.4g", d$published)
  d$rebuilt   <- sprintf("%.4g", d$rebuilt)
  d$difference <- sprintf("%+.4g", as.numeric(drift$difference))
  print(d[, c("check", "item", "published", "rebuilt", "difference")], row.names = FALSE)

  hit <- unique(unlist(lapply(names(AT_RISK), function(v) {
    lab <- T2_MAP[v]
    if (is.na(lab)) return(NULL)          # e.g. ndvi_800, not in Table 2
    if (any(grepl(lab, drift$item, fixed = TRUE))) v else NULL
  })))
  if (length(hit)) {
    cat("\n  Of these, the following have a KNOWN upstream vintage change.\n")
    cat("  Drift in them is expected, and is a data-provenance question:\n\n")
    for (v in hit) cat("    ", T2_MAP[[v]], "\n       ", AT_RISK[[v]], "\n", sep = "")
    cat("\n")
  }
  unexplained <- setdiff(unique(sub(" \\[.*", "", drift$item)),
                         unname(T2_MAP[names(AT_RISK)]))
  if (length(unexplained)) {
    cat("\n  These drifted WITHOUT a known upstream change, which is the more\n")
    cat("  worrying case -- it suggests the rebuild differs in processing:\n\n")
    for (u in unexplained) cat("    ", u, "\n", sep = "")
  }
}

if (nrow(missing)) {
  cat("\n-- Not computed in this rebuild -----------------------------------------\n")
  cat(paste("   ", unique(missing$item), collapse = "\n"), "\n")
}

if (!nrow(drift) && !nrow(missing) && !nrow(within)) {
  cat("\n  Every check reproduces the published figure exactly.\n")
} else if (!nrow(drift) && !nrow(missing)) {
  cat("\n  No drift and nothing missing; see the within-tolerance list above.\n")
}

# A verification that found drift or could not run a check is a failure, so
# that a pipeline calling this script cannot report success by accident.
if (nrow(drift) || nrow(missing)) {
  if (interactive()) stop("verification failed: ", nrow(drift), " drift, ", nrow(missing), " missing", call. = FALSE)
  quit(save = "no", status = 1)
}

cat("\nWrote rebuild_verification.csv\n")
cat("\nA reminder on how to read this. Matching descriptives with a moved\n")
cat("coefficient points at the modelling code. Moved descriptives point at the\n")
cat("input file, and the fix is to identify which vintage you downloaded and\n")
cat("report it -- not to loosen the tolerance.\n")
