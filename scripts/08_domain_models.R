# =============================================================================
# 08_domain_models.R
# Scale reliability, base/context models, domain-specific models, final compact
# model, VIF diagnostics. Produces the numbers behind Results 3.1-3.6
# (paper Tables 3-7).
#
#   INPUT : respondents_with_transport_ht_segpoi_stoz_diversity.csv
#   OUTPUT: domain_models_base_context.html / .docx
#           domain_models_swb.html
#           domain_models_belonging.html
#           domain_models_swb_with_belonging.html
#           domain_models_land_use.html
#           domain_models_final_compact.html
#           vif_diagnostics.csv           <-- NEW
#           model_data_domain_models.csv  (analysis frame, for 09_mediation.R)
#
# CHANGES vs. the original Wellbeing.Rmd chunk:
#   1. Adds a fifth domain, `land_use`, using the zoning / pedestrian-focus-area
#      joins that script 02 and 04 already produce. The original chunk built
#      these columns and then never modelled them.
#   2. Actually EXPORTS the domain model tables. The original chunk fit the
#      models but only wrote out the mediation table, so Tables 3-7 had to be
#      read off the console.
#   3. Computes and exports VIFs for every model. Methods claims VIFs were
#      examined; nothing in the original pipeline computed them.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
wb_require(wb_packages_analysis)

dat <- readr::read_csv(analysis_file, show_col_types = FALSE) %>%
  janitor::clean_names()

rename_if_present <- function(df, old, new) {
  if (old %in% names(df) && !(new %in% names(df))) {
    df <- df %>% dplyr::rename("{new}" := dplyr::all_of(old))
  }
  df
}

# -----------------------------------------------------------------------------
# 1. Rename raw Qualtrics columns to readable names
# -----------------------------------------------------------------------------

rename_map <- c(
  year_born = "q3_1", gender = "q3_2", marital = "x2", children = "q3_4",
  hispanic = "q3_5", race = "q3_6", imm_status = "q3_7", income_hh = "q3_10",
  engl_speak = "q3_11_1", edu_level = "q3_14",
  time_live_denver = "q5_9", time_live_hood = "q5_10",
  sat_stand_liv = "q8_1_1", sat_health = "q8_1_2", sat_life_achieve = "q8_1_3",
  feel_safe = "q8_1_4", feel_accepted = "q8_1_5", feel_conf_finance = "q8_1_6",
  hood_belong = "q9_7_1", hood_trust_people = "q9_7_2",
  hood_borrow = "q9_7_3", hood_contacts = "q9_7_4"
)
for (new_name in names(rename_map)) {
  dat <- rename_if_present(dat, rename_map[[new_name]], new_name)
}

# -----------------------------------------------------------------------------
# 2. Outcome scales
# -----------------------------------------------------------------------------

swb_items <- c("sat_stand_liv", "sat_health", "sat_life_achieve",
               "feel_safe", "feel_accepted", "feel_conf_finance")
belonging_items <- c("hood_belong", "hood_trust_people", "hood_borrow", "hood_contacts")

dat <- dat %>%
  dplyr::mutate(
    dplyr::across(dplyr::any_of(c(swb_items, belonging_items)), to_num),
    swb_index       = rowMeans(dplyr::across(dplyr::any_of(swb_items)), na.rm = TRUE),
    belonging_index = rowMeans(dplyr::across(dplyr::any_of(belonging_items)), na.rm = TRUE),
    swb_index       = ifelse(is.nan(swb_index), NA_real_, swb_index),
    belonging_index = ifelse(is.nan(belonging_index), NA_real_, belonging_index),
    swb_z           = zscore(swb_index),
    belonging_z     = zscore(belonging_index),
    age             = survey_year - to_num(year_born)
  )

cat("\n== Cronbach's alpha: SWB ==\n");       print(psych::alpha(dat[, swb_items]))
cat("\n== Cronbach's alpha: Belonging ==\n"); print(psych::alpha(dat[, belonging_items]))

cat(sprintf(
  "\nRespondents: %d total | SWB index available: %d | belonging index available: %d\n",
  nrow(dat), sum(!is.na(dat$swb_index)), sum(!is.na(dat$belonging_index))
))

# -----------------------------------------------------------------------------
# 3. Covariates and domains
# -----------------------------------------------------------------------------

# ---- Demographic controls as stable binary indicators ------------------------
# The categorical demographic variables are collapsed into binary indicators
# computed once on the full sample, rather than entered as factors. Two reasons:
#
#   1. Several categories are very sparse -- race has cells of n = 2, 4 and 5;
#      marital status has cells of 8 and 16; immigration status has cells of 5
#      and 10. Bootstrap resampling in 09_mediation.R can draw a sample in which
#      such a level is empty, which makes lm() fail. That is why the mediation
#      models previously had to drop these controls entirely.
#   2. As factors they spend 18 degrees of freedom, much of it on those sparse
#      cells; as indicators they spend 7. On analytic samples of 238-320 that
#      difference is material.
#
# Collapsing this way lets the domain models and the mediation models use an
# IDENTICAL confounder set on IDENTICAL samples, so Table 9 is directly
# comparable to Tables 4-8.
#
# NOTE: indicator names follow the raw survey response codes. Confirm the
# substantive labels against the survey codebook and rename via
# `demographic_labels` below before publication.

demographic_labels <- c(
  ind_gender_1   = "Gender (category 1)",
  ind_marital_1  = "Marital status (category 1)",
  ind_hispanic_1 = "Hispanic ethnicity",
  ind_race_4     = "Race (modal category)",
  ind_imm_1      = "Immigration status (category 1)",
  ind_imm_2      = "Immigration status (category 2)",
  ind_imm_other  = "Immigration status (other categories)"
)

dat <- dat %>%
  dplyr::mutate(
    ind_gender_1   = as.numeric(to_num(gender)     == 1),
    ind_marital_1  = as.numeric(to_num(marital)    == 1),
    ind_hispanic_1 = as.numeric(to_num(hispanic)   == 1),
    ind_race_4     = as.numeric(to_num(race)       == 4),
    ind_imm_1      = as.numeric(to_num(imm_status) == 1),
    ind_imm_2      = as.numeric(to_num(imm_status) == 2),
    ind_imm_other  = as.numeric(to_num(imm_status) %in% c(3, 4, 5, 6, 7))
  )

cat("\nDemographic indicator coverage:\n")
for (v in names(demographic_labels)) {
  cat(sprintf("  %-16s n = %3d of %d, mean = %.3f\n", v,
              sum(!is.na(dat[[v]])), nrow(dat), mean(dat[[v]], na.rm = TRUE)))
}

individual_controls <- c(
  "age", "children", "income_hh", "edu_level", "engl_speak",
  "time_live_denver", "time_live_hood",
  names(demographic_labels)
)

context_vars <- c("pop_density", "housing_density", "dist_downtown_km",
                  "pct_poverty", "pct_non_native", "neighborhood_ses_index")

domain_vars <- list(
  urban_form = c(
    "walk_nat_walk_ind",            # EPA National Walkability Index
    "street_intdensity",            # NaNDA intersection density (see NOTE on units)
    "urban_center_nearest_dist_m"
  ),
  access_transport = c(
    "hudjob_jobs_idx",
    "sidewalk_density_800",
    "bike_facility_density_800",
    "active_corridor_density_800",
    "ht_t_ami"                      # H+T transportation cost, % of income at AMI
  ),
  green_parks = c(
    "tree_tes",                     # Tree Equity Score
    "tree_treecanopy",              # Tree Equity canopy share  (collinear with tree_tes,
    "park_acres_half_mile",         #   r = .89 -- see VIF output below)
    "park_nearest_dist_m",
    "lc_800m_tree_canopy",          # DRCOG landcover canopy in 800 m buffer
    "lc_800m_impervious_surfaces"
  ),
  safety_social = c(
    "crash_density_800",
    "ped_crash_density_800",
    "bike_crash_density_800",
    "short_trip_zone_share_800",
    "div_total_diversity_resi",
    "div_exposure_mean"
  ),
  # ---- NEW DOMAIN -----------------------------------------------------------
  # Regulatory land use and pedestrian planning designations. Built by scripts
  # 02 (zoning shapefile overlay) and 04 (pedestrian focus areas) and, until
  # now, never used in a model.
  land_use = c("zone_adu_yes", "pfa_share_800")
)

# Zoning is categorical, so it is handled separately from the z-scored numerics.
collapse_zone <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  out[x == "Residential_Low"] <- "Residential, low density"
  out[x %in% c("Residential_Med", "Residential_MedHigh", "Residential_High")] <-
    "Residential, medium-high density"
  out[grepl("^MixedUse", x)] <- "Mixed use"
  out[x %in% c("Commercial", "Industrial", "Civic", "OpenSpace")] <- "Nonresidential"
  factor(out, levels = c("Residential, low density",
                         "Residential, medium-high density",
                         "Mixed use", "Nonresidential"))
}

dat <- dat %>%
  dplyr::mutate(
    zone_category = collapse_zone(zone_gen_zone2),
    zone_adu_yes  = dplyr::case_when(zone_adu == "Yes" ~ 1,
                                     zone_adu == "No"  ~ 0,
                                     TRUE ~ NA_real_)
  )

all_numeric_covars <- unique(c(context_vars, unlist(domain_vars)))

# raw categorical survey columns, kept numeric only so the indicators above can
# be built from them; they do not enter any model directly
raw_categoricals <- c("gender", "marital", "hispanic", "race", "imm_status")

dat <- dat %>%
  dplyr::mutate(
    dplyr::across(dplyr::any_of(c(individual_controls, all_numeric_covars)), to_num)
  ) %>%
  dplyr::mutate(
    dplyr::across(dplyr::any_of(all_numeric_covars), zscore, .names = "{.col}_z")
  )

# zone_adu_yes is a 0/1 indicator -- keep it unstandardised for interpretability.
zname <- function(x) { o <- paste0(x, "_z"); o[o %in% names(dat)] }
context_z <- zname(context_vars)
domain_z  <- lapply(domain_vars, zname)
domain_z$land_use <- c("zone_category", "zone_adu_yes", zname("pfa_share_800"))

individual_controls <- individual_controls[individual_controls %in% names(dat)]

model_vars <- unique(c("swb_z", "belonging_z", individual_controls,
                       context_z, unlist(domain_z)))

model_dat <- dat %>%
  dplyr::select(dplyr::any_of(model_vars)) %>%
  dplyr::filter(!is.na(swb_z), !is.na(belonging_z))

keep_vars <- names(model_dat)[
  vapply(model_dat, function(x) dplyr::n_distinct(x, na.rm = TRUE) > 1, logical(1))
]
model_dat <- model_dat %>% dplyr::select(dplyr::all_of(keep_vars))

individual_controls <- individual_controls[individual_controls %in% names(model_dat)]
context_z <- context_z[context_z %in% names(model_dat)]
domain_z  <- lapply(domain_z, function(x) x[x %in% names(model_dat)])

readr::write_csv(model_dat, file.path(out_dir, "model_data_domain_models.csv"))

# -----------------------------------------------------------------------------
# 4. Fit models
# -----------------------------------------------------------------------------

rhs_base    <- individual_controls
rhs_context <- unique(c(individual_controls, context_z))

base_context_models <- list(
  "SWB: individual controls"          = make_lm("swb_z",       rhs_base,    model_dat),
  "SWB: + neighborhood context"       = make_lm("swb_z",       rhs_context, model_dat),
  "Belonging: individual controls"    = make_lm("belonging_z", rhs_base,    model_dat),
  "Belonging: + neighborhood context" = make_lm("belonging_z", rhs_context, model_dat)
)

domain_models_swb <- domain_models_belonging <- domain_models_swb_with_belonging <- list()

for (d in names(domain_z)) {
  rhs_domain <- unique(c(individual_controls, context_z, domain_z[[d]]))
  domain_models_swb[[paste0("SWB: ", d)]] <-
    make_lm("swb_z", rhs_domain, model_dat)
  domain_models_belonging[[paste0("Belonging: ", d)]] <-
    make_lm("belonging_z", rhs_domain, model_dat)
  domain_models_swb_with_belonging[[paste0("SWB + belonging: ", d)]] <-
    make_lm("swb_z", unique(c("belonging_z", rhs_domain)), model_dat)
}

final_be <- c("walk_nat_walk_ind_z", "bike_facility_density_800_z",
              "park_acres_half_mile_z", "tree_tes_z", "crash_density_800_z",
              "short_trip_zone_share_800_z", "div_exposure_mean_z")
final_be <- final_be[final_be %in% names(model_dat)]

final_models <- list(
  "SWB: final BE model"       = make_lm("swb_z",       unique(c(individual_controls, context_z, final_be)), model_dat),
  "Belonging: final BE model" = make_lm("belonging_z", unique(c(individual_controls, context_z, final_be)), model_dat),
  "SWB: final BE + belonging" = make_lm("swb_z",       unique(c("belonging_z", individual_controls, context_z, final_be)), model_dat)
)

# -----------------------------------------------------------------------------
# 4b. Persist model objects IMMEDIATELY
# -----------------------------------------------------------------------------
# This must happen before any table export. Table writing depends on pandoc,
# which is an external binary and the most likely thing to fail on a machine
# other than the one this was developed on. If it does fail, everything above
# is still on disk and 09_mediation.R can still run.

all_models <- c(base_context_models, domain_models_swb, domain_models_belonging,
                domain_models_swb_with_belonging, final_models)

saveRDS(
  list(model_dat = model_dat, individual_controls = individual_controls,
       context_z = context_z, domain_z = domain_z, final_be = final_be),
  file.path(out_dir, "model_objects.rds")
)
cat("Wrote model_objects.rds\n")

# Plain-text coefficient table for every model. This is the authoritative
# numeric output -- the .html and .docx tables are formatted views of it.
coef_table <- purrr::imap_dfr(all_models, function(m, nm) {
  s <- summary(m)
  broom::tidy(m) %>%
    dplyr::mutate(model = nm, n = length(stats::residuals(m)),
                  r_squared = s$r.squared, adj_r_squared = s$adj.r.squared)
}) %>%
  dplyr::select(model, term, estimate, std.error, statistic, p.value,
                n, r_squared, adj_r_squared)

readr::write_csv(coef_table, file.path(out_dir, "all_model_coefficients.csv"))
cat("Wrote all_model_coefficients.csv (", nrow(coef_table), " rows )\n")

model_fit <- purrr::imap_dfr(all_models, function(m, nm) {
  s <- summary(m)
  tibble::tibble(model = nm, n = length(stats::residuals(m)),
                 r_squared = s$r.squared, adj_r_squared = s$adj.r.squared,
                 n_parameters = length(stats::coef(m)))
})
readr::write_csv(model_fit, file.path(out_dir, "model_fit_summary.csv"))
print(model_fit, n = 30)

# -----------------------------------------------------------------------------
# 5. Joint test for the categorical zoning term
# -----------------------------------------------------------------------------
# A four-level factor spread over three dummy coefficients needs a joint F test,
# not three separate t tests.

cat("\n== Joint significance of zoning category ==\n")
zoning_tests <- purrr::map_dfr(c("swb_z", "belonging_z"), function(outcome) {
  rhs_full <- unique(c(individual_controls, context_z, domain_z$land_use))
  full <- make_lm(outcome, rhs_full, model_dat)
  red  <- make_lm(outcome, setdiff(rhs_full, "zone_category"), model_dat)
  a <- stats::anova(red, full)
  tibble::tibble(outcome = outcome, df = a$Df[2], resid_df = a$Res.Df[2],
                 F = a$`F`[2], p = a$`Pr(>F)`[2])
})
print(zoning_tests)
readr::write_csv(zoning_tests, file.path(out_dir, "zoning_joint_tests.csv"))

# -----------------------------------------------------------------------------
# 6. VIF diagnostics  (NEW -- Methods claims these were examined)
# -----------------------------------------------------------------------------

vif_table <- purrr::imap_dfr(all_models, function(m, nm) {
  v <- try(car::vif(m), silent = TRUE)
  if (inherits(v, "try-error")) return(tibble::tibble(model = nm, term = NA_character_, gvif = NA_real_, df = NA_real_))
  # car::vif returns a matrix (GVIF, Df, GVIF^(1/2Df)) when factors are present
  if (is.matrix(v)) {
    tibble::tibble(model = nm, term = rownames(v), gvif = v[, "GVIF"], df = v[, "Df"])
  } else {
    tibble::tibble(model = nm, term = names(v), gvif = as.numeric(v), df = 1)
  }
}) %>%
  # IMPORTANT: raw GVIF is NOT comparable to the usual threshold of 10 when a
  # term has more than one degree of freedom -- a 7-level factor inflates GVIF
  # mechanically. The comparable quantity is GVIF^(1/(2*df)), squared to put it
  # back on the familiar VIF scale. For single-df terms the two are identical.
  dplyr::mutate(
    vif_comparable = gvif^(1 / df),
    flag = dplyr::case_when(vif_comparable > 10 ~ "EXCEEDS 10",
                            vif_comparable > 5  ~ "above 5",
                            TRUE ~ "")
  )

readr::write_csv(vif_table, file.path(out_dir, "vif_diagnostics.csv"))

cat("\n== Terms with comparable VIF > 5 ==\n")
cat("   (single-df terms: ordinary VIF. Multi-df factors: GVIF^(1/df),\n",
    "    which is what should be compared against the threshold.)\n\n")
print(
  vif_table %>%
    dplyr::filter(vif_comparable > 5) %>%
    dplyr::arrange(dplyr::desc(vif_comparable)) %>%
    dplyr::select(model, term, df, gvif, vif_comparable, flag),
  n = 40
)

cat("\n== Correlations among greenness measures ==\n")
green_z <- intersect(c("tree_tes_z", "tree_treecanopy_z", "lc_800m_tree_canopy_z",
                       "lc_800m_impervious_surfaces_z", "park_acres_half_mile_z"),
                     names(model_dat))
print(round(cor(model_dat[, green_z], use = "pairwise.complete.obs"), 3))

# -----------------------------------------------------------------------------
# 7. Export model tables
# -----------------------------------------------------------------------------

gof <- c("nobs", "r.squared", "adj.r.squared")

# Formatted tables are a convenience, not the source of truth -- the numbers
# already live in all_model_coefficients.csv. So every export is wrapped: a
# missing pandoc or a rendering backend quirk warns and moves on rather than
# taking down the script.
# Word and CSV tables are written with officer/flextable, which generate the
# document XML directly from R. modelsummary's .docx path shells out to pandoc,
# an external binary that R launched from a terminal does not inherit from
# RStudio -- so the Word tables are built here instead, with no dependency
# beyond packages the pipeline already requires. modelsummary still produces
# the .html version, which does not need pandoc.
source("wb_model_tables.R")

export_models <- function(models, stem, title) {

  # 1. CSV and DOCX via officer -- no external binary involved.
  wb_write_model_csv(models, file.path(out_dir, paste0(stem, ".csv")))
  cat("Wrote", paste0(stem, ".csv"), "\n")

  ok <- tryCatch({
    wb_write_model_docx(models, file.path(out_dir, paste0(stem, ".docx")), title)
    TRUE
  }, error = function(e) {
    message("  could not write ", stem, ".docx: ", conditionMessage(e)); FALSE
  })
  if (ok) cat("Wrote", paste0(stem, ".docx"), "\n")

  # 2. HTML via modelsummary, for quick on-screen reading.
  tryCatch({
    modelsummary::modelsummary(
      models, stars = TRUE, statistic = "std.error", gof_map = gof,
      output = file.path(out_dir, paste0(stem, ".html")), title = title
    )
    cat("Wrote", paste0(stem, ".html"), "\n")
  }, error = function(e) {
    message("  could not write ", stem, ".html: ", conditionMessage(e))
  })
}

export_models(base_context_models, "domain_models_base_context",
              "Individual characteristics and neighborhood socioeconomic context")
export_models(domain_models_swb, "domain_models_swb",
              "Built environment domains predicting subjective well-being")
export_models(domain_models_belonging, "domain_models_belonging",
              "Built environment domains predicting neighborhood belonging")
export_models(domain_models_swb_with_belonging, "domain_models_swb_with_belonging",
              "Built environment domains predicting SWB, adjusting for belonging")
export_models(final_models, "domain_models_final_compact",
              "Final parsimonious models")

cat("\nDone.\n")
cat("Authoritative numeric output: all_model_coefficients.csv, model_fit_summary.csv\n")
cat("Word tables: domain_models_*.docx  (written via officer, no pandoc needed)\n")
