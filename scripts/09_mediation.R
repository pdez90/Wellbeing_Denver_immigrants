# =============================================================================
# 09_mediation.R
# Formal causal mediation: does neighborhood belonging mediate the association
# between built-environment characteristics and subjective well-being?
# Produces the numbers behind Results 3.7 (paper Table 8).
#
#   INPUT : model_objects.rds  (written by 08_domain_models.R)
#   OUTPUT: formal_mediation_results_domain_models.csv
#           Table8_Formal_Mediation.csv / .docx
#
# Runtime: ~2-5 minutes (11 predictors x 1,000 bootstrap resamples).
#
# CHANGES vs. the original Wellbeing.Rmd chunk:
#   1. set.seed() before the bootstrap. The original had none, so the published
#      confidence intervals were not reproducible run to run.
#   2. Adds the land-use / destination predictors to the candidate list.
#   3. Records the covariate set actually used, which differs from the domain
#      models (see NOTE below) -- this needs to be stated in the paper.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
wb_require(wb_packages_analysis)

obj <- readRDS(file.path(out_dir, "model_objects.rds"))
model_dat <- obj$model_dat
context_z <- obj$context_z

set.seed(WB_SEED)

# -----------------------------------------------------------------------------
# NOTE ON THE COVARIATE SET -- read before writing this up
# -----------------------------------------------------------------------------
# The mediation models adjust for a SMALLER set of covariates than the domain
# models in 08. The categorical controls (gender, marital status, hispanic,
# race, immigration status) are excluded, because a bootstrap resample can
# produce a factor level with zero observations, which makes lm() fail inside
# mediation::mediate().
#
# Consequence: mediation models run on n = 319 (or 249 where Tree Equity data
# is missing), whereas the domain models run on n = 238-304, AND they adjust
# for a different confounder set. The two sets of estimates are therefore not
# directly comparable, and the paper must say so rather than presenting Table 8
# as a continuation of Tables 4-6.
#
# If you want them comparable, the fix is to collapse the categorical controls
# into numeric indicators (e.g. foreign_born_recent 0/1, partnered 0/1) that
# survive resampling, and use the same set in both places.
# -----------------------------------------------------------------------------

med_covars <- c("age", "children", "income_hh", "edu_level", "engl_speak",
                "time_live_denver", "time_live_hood", context_z)
med_covars <- med_covars[med_covars %in% names(model_dat)]

cat("Mediation covariate set:\n  ", paste(med_covars, collapse = ", "), "\n\n")

predictor_labels <- c(
  walk_nat_walk_ind_z          = "EPA Walkability Index",
  sidewalk_density_800_z       = "Sidewalk density",
  bike_facility_density_800_z  = "Bicycle facility density",
  tree_tes_z                   = "Tree Equity Score",
  tree_treecanopy_z            = "Tree canopy (Tree Equity)",
  park_acres_half_mile_z       = "Park acreage within 800 m",
  crash_density_800_z          = "Crash density",
  bike_crash_density_800_z     = "Bicycle crash density",
  pfa_share_800_z              = "Pedestrian focus area share",
  zone_adu_yes                 = "ADU permitted in zone"
)

domain_labels <- c(
  urban_form       = "Urban form",
  access_transport = "Transportation",
  green_parks      = "Greenness and parks",
  safety_social    = "Safety and neighborhood environment",
  land_use         = "Land use and regulation"
)

mediation_candidates <- tibble::tribble(
  ~domain,            ~predictor,
  "urban_form",       "walk_nat_walk_ind_z",
  "access_transport", "sidewalk_density_800_z",
  "access_transport", "bike_facility_density_800_z",
  "green_parks",      "tree_tes_z",
  "green_parks",      "tree_treecanopy_z",
  "green_parks",      "park_acres_half_mile_z",
  "safety_social",    "crash_density_800_z",
  "safety_social",    "bike_crash_density_800_z",
  "land_use",         "pfa_share_800_z",
  "land_use",         "zone_adu_yes"
) %>%
  dplyr::filter(predictor %in% names(model_dat))

empty_med_row <- function(domain_name, treat_var, msg = NA_character_) {
  tibble::tibble(
    domain = domain_name, predictor = treat_var, n = NA_integer_,
    acme = NA_real_, acme_ci_low = NA_real_, acme_ci_high = NA_real_, acme_p = NA_real_,
    ade = NA_real_, ade_ci_low = NA_real_, ade_ci_high = NA_real_, ade_p = NA_real_,
    total_effect = NA_real_, total_ci_low = NA_real_, total_ci_high = NA_real_, total_p = NA_real_,
    prop_mediated = NA_real_, prop_mediated_p = NA_real_,
    model_status = paste("Failed:", msg)
  )
}

run_one_mediation <- function(domain_name, treat_var, sims = 1000) {

  vars_needed <- unique(c("swb_z", "belonging_z", treat_var, med_covars))
  med_dat <- model_dat %>%
    dplyr::select(dplyr::any_of(vars_needed)) %>%
    tidyr::drop_na()

  if (nrow(med_dat) < 50) return(empty_med_row(domain_name, treat_var, "Too few complete cases"))
  if (dplyr::n_distinct(med_dat[[treat_var]], na.rm = TRUE) < 2)
    return(empty_med_row(domain_name, treat_var, "Predictor has no variation"))

  covars_here <- setdiff(med_covars, treat_var)

  med_formula <- stats::as.formula(
    paste("belonging_z ~", paste(c(treat_var, covars_here), collapse = " + ")))
  out_formula <- stats::as.formula(
    paste("swb_z ~ belonging_z +", paste(c(treat_var, covars_here), collapse = " + ")))

  tryCatch({
    med_fit <- do.call(stats::lm, list(formula = med_formula, data = quote(med_dat)))
    out_fit <- do.call(stats::lm, list(formula = out_formula, data = quote(med_dat)))

    med_out <- mediation::mediate(
      med_fit, out_fit, treat = treat_var, mediator = "belonging_z",
      boot = TRUE, sims = sims
    )

    tibble::tibble(
      domain = domain_name, predictor = treat_var, n = nrow(med_dat),
      acme = med_out$d0, acme_ci_low = med_out$d0.ci[1],
      acme_ci_high = med_out$d0.ci[2], acme_p = med_out$d0.p,
      ade = med_out$z0, ade_ci_low = med_out$z0.ci[1],
      ade_ci_high = med_out$z0.ci[2], ade_p = med_out$z0.p,
      total_effect = med_out$tau.coef, total_ci_low = med_out$tau.ci[1],
      total_ci_high = med_out$tau.ci[2], total_p = med_out$tau.p,
      prop_mediated = med_out$n0, prop_mediated_p = med_out$n0.p,
      model_status = "Ran successfully"
    )
  }, error = function(e) empty_med_row(domain_name, treat_var, e$message))
}

mediation_table <- purrr::pmap_dfr(
  mediation_candidates,
  function(domain, predictor) {
    cat("Running mediation:", domain, "-", predictor, "\n")
    run_one_mediation(domain, predictor, sims = 1000)
  }
)

# -----------------------------------------------------------------------------
# Multiplicity
# -----------------------------------------------------------------------------
# Ten indirect effects are tested. An uncorrected p < .05 among ten tests is
# weak evidence on its own, so report both.

mediation_table_clean <- mediation_table %>%
  dplyr::mutate(
    acme_p_holm = stats::p.adjust(acme_p, method = "holm"),
    mediation_supported = dplyr::case_when(
      model_status != "Ran successfully"        ~ model_status,
      !is.na(acme_p_holm) & acme_p_holm < 0.05  ~ "Yes: ACME p < 0.05 after Holm correction",
      !is.na(acme_p) & acme_p < 0.05            ~ "Uncorrected p < 0.05 only; not robust to multiplicity",
      !is.na(acme_p) & acme_p < 0.10            ~ "Weak: uncorrected ACME p < 0.10",
      TRUE                                      ~ "No formal mediation evidence"
    )
  )

print(mediation_table_clean, width = Inf)
readr::write_csv(mediation_table_clean,
                 file.path(out_dir, "formal_mediation_results_domain_models.csv"))

# -----------------------------------------------------------------------------
# Table 8
# -----------------------------------------------------------------------------

fmt <- function(x, d = 3) ifelse(is.na(x), "", sprintf(paste0("%.", d, "f"), x))

table8 <- mediation_table_clean %>%
  dplyr::mutate(
    Domain    = dplyr::recode(domain, !!!domain_labels, .default = domain),
    Predictor = dplyr::recode(predictor, !!!predictor_labels, .default = predictor),
    N = n,
    `Indirect effect (ACME)` = fmt(acme),
    `95% CI (ACME)` = ifelse(is.na(acme_ci_low), "",
                             paste0("(", fmt(acme_ci_low), ", ", fmt(acme_ci_high), ")")),
    `ACME p` = fmt(acme_p),
    `ACME p (Holm)` = fmt(acme_p_holm),
    `Direct effect (ADE)` = fmt(ade),
    `ADE p` = fmt(ade_p),
    `Total effect` = fmt(total_effect),
    `Total p` = fmt(total_p),
    `% mediated` = ifelse(is.na(prop_mediated), "", sprintf("%.1f%%", 100 * prop_mediated)),
    Interpretation = mediation_supported
  ) %>%
  dplyr::select(Domain, Predictor, N, `Indirect effect (ACME)`, `95% CI (ACME)`,
                `ACME p`, `ACME p (Holm)`, `Direct effect (ADE)`, `ADE p`,
                `Total effect`, `Total p`, `% mediated`, Interpretation) %>%
  dplyr::arrange(Domain, Predictor)

readr::write_csv(table8, file.path(out_dir, "Table8_Formal_Mediation.csv"))

ft8 <- flextable::flextable(table8) %>%
  flextable::theme_booktabs() %>% flextable::autofit() %>%
  flextable::fontsize(size = 8, part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::align(j = c("Domain", "Predictor", "Interpretation"), align = "left") %>%
  flextable::add_footer_lines(paste(
    "Table 8. Formal causal mediation analyses evaluating neighborhood belonging as a",
    "mediator of associations between objective built-environment characteristics and",
    "subjective well-being. ACME = average causal mediation effect; ADE = average direct",
    "effect. Indirect effects estimated using 1,000 nonparametric bootstrap simulations",
    sprintf("(seed = %d).", WB_SEED),
    "Continuous built-environment variables are standardized. Mediation models adjust for",
    "age, number of children, household income, education, English proficiency, years in",
    "Denver, years in neighborhood, and neighborhood socioeconomic context; unlike the",
    "domain models in Tables 4-6 they do NOT adjust for gender, marital status, race,",
    "ethnicity, or immigration status. Holm-adjusted p-values account for testing ten",
    "indirect effects."
  ))

doc8 <- officer::read_docx() %>%
  officer::body_add_par("Table 8. Formal Mediation Analysis", style = "heading 1") %>%
  flextable::body_add_flextable(ft8)
print(doc8, target = file.path(out_dir, "Table8_Formal_Mediation.docx"))

cat("\nSaved Table 8 (csv + docx) to", out_dir, "\n")
