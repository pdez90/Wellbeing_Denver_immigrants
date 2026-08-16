# =============================================================================
# 08_models_mediation_ORIGINAL.R
# ORIGINAL combined domain models + formal mediation chunk, split verbatim for reference
#
#   INPUT : respondents_with_transport_ht_segpoi_stoz_diversity.csv
#   OUTPUT: Table8_Formal_Mediation.docx
#
# Split from Wellbeing.Rmd (lines 4370-4869). Body is verbatim from the original
# chunk except where noted; run 00_config.R first or source() it below.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")

# ============================================================
# SWB / BELONGING DOMAIN MODELS + FORMAL MEDIATION TESTS
# ============================================================

packages <- c(
  "tidyverse", "janitor", "psych", "car", "broom",
  "modelsummary", "readr", "mediation", "flextable", "officer"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

invisible(lapply(packages, install_if_missing))
invisible(lapply(packages, library, character.only = TRUE))

# out_dir, target_crs, map_dir, table_dir come from 00_config.R -- do not hardcode.

infile <- file.path(
  out_dir,
  "respondents_with_transport_ht_segpoi_stoz_diversity.csv"
)

dat <- readr::read_csv(infile, show_col_types = FALSE) %>%
  janitor::clean_names()

# ============================================================
# HELPERS
# ============================================================

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
zscore <- function(x) as.numeric(scale(x))

rename_if_present <- function(df, old, new) {
  if (old %in% names(df) && !(new %in% names(df))) {
    df <- df %>% dplyr::rename("{new}" := dplyr::all_of(old))
  }
  df
}

make_lm <- function(outcome, rhs, data) {
  rhs <- rhs[rhs %in% names(data)]
  rhs <- rhs[sapply(data[rhs], function(x) dplyr::n_distinct(x, na.rm = TRUE) > 1)]
  f <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
  lm(f, data = data)
}

# ============================================================
# RENAME SURVEY VARIABLES
# ============================================================

rename_map <- c(
  year_born = "q3_1",
  gender = "q3_2",
  marital = "x2",
  children = "q3_4",
  hispanic = "q3_5",
  race = "q3_6",
  imm_status = "q3_7",
  income_hh = "q3_10",
  engl_speak = "q3_11_1",
  edu_level = "q3_14",
  time_live_denver = "q5_9",
  time_live_hood = "q5_10",
  sat_stand_liv = "q8_1_1",
  sat_health = "q8_1_2",
  sat_life_achieve = "q8_1_3",
  feel_safe = "q8_1_4",
  feel_accepted = "q8_1_5",
  feel_conf_finance = "q8_1_6",
  hood_belong = "q9_7_1",
  hood_trust_people = "q9_7_2",
  hood_borrow = "q9_7_3",
  hood_contacts = "q9_7_4"
)

for (new_name in names(rename_map)) {
  dat <- rename_if_present(dat, rename_map[[new_name]], new_name)
}

# ============================================================
# OUTCOMES
# ============================================================

swb_items <- c(
  "sat_stand_liv", "sat_health", "sat_life_achieve",
  "feel_safe", "feel_accepted", "feel_conf_finance"
)

belonging_items <- c(
  "hood_belong", "hood_trust_people", "hood_borrow", "hood_contacts"
)

dat <- dat %>%
  dplyr::mutate(
    dplyr::across(dplyr::any_of(c(swb_items, belonging_items)), to_num),
    swb_index = rowMeans(dplyr::across(dplyr::any_of(swb_items)), na.rm = TRUE),
    belonging_index = rowMeans(dplyr::across(dplyr::any_of(belonging_items)), na.rm = TRUE),
    swb_z = zscore(swb_index),
    belonging_z = zscore(belonging_index),
    age = if ("year_born" %in% names(.)) 2019 - to_num(year_born) else age
  )

cat("\nSWB alpha:\n")
print(psych::alpha(dat[, swb_items[swb_items %in% names(dat)]]))

cat("\nBelonging alpha:\n")
print(psych::alpha(dat[, belonging_items[belonging_items %in% names(dat)]]))

# ============================================================
# DEFINE VARIABLES
# ============================================================

individual_controls <- c(
  "age", "gender", "marital", "children", "hispanic", "race",
  "imm_status", "income_hh", "edu_level", "engl_speak",
  "time_live_denver", "time_live_hood"
)

context_vars <- c(
  "pop_density", "housing_density", "dist_downtown_km",
  "pct_poverty", "pct_non_native", "neighborhood_ses_index"
)

domain_vars <- list(
  urban_form = c(
    "walk_nat_walk_ind",
    "street_intdensity",
    "urban_center_nearest_dist_m"
  ),

  access_transport = c(
    "hudjob_jobs_idx",
    "sidewalk_density_800",
    "bike_facility_density_800",
    "active_corridor_density_800",
    "ht_t_ami"
  ),

  green_parks = c(
    "tree_tes",
    "tree_treecanopy",
    "park_acres_half_mile",
    "park_nearest_dist_m",
    "lc_800m_tree_canopy",
    "lc_800m_impervious_surfaces"
  ),

  safety_social = c(
    "crash_density_800",
    "ped_crash_density_800",
    "bike_crash_density_800",
    "short_trip_zone_share_800",
    "div_total_diversity_resi",
    "div_exposure_mean"
  )
)

all_numeric_covars <- unique(c(context_vars, unlist(domain_vars)))

dat <- dat %>%
  dplyr::mutate(
    dplyr::across(dplyr::any_of(c(individual_controls, all_numeric_covars)), to_num)
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::any_of(all_numeric_covars),
      zscore,
      .names = "{.col}_z"
    )
  )

zname <- function(x) {
  out <- paste0(x, "_z")
  out[out %in% names(dat)]
}

context_z <- zname(context_vars)
domain_z <- lapply(domain_vars, zname)

individual_controls <- individual_controls[individual_controls %in% names(dat)]

model_vars <- unique(c(
  "swb_z", "belonging_z",
  individual_controls,
  context_z,
  unlist(domain_z)
))

model_dat <- dat %>%
  dplyr::select(dplyr::any_of(model_vars)) %>%
  dplyr::filter(!is.na(swb_z), !is.na(belonging_z))

factor_controls <- c("gender", "marital", "hispanic", "race", "imm_status")
factor_controls <- factor_controls[factor_controls %in% names(model_dat)]

model_dat <- model_dat %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(factor_controls), as.factor))

keep_vars <- names(model_dat)[
  sapply(model_dat, function(x) dplyr::n_distinct(x, na.rm = TRUE) > 1)
]

model_dat <- model_dat %>%
  dplyr::select(dplyr::all_of(keep_vars))

individual_controls <- individual_controls[individual_controls %in% names(model_dat)]
context_z <- context_z[context_z %in% names(model_dat)]
domain_z <- lapply(domain_z, function(x) x[x %in% names(model_dat)])

# ============================================================
# MODEL SETS
# ============================================================

rhs_base <- individual_controls
rhs_context <- unique(c(individual_controls, context_z))

m_swb_base <- make_lm("swb_z", rhs_base, model_dat)
m_swb_context <- make_lm("swb_z", rhs_context, model_dat)

m_belong_base <- make_lm("belonging_z", rhs_base, model_dat)
m_belong_context <- make_lm("belonging_z", rhs_context, model_dat)

domain_models_swb <- list()
domain_models_belonging <- list()
domain_models_swb_with_belonging <- list()

for (d in names(domain_z)) {

  rhs_domain <- unique(c(individual_controls, context_z, domain_z[[d]]))

  domain_models_swb[[paste0("SWB: ", d)]] <-
    make_lm("swb_z", rhs_domain, model_dat)

  domain_models_belonging[[paste0("Belonging: ", d)]] <-
    make_lm("belonging_z", rhs_domain, model_dat)

  domain_models_swb_with_belonging[[paste0("SWB + belonging: ", d)]] <-
    make_lm("swb_z", unique(c("belonging_z", rhs_domain)), model_dat)
}

# Final compact model: strongest / most interpretable from each domain
final_be <- c(
  "walk_nat_walk_ind_z",
  "bike_facility_density_800_z",
  "park_acres_half_mile_z",
  "tree_tes_z",
  "crash_density_800_z",
  "short_trip_zone_share_800_z",
  "div_exposure_mean_z"
)

final_be <- final_be[final_be %in% names(model_dat)]

m_swb_final <- make_lm(
  "swb_z",
  unique(c(individual_controls, context_z, final_be)),
  model_dat
)

m_belonging_final <- make_lm(
  "belonging_z",
  unique(c(individual_controls, context_z, final_be)),
  model_dat
)

m_swb_final_with_belonging <- make_lm(
  "swb_z",
  unique(c("belonging_z", individual_controls, context_z, final_be)),
  model_dat
)

# ============================================================
# FORMAL MEDIATION TESTS + TABLE 8
# Robust replacement
# ============================================================

rm(list = intersect(ls(), c(
  "mediation_table", "mediation_table_clean", "table8", "ft8", "doc8"
)))

predictor_labels <- c(
  walk_nat_walk_ind_z = "EPA Walkability Index",
  sidewalk_density_800_z = "Sidewalk density",
  bike_facility_density_800_z = "Bicycle facility density",
  tree_tes_z = "Tree Equity Score",
  tree_treecanopy_z = "Tree canopy",
  park_acres_half_mile_z = "Park acreage within 800 m",
  crash_density_800_z = "Crash density",
  bike_crash_density_800_z = "Bicycle crash density"
)

domain_labels <- c(
  urban_form = "Urban form",
  access_transport = "Transportation",
  green_parks = "Greenness and parks",
  safety_social = "Safety and neighborhood environment"
)

mediation_candidates <- tibble::tribble(
  ~domain, ~predictor,
  "urban_form",       "walk_nat_walk_ind_z",
  "access_transport", "sidewalk_density_800_z",
  "access_transport", "bike_facility_density_800_z",
  "green_parks",      "tree_tes_z",
  "green_parks",      "tree_treecanopy_z",
  "green_parks",      "park_acres_half_mile_z",
  "safety_social",    "crash_density_800_z",
  "safety_social",    "bike_crash_density_800_z"
) %>%
  dplyr::filter(predictor %in% names(model_dat))

med_covars <- c(
  "age", "children", "income_hh", "edu_level", "engl_speak",
  "time_live_denver", "time_live_hood", context_z
)

med_covars <- med_covars[med_covars %in% names(model_dat)]

empty_med_row <- function(domain_name, treat_var, msg = NA_character_) {
  tibble::tibble(
    domain = domain_name,
    predictor = treat_var,
    n = NA_integer_,
    acme = NA_real_,
    acme_ci_low = NA_real_,
    acme_ci_high = NA_real_,
    acme_p = NA_real_,
    ade = NA_real_,
    ade_ci_low = NA_real_,
    ade_ci_high = NA_real_,
    ade_p = NA_real_,
    total_effect = NA_real_,
    total_ci_low = NA_real_,
    total_ci_high = NA_real_,
    total_p = NA_real_,
    prop_mediated = NA_real_,
    prop_mediated_p = NA_real_,
    model_status = paste("Failed:", msg)
  )
}

run_one_mediation <- function(domain_name, treat_var, sims = 1000) {

  vars_needed <- unique(c("swb_z", "belonging_z", treat_var, med_covars))

  med_dat <- model_dat %>%
    dplyr::select(dplyr::any_of(vars_needed)) %>%
    tidyr::drop_na()

  if (nrow(med_dat) < 50) {
    return(empty_med_row(domain_name, treat_var, "Too few complete cases"))
  }

  if (dplyr::n_distinct(med_dat[[treat_var]], na.rm = TRUE) < 2) {
    return(empty_med_row(domain_name, treat_var, "Predictor has no variation"))
  }

  covars_here <- setdiff(med_covars, treat_var)

  med_formula <- stats::as.formula(
    paste("belonging_z ~", paste(c(treat_var, covars_here), collapse = " + "))
  )

  out_formula <- stats::as.formula(
    paste("swb_z ~ belonging_z +", paste(c(treat_var, covars_here), collapse = " + "))
  )

  tryCatch({

med_fit <- do.call(
  stats::lm,
  list(
    formula = med_formula,
    data = quote(med_dat)
  )
)

out_fit <- do.call(
  stats::lm,
  list(
    formula = out_formula,
    data = quote(med_dat)
  )
)

    med_out <- mediation::mediate(
      med_fit,
      out_fit,
      treat = treat_var,
      mediator = "belonging_z",
      boot = TRUE,
      sims = sims
    )

    tibble::tibble(
      domain = domain_name,
      predictor = treat_var,
      n = nrow(med_dat),
      acme = med_out$d0,
      acme_ci_low = med_out$d0.ci[1],
      acme_ci_high = med_out$d0.ci[2],
      acme_p = med_out$d0.p,
      ade = med_out$z0,
      ade_ci_low = med_out$z0.ci[1],
      ade_ci_high = med_out$z0.ci[2],
      ade_p = med_out$z0.p,
      total_effect = med_out$tau.coef,
      total_ci_low = med_out$tau.ci[1],
      total_ci_high = med_out$tau.ci[2],
      total_p = med_out$tau.p,
      prop_mediated = med_out$n0,
      prop_mediated_p = med_out$n0.p,
      model_status = "Ran successfully"
    )

  }, error = function(e) {
    empty_med_row(domain_name, treat_var, e$message)
  })
}

mediation_table <- purrr::pmap_dfr(
  mediation_candidates,
  function(domain, predictor) {
    cat("\nRunning mediation:", domain, "-", predictor, "\n")
    run_one_mediation(domain, predictor, sims = 1000)
  }
)

mediation_table_clean <- mediation_table %>%
  dplyr::mutate(
    mediation_supported = dplyr::case_when(
      model_status != "Ran successfully" ~ model_status,
      !is.na(acme_p) & acme_p < 0.05 ~ "Yes: ACME p < 0.05",
      !is.na(acme_p) & acme_p < 0.10 ~ "Weak: ACME p < 0.10",
      TRUE ~ "No formal mediation evidence"
    )
  )

print(mediation_table_clean)

readr::write_csv(
  mediation_table_clean,
  file.path(out_dir, "formal_mediation_results_domain_models.csv")
)

table8 <- mediation_table_clean %>%
  dplyr::mutate(
    Domain = dplyr::recode(domain, !!!domain_labels, .default = domain),
    Predictor = dplyr::recode(predictor, !!!predictor_labels, .default = predictor),
    `N` = n,
    `Indirect effect (ACME)` = ifelse(is.na(acme), "", sprintf("%.3f", acme)),
    `95% CI (ACME)` = ifelse(
      is.na(acme_ci_low), "",
      paste0("(", sprintf("%.3f", acme_ci_low), ", ", sprintf("%.3f", acme_ci_high), ")")
    ),
    `ACME p-value` = ifelse(is.na(acme_p), "", sprintf("%.3f", acme_p)),
    `Direct effect (ADE)` = ifelse(is.na(ade), "", sprintf("%.3f", ade)),
    `ADE p-value` = ifelse(is.na(ade_p), "", sprintf("%.3f", ade_p)),
    `Total effect` = ifelse(is.na(total_effect), "", sprintf("%.3f", total_effect)),
    `Total effect p-value` = ifelse(is.na(total_p), "", sprintf("%.3f", total_p)),
    `% mediated` = ifelse(is.na(prop_mediated), "", sprintf("%.1f%%", 100 * prop_mediated)),
    `Interpretation` = mediation_supported
  ) %>%
  dplyr::select(
    Domain, Predictor, N,
    `Indirect effect (ACME)`, `95% CI (ACME)`, `ACME p-value`,
    `Direct effect (ADE)`, `ADE p-value`,
    `Total effect`, `Total effect p-value`,
    `% mediated`, `Interpretation`
  ) %>%
  dplyr::arrange(Domain, Predictor)

readr::write_csv(table8, file.path(out_dir, "Table8_Formal_Mediation.csv"))

ft8 <- flextable::flextable(table8) %>%
  flextable::theme_booktabs() %>%
  flextable::autofit() %>%
  flextable::fontsize(size = 8, part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::align(j = c("Domain", "Predictor", "Interpretation"), align = "left") %>%
  flextable::align(
    j = setdiff(names(table8), c("Domain", "Predictor", "Interpretation")),
    align = "center"
  ) %>%
  flextable::add_footer_lines(
    "Table 8. Formal causal mediation analyses evaluating neighborhood belonging as a mediator of associations between objective built-environment characteristics and subjective well-being. ACME = average causal mediation effect; ADE = average direct effect. Indirect effects were estimated using 1,000 nonparametric bootstrap simulations. Continuous built-environment variables are standardized."
  )

doc8 <- officer::read_docx() %>%
  officer::body_add_par("Table 8. Formal Mediation Analysis", style = "heading 1") %>%
  flextable::body_add_flextable(ft8)

print(doc8, target = file.path(out_dir, "Table8_Formal_Mediation.docx"))

cat("\nSaved Table 8:\n")
cat(file.path(out_dir, "Table8_Formal_Mediation.docx"), "\n")
cat(file.path(out_dir, "Table8_Formal_Mediation.csv"), "\n")
