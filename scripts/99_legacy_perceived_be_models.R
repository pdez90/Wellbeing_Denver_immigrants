# =============================================================================
# 99_legacy_perceived_be_models.R
# LEGACY / ARCHIVED - models of PERCEIVED built environment from survey items only
#
#   INPUT : swb_data.csv
#   OUTPUT: Table1-3_*.docx, swb_model_results.html, sem_mediation_results.csv
#
# Split from Wellbeing.Rmd (lines 12-849). Body is verbatim from the original
# chunk except where noted; run 00_config.R first or source() it below.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")

# ============================================================
# SWB / Belonging / Built Environment Models
# ============================================================

packages <- c(
  "tidyverse", "janitor", "psych", "lavaan",
  "car", "broom", "modelsummary", "readr"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

invisible(lapply(packages, install_if_missing))
invisible(lapply(packages, library, character.only = TRUE))

# ----------------------------
# 1. Read data
# ----------------------------

infile <- wb_path("survey")

raw <- read_csv(infile, show_col_types = FALSE)

dat <- raw %>%
  clean_names()

dat <- dat %>%
  filter(
    !is.na(response_id),
    response_id != "Response ID",
    response_id != "response_id"
  )

# ----------------------------
# 2. Helper functions
# ----------------------------

to_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

zscore <- function(x) {
  as.numeric(scale(x))
}

safe_alpha <- function(df, vars, scale_name) {
  vars_present <- vars[vars %in% names(df)]

  if (length(vars_present) < 2) {
    message(paste("Not enough items for alpha:", scale_name))
    return(NULL)
  }

  psych::alpha(df[, vars_present])
}

make_index <- function(df, vars, index_name) {
  vars_present <- vars[vars %in% names(df)]

  if (length(vars_present) == 0) {
    warning(paste("No variables found for", index_name))
    df[[index_name]] <- NA_real_
    return(df)
  }

  df %>%
    mutate(across(all_of(vars_present), to_num)) %>%
    mutate("{index_name}" := rowMeans(across(all_of(vars_present)), na.rm = TRUE))
}

# ----------------------------
# 3. Rename survey variables
# ----------------------------

dat <- dat %>%
  rename(
    year_born = q3_1,
    gender = q3_2,
    marital = x2,
    children = q3_4,
    hispanic = q3_5,
    race = q3_6,
    imm_status = q3_7,
    age_migr = q3_8,
    country = q3_9,
    income_hh = q3_10,
    engl_speak = q3_11_1,
    engl_und = q3_11_2,
    engl_read = q3_11_3,
    engl_write = q3_11_4,
    edu_level = q3_14,
    health_care = q3_18,
    latitude = lat,
    longitude = long,
    house_type = q5_2,
    house_tenure = q5_3,
    adults_hh = q5_4,
    children_hh = q5_5,
    time_live_denver = q5_9,
    time_live_hood = q5_10,
    vehicles_hh = q6_1,
    commute_mode = q6_2,
    commute_length = q6_3,
    cycle_yn = q6_4,
    walk_yn = q6_5,
    transit_freq = q6_6,
    uber_freq = q6_7,
    cult_event_freq = q7_6,
    volunteer_freq = q7_7,
    soc_act_freq = q7_8,
    sat_stand_liv = q8_1_1,
    sat_health = q8_1_2,
    sat_life_achieve = q8_1_3,
    feel_safe = q8_1_4,
    feel_accepted = q8_1_5,
    feel_conf_finance = q8_1_6,
    friend_fam_discuss = q8_2,
    meet_socially_freq = q8_3,
    feels_home = q9_2,
    discriminate_re = q9_3,
    unwelcome_public_space_re = q9_4,
    discr_lang = q9_5,
    discr_increase = q9_6,
    hood_belong = q9_7_1,
    hood_trust_people = q9_7_2,
    hood_borrow = q9_7_3,
    hood_contacts = q9_7_4,
    hood_adults_role = q9_7_5,
    hood_ideas_raise_children = q9_7_6,
    hood_crime_problem = q9_7_7,
    vhood_park = q9_8_2,
    vhood_specialty_park = q9_8_3,
    vhood_rec = q9_8_4,
    vhood_library = q9_8_5,
    vhood_museum = q9_8_6,
    vhood_sport = q9_8_7
  )

# ----------------------------
# 4. Define variables
# ----------------------------

swb_items <- c(
  "sat_stand_liv",
  "sat_health",
  "sat_life_achieve",
  "feel_safe",
  "feel_accepted",
  "feel_conf_finance"
)

belonging_items <- c(
  "hood_belong",
  "hood_trust_people",
  "hood_borrow",
  "hood_contacts"
)

control_vars <- c(
  "age",
  "gender",
  "marital",
  "children",
  "hispanic",
  "race",
  "imm_status",
  "income_hh",
  "edu_level",
  "engl_speak",
  "time_live_denver",
  "time_live_hood"
)

housing_vars <- c(
  "house_tenure",
  "house_type",
  "adults_hh",
  "children_hh"
)

mobility_vars <- c(
  "vehicles_hh",
  "commute_length",
  "transit_freq",
  "uber_freq",
  "cycle_yn",
  "walk_yn"
)

public_space_vars <- c(
  "vhood_park",
  "vhood_specialty_park",
  "vhood_rec",
  "vhood_library",
  "vhood_museum",
  "vhood_sport"
)

safety_vars <- c(
  "hood_crime_problem"
)

social_participation_vars <- c(
  "cult_event_freq",
  "volunteer_freq",
  "soc_act_freq"
)

all_numeric_vars <- c(
  swb_items,
  belonging_items,
  control_vars,
  housing_vars,
  mobility_vars,
  public_space_vars,
  safety_vars,
  social_participation_vars,
  "year_born",
  "age_migr"
)

dat <- dat %>%
  mutate(across(any_of(all_numeric_vars), to_num))

# ----------------------------
# 5. Create SWB and belonging scales
# ----------------------------

swb_present <- swb_items[swb_items %in% names(dat)]
belong_present <- belonging_items[belonging_items %in% names(dat)]

print(swb_present)
print(belong_present)

safe_alpha(dat, swb_present, "SWB")
safe_alpha(dat, belong_present, "Belonging")

dat <- dat %>%
  mutate(
    swb_index = rowMeans(across(all_of(swb_present)), na.rm = TRUE),
    belonging_index = rowMeans(across(all_of(belong_present)), na.rm = TRUE),
    swb_z = zscore(swb_index),
    belonging_z = zscore(belonging_index),
    age = 2019 - year_born
  ) %>%
  filter(!is.na(swb_z), !is.na(belonging_z))

# ----------------------------
# 6. Create built environment indices
# ----------------------------

dat <- dat %>%
  make_index(housing_vars, "be_housing") %>%
  make_index(mobility_vars, "be_mobility") %>%
  make_index(public_space_vars, "be_public_space") %>%
  make_index(safety_vars, "be_safety") %>%
  make_index(social_participation_vars, "social_participation")

be_indices <- c(
  "be_housing",
  "be_mobility",
  "be_public_space",
  "be_safety"
)

be_indices <- be_indices[
  be_indices %in% names(dat) &
    sapply(be_indices, function(x) sum(!is.na(dat[[x]])) > 0)
]

dat <- dat %>%
  mutate(across(all_of(be_indices), zscore, .names = "{.col}_z"))

be_indices_z <- paste0(be_indices, "_z")

# ----------------------------
# 7. Prepare model data
# ----------------------------

control_vars <- control_vars[control_vars %in% names(dat)]

model_vars <- c(
  "swb_z",
  "belonging_z",
  be_indices_z,
  control_vars
)

model_dat <- dat %>%
  select(any_of(model_vars)) %>%
  drop_na(swb_z, belonging_z)

factor_controls <- c("gender", "marital", "hispanic", "race", "imm_status")
factor_controls <- factor_controls[factor_controls %in% names(model_dat)]

model_dat <- model_dat %>%
  mutate(across(all_of(factor_controls), as.factor))

keep_vars <- names(model_dat)[
  sapply(model_dat, function(x) n_distinct(x, na.rm = TRUE) > 1)
]

model_dat <- model_dat %>%
  select(all_of(keep_vars))

be_indices_z <- be_indices_z[be_indices_z %in% names(model_dat)]
control_vars <- control_vars[control_vars %in% names(model_dat)]

rhs_vars <- c(be_indices_z, control_vars)

# ----------------------------
# 8. Main models
# ----------------------------

f_m1 <- as.formula(
  paste("swb_z ~", paste(rhs_vars, collapse = " + "))
)

m1 <- lm(f_m1, data = model_dat)

f_m2 <- as.formula(
  paste("belonging_z ~", paste(rhs_vars, collapse = " + "))
)

m2 <- lm(f_m2, data = model_dat)

f_m3 <- as.formula(
  paste("swb_z ~ belonging_z +", paste(rhs_vars, collapse = " + "))
)

m3 <- lm(f_m3, data = model_dat)

summary(m1)
summary(m2)
summary(m3)

# ----------------------------
# 9. VIF checks
# ----------------------------

try(car::vif(m1))
try(car::vif(m2))
try(car::vif(m3))

# ----------------------------
# 10. SEM-style mediation model
# ----------------------------

sem_dat <- model_dat %>%
  mutate(across(where(is.factor), as.factor))

sem_rhs <- sem_dat %>%
  select(all_of(rhs_vars))

keep_sem <- complete.cases(sem_rhs)

sem_dat <- sem_dat[keep_sem, ]
sem_rhs <- sem_rhs[keep_sem, ]

x_mat <- model.matrix(
  ~ . - 1,
  data = sem_rhs
)

x_df <- as.data.frame(x_mat) %>%
  janitor::clean_names()

sem_dat2 <- bind_cols(
  sem_dat %>% select(swb_z, belonging_z),
  x_df
)

sem_rhs_vars <- names(x_df)

sem_model <- paste0("
  belonging_z ~ ", paste(sem_rhs_vars, collapse = " + "), "
  swb_z ~ belonging_z + ", paste(sem_rhs_vars, collapse = " + "), "
")

fit_sem <- lavaan::sem(
  sem_model,
  data = sem_dat2,
  missing = "fiml",
  se = "bootstrap",
  bootstrap = 1000
)

summary(
  fit_sem,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

sem_results <- parameterEstimates(
  fit_sem,
  standardized = TRUE,
  boot.ci.type = "perc"
)

print(sem_results)

# ----------------------------
# 12. Formal mediation tests
# ----------------------------

if (!requireNamespace("mediation", quietly = TRUE)) {
  install.packages("mediation")
}
library(mediation)

med_vars <- c(
  "swb_z",
  "belonging_z",
  be_indices_z,
  control_vars
)

med_dat <- model_dat %>%
  dplyr::select(dplyr::any_of(med_vars)) %>%
  tidyr::drop_na()

# For mediation bootstrapping, keep categorical controls numeric
# to avoid factor-level errors during resampling.
cat_controls <- c("gender", "marital", "hispanic", "race", "imm_status")
cat_controls <- cat_controls[cat_controls %in% names(med_dat)]

med_dat <- med_dat %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cat_controls), as.numeric))

# ----------------------------
# Public space mediation
# ----------------------------

med_fit_public <- lm(
  belonging_z ~ be_public_space_z + be_housing_z + be_mobility_z + be_safety_z +
    age + gender + marital + children + hispanic + race + imm_status +
    income_hh + edu_level + engl_speak + time_live_denver + time_live_hood,
  data = med_dat
)

out_fit_public <- lm(
  swb_z ~ be_public_space_z + belonging_z + be_housing_z + be_mobility_z + be_safety_z +
    age + gender + marital + children + hispanic + race + imm_status +
    income_hh + edu_level + engl_speak + time_live_denver + time_live_hood,
  data = med_dat
)

med_public <- mediation::mediate(
  med_fit_public,
  out_fit_public,
  treat = "be_public_space_z",
  mediator = "belonging_z",
  boot = TRUE,
  sims = 1000
)

summary(med_public)

# ----------------------------
# Housing mediation
# ----------------------------

med_fit_housing <- lm(
  belonging_z ~ be_housing_z + be_public_space_z + be_mobility_z + be_safety_z +
    age + gender + marital + children + hispanic + race + imm_status +
    income_hh + edu_level + engl_speak + time_live_denver + time_live_hood,
  data = med_dat
)

out_fit_housing <- lm(
  swb_z ~ be_housing_z + belonging_z + be_public_space_z + be_mobility_z + be_safety_z +
    age + gender + marital + children + hispanic + race + imm_status +
    income_hh + edu_level + engl_speak + time_live_denver + time_live_hood,
  data = med_dat
)

med_housing <- mediation::mediate(
  med_fit_housing,
  out_fit_housing,
  treat = "be_housing_z",
  mediator = "belonging_z",
  boot = TRUE,
  sims = 1000
)

summary(med_housing)

# ----------------------------
# Mobility mediation
# ----------------------------

med_fit_mobility <- lm(
  belonging_z ~ be_mobility_z + be_housing_z + be_public_space_z + be_safety_z +
    age + gender + marital + children + hispanic + race + imm_status +
    income_hh + edu_level + engl_speak + time_live_denver + time_live_hood,
  data = med_dat
)

out_fit_mobility <- lm(
  swb_z ~ be_mobility_z + belonging_z + be_housing_z + be_public_space_z + be_safety_z +
    age + gender + marital + children + hispanic + race + imm_status +
    income_hh + edu_level + engl_speak + time_live_denver + time_live_hood,
  data = med_dat
)

med_mobility <- mediation::mediate(
  med_fit_mobility,
  out_fit_mobility,
  treat = "be_mobility_z",
  mediator = "belonging_z",
  boot = TRUE,
  sims = 1000
)

summary(med_mobility)

# ----------------------------
# Safety mediation
# ----------------------------

med_fit_safety <- lm(
  belonging_z ~ be_safety_z + be_housing_z + be_mobility_z + be_public_space_z +
    age + gender + marital + children + hispanic + race + imm_status +
    income_hh + edu_level + engl_speak + time_live_denver + time_live_hood,
  data = med_dat
)

out_fit_safety <- lm(
  swb_z ~ be_safety_z + belonging_z + be_housing_z + be_mobility_z + be_public_space_z +
    age + gender + marital + children + hispanic + race + imm_status +
    income_hh + edu_level + engl_speak + time_live_denver + time_live_hood,
  data = med_dat
)

med_safety <- mediation::mediate(
  med_fit_safety,
  out_fit_safety,
  treat = "be_safety_z",
  mediator = "belonging_z",
  boot = TRUE,
  sims = 1000
)

summary(med_safety)

# ----------------------------
# Export mediation results
# ----------------------------

extract_mediation <- function(med_out, name) {
  tibble::tibble(
    predictor = name,

    acme_estimate = med_out$d0,
    acme_ci_low = med_out$d0.ci[1],
    acme_ci_high = med_out$d0.ci[2],
    acme_p = med_out$d0.p,

    ade_estimate = med_out$z0,
    ade_ci_low = med_out$z0.ci[1],
    ade_ci_high = med_out$z0.ci[2],
    ade_p = med_out$z0.p,

    total_effect = med_out$tau.coef,
    total_ci_low = med_out$tau.ci[1],
    total_ci_high = med_out$tau.ci[2],
    total_p = med_out$tau.p,

    prop_mediated = med_out$n0,
    prop_mediated_ci_low = med_out$n0.ci[1],
    prop_mediated_ci_high = med_out$n0.ci[2],
    prop_mediated_p = med_out$n0.p
  )
}

mediation_table <- dplyr::bind_rows(
  extract_mediation(med_public, "be_public_space_z"),
  extract_mediation(med_housing, "be_housing_z"),
  extract_mediation(med_mobility, "be_mobility_z"),
  extract_mediation(med_safety, "be_safety_z")
)

print(mediation_table)

readr::write_csv(
  mediation_table,
  file.path(out_dir, "formal_mediation_results.csv")
)
# ----------------------------
# 11. Export results
# ----------------------------

models <- list(
  "M1: Built Environment / Controls -> SWB" = m1,
  "M2: Built Environment / Controls -> Belonging" = m2,
  "M3: BE + Belonging -> SWB" = m3
)

modelsummary(
  models,
  output = file.path(out_dir, "swb_model_results.html"),
  statistic = "({std.error})",
  stars = TRUE
)

write_csv(
  broom::tidy(m1),
  file.path(out_dir, "model1_built_environment_swb.csv")
)

write_csv(
  broom::tidy(m2),
  file.path(out_dir, "model2_built_environment_belonging.csv")
)

write_csv(
  broom::tidy(m3),
  file.path(out_dir, "model3_belonging_swb.csv")
)

write_csv(
  sem_results,
  file.path(out_dir, "sem_mediation_results.csv")
)

write_csv(
  model_dat,
  file.path(out_dir, "model_data_cleaned.csv")
)

# ============================================================
# 13. Word-ready tables
# ============================================================

packages_tables <- c(
  "officer", "flextable", "gtsummary", "gt", "labelled"
)

invisible(lapply(packages_tables, install_if_missing))
invisible(lapply(packages_tables, library, character.only = TRUE))

# ----------------------------
# Output folder
# ----------------------------

# out_dir, target_crs, map_dir, table_dir come from 00_config.R -- do not hardcode.

# ----------------------------
# 13A. Clean variable labels
# ----------------------------

var_labels <- c(
  swb_z = "Subjective well-being index, standardized",
  belonging_z = "Neighborhood belonging index, standardized",
  be_housing_z = "Housing and household context index, standardized",
  be_mobility_z = "Mobility index, standardized",
  be_public_space_z = "Public-space engagement index, standardized",
  be_safety_z = "Neighborhood safety/crime concern index, standardized",
  age = "Age",
  gender = "Gender",
  marital = "Marital status",
  children = "Number of children",
  hispanic = "Hispanic",
  race = "Race",
  imm_status = "Immigration status",
  income_hh = "Household income",
  edu_level = "Education level",
  engl_speak = "English-speaking proficiency",
  time_live_denver = "Years lived in Denver area",
  time_live_hood = "Years lived in current neighborhood"
)

# Apply labels where variables exist
for (v in names(var_labels)) {
  if (v %in% names(model_dat)) {
    labelled::var_label(model_dat[[v]]) <- var_labels[[v]]
  }
}

# ----------------------------
# 13B. Descriptive statistics table
# ----------------------------

desc_vars <- c(
  "swb_z",
  "belonging_z",
  be_indices_z,
  "age",
  "children",
  "income_hh",
  "edu_level",
  "engl_speak",
  "time_live_denver",
  "time_live_hood",
  "gender",
  "marital",
  "hispanic",
  "race",
  "imm_status"
)

desc_vars <- desc_vars[desc_vars %in% names(model_dat)]

desc_table <- model_dat %>%
  dplyr::select(dplyr::all_of(desc_vars)) %>%
  gtsummary::tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing = "ifany"
  ) %>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  gtsummary::modify_caption("**Table 1. Descriptive statistics**") %>%
  gtsummary::bold_labels()

desc_flex <- gtsummary::as_flex_table(desc_table)

flextable::save_as_docx(
  desc_flex,
  path = file.path(out_dir, "Table1_Descriptive_Statistics.docx")
)

# ----------------------------
# 13C. Model results table
# ----------------------------

model_table <- modelsummary::modelsummary(
  models,
  output = "flextable",
  statistic = "({std.error})",
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  coef_map = c(
    "belonging_z" = "Neighborhood belonging",
    "be_housing_z" = "Housing and household context",
    "be_mobility_z" = "Mobility",
    "be_public_space_z" = "Public-space engagement",
    "be_safety_z" = "Neighborhood safety/crime concern",
    "age" = "Age",
    "children" = "Number of children",
    "income_hh" = "Household income",
    "edu_level" = "Education level",
    "engl_speak" = "English-speaking proficiency",
    "time_live_denver" = "Years lived in Denver area",
    "time_live_hood" = "Years lived in current neighborhood"
  ),
  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"
  ),
  notes = "Standard errors in parentheses. * p < 0.05, ** p < 0.01, *** p < 0.001."
)

model_table <- model_table %>%
  flextable::autofit() %>%
  flextable::set_caption("Table 2. Regression models predicting subjective well-being and neighborhood belonging")

flextable::save_as_docx(
  model_table,
  path = file.path(out_dir, "Table2_Model_Results.docx")
)

# ----------------------------
# 13D. Formal mediation table
# ----------------------------

mediation_table_clean <- mediation_table %>%
  dplyr::mutate(
    predictor = dplyr::recode(
      predictor,
      "be_public_space_z" = "Public-space engagement",
      "be_housing_z" = "Housing and household context",
      "be_mobility_z" = "Mobility",
      "be_safety_z" = "Neighborhood safety/crime concern"
    )
  ) %>%
  dplyr::select(
    predictor,
    acme_estimate, acme_ci_low, acme_ci_high, acme_p,
    ade_estimate, ade_ci_low, ade_ci_high, ade_p,
    total_effect, total_ci_low, total_ci_high, total_p,
    prop_mediated
  ) %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~ round(.x, 3))
  )

mediation_flex <- flextable::flextable(mediation_table_clean) %>%
  flextable::set_header_labels(
    predictor = "Predictor",
    acme_estimate = "ACME",
    acme_ci_low = "ACME CI low",
    acme_ci_high = "ACME CI high",
    acme_p = "ACME p",
    ade_estimate = "ADE",
    ade_ci_low = "ADE CI low",
    ade_ci_high = "ADE CI high",
    ade_p = "ADE p",
    total_effect = "Total effect",
    total_ci_low = "Total CI low",
    total_ci_high = "Total CI high",
    total_p = "Total p",
    prop_mediated = "Proportion mediated"
  ) %>%
  flextable::set_caption("Table 3. Formal mediation analyses") %>%
  flextable::autofit()

flextable::save_as_docx(
  mediation_flex,
  path = file.path(out_dir, "Table3_Mediation_Results.docx")
)

# ----------------------------
# 13E. Combined Word document
# ----------------------------

doc <- officer::read_docx()

doc <- doc %>%
  officer::body_add_par("Table 1. Descriptive Statistics", style = "heading 1") %>%
  flextable::body_add_flextable(desc_flex) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_par("Table 2. Regression Model Results", style = "heading 1") %>%
  flextable::body_add_flextable(model_table) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_par("Table 3. Formal Mediation Results", style = "heading 1") %>%
  flextable::body_add_flextable(mediation_flex)

print(
  doc,
  target = file.path(out_dir, "SWB_Word_Ready_Tables.docx")
)
