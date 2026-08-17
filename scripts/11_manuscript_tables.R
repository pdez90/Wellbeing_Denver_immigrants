# =============================================================================
# 11_manuscript_tables.R
# Builds Tables 1-10 exactly as they appear in the manuscript.
#
#   INPUT : respondents_with_transport_ht_segpoi_stoz_diversity.csv
#           model_objects.rds  (from 08_domain_models.R)
#   OUTPUT: manuscript_tables/Table01.csv ... Table10.csv
#           manuscript_tables/Manuscript_Tables.docx  (all ten, landscape)
#
# The regression tables in 08_domain_models.R are diagnostic views of each
# model set. This script assembles the same models into the numbered,
# publication-ordered tables the manuscript refers to, with readable variable
# labels and the goodness-of-fit rows journals expect.
#
# Table numbering below is the numbering of the model tables as a set. The
# submitted manuscript keeps five items in the main text and moves the rest to
# the Supplementary Information, so the mapping is:
#
#   Table 1  -> main text Table 1        Table 6  -> SI Table S18
#   Table 2  -> SI Table S1              Table 7  -> SI Table S19
#   Table 3  -> SI Table S14             Table 8  -> main text Table 2
#   Table 4  -> SI Table S15             Table 9  -> main text Table 3
#   Table 5  -> SI Table S17             Table 10 -> SI Table S16
#
# The SI has two sections: Section S1 is descriptive statistics for every
# analysis variable (Tables S1-S13, Figures S1-S9, from 13_descriptives_all.R)
# and Section S2 is the full model output (Tables S14-S19). Main text also
# carries Figure 1 (12_figure_mediation_dag.R) and Figure 2
# (14_figure_coefficients.R). This mapping is written to
# manuscript_tables/table_numbering.csv as well, so it is machine-readable.
#
# Table numbering:
#   1  Sample characteristics and outcome variables
#   2  Neighborhood and built environment characteristics
#   3  Individual characteristics and neighborhood socioeconomic context
#   4  Built environment domains predicting subjective well-being
#   5  Built environment domains predicting neighborhood belonging
#   6  Land use and regulation domain models
#   7  Domains predicting SWB, adjusting for belonging
#   8  Final parsimonious models
#   9  Formal causal mediation analyses
#  10  Greenness measures: specification sensitivity
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
wb_require(c("tidyverse", "janitor", "readr", "car", "flextable", "officer"))

out_tab <- file.path(out_dir, "manuscript_tables")
dir.create(out_tab, showWarnings = FALSE, recursive = TRUE)

obj <- readRDS(file.path(out_dir, "model_objects.rds"))
model_dat           <- obj$model_dat
individual_controls <- obj$individual_controls
context_z           <- obj$context_z
domain_z            <- obj$domain_z
final_be            <- obj$final_be

set.seed(WB_SEED)

# -----------------------------------------------------------------------------
# Readable labels
# -----------------------------------------------------------------------------
if (!exists("WB_LABELS")) source("wb_labels.R")
LBL <- WB_LABELS
lab <- function(x) ifelse(x %in% names(LBL), LBL[x], x)

stars <- function(p) ifelse(is.na(p), "",
  ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .10, "+", "")))))

# Coefficient table: estimate row + SE row per term, then fit statistics.
coef_table <- function(models, headers) {
  terms <- unique(unlist(lapply(models, function(m) rownames(summary(m)$coefficients))))
  terms <- setdiff(terms, "(Intercept)")
  rows <- list()
  for (tm in terms) {
    est <- se <- character(0)
    for (m in models) {
      co <- summary(m)$coefficients
      if (tm %in% rownames(co)) {
        est <- c(est, paste0(sprintf("%.3f", co[tm, 1]), stars(co[tm, 4])))
        se  <- c(se,  sprintf("(%.3f)", co[tm, 2]))
      } else { est <- c(est, ""); se <- c(se, "") }
    }
    rows[[length(rows) + 1]] <- c(lab(tm), est)
    rows[[length(rows) + 1]] <- c("", se)
  }
  rows[[length(rows) + 1]] <- c("Observations", vapply(models, function(m) format(length(stats::residuals(m))), character(1)))
  rows[[length(rows) + 1]] <- c("R²",          vapply(models, function(m) sprintf("%.3f", summary(m)$r.squared), character(1)))
  rows[[length(rows) + 1]] <- c("Adjusted R²", vapply(models, function(m) sprintf("%.3f", summary(m)$adj.r.squared), character(1)))
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c(" ", headers)
  out
}

# -----------------------------------------------------------------------------
# Tables 1-2: descriptive statistics
# -----------------------------------------------------------------------------
raw <- readr::read_csv(analysis_file, show_col_types = FALSE) %>% janitor::clean_names()
rn <- c(year_born="q3_1", gender="q3_2", marital="x2", children="q3_4", hispanic="q3_5",
        race="q3_6", imm_status="q3_7", income_hh="q3_10", engl_speak="q3_11_1",
        edu_level="q3_14", time_live_denver="q5_9", time_live_hood="q5_10",
        s1="q8_1_1", s2="q8_1_2", s3="q8_1_3", s4="q8_1_4", s5="q8_1_5", s6="q8_1_6",
        b1="q9_7_1", b2="q9_7_2", b3="q9_7_3", b4="q9_7_4")
for (nn in names(rn)) if (rn[[nn]] %in% names(raw)) names(raw)[names(raw) == rn[[nn]]] <- nn

rmean <- function(m) { r <- rowMeans(m, na.rm = TRUE); r[is.nan(r)] <- NA; r }
raw$swb <- rmean(sapply(raw[, paste0("s", 1:6)], to_num))
raw$bel <- rmean(sapply(raw[, paste0("b", 1:4)], to_num))
raw$age <- survey_year - to_num(raw$year_born)

cont <- c(swb = "Subjective well-being index", bel = "Neighborhood belonging index",
          age = "Age (years)", children = "Number of children",
          income_hh = "Household income (ordinal band)", edu_level = "Educational attainment (ordinal)",
          engl_speak = "English-speaking proficiency (1-5)",
          time_live_denver = "Years in Denver metro (ordinal)",
          time_live_hood = "Years in current neighborhood (ordinal)")
# Table 1 is split into two panels. A standard deviation, minimum and maximum
# are not defined for a categorical measure, so those cells carry an em dash
# rather than being left empty -- an empty cell reads as a missing number.
NA_CELL <- "\u2014"

rows1 <- list()
rows1[[1]] <- c("Panel A. Continuous and ordinal measures", "", "", "", "", "")
for (v in names(cont)) {
  x <- to_num(raw[[v]])
  rows1[[length(rows1) + 1]] <- c(cont[[v]], sum(!is.na(x)), sprintf("%.2f", mean(x, na.rm = TRUE)),
                                  sprintf("%.2f", sd(x, na.rm = TRUE)), sprintf("%.2f", min(x, na.rm = TRUE)),
                                  sprintf("%.2f", max(x, na.rm = TRUE)))
}
rows1[[length(rows1) + 1]] <- c("Panel B. Categorical measures", "", "", "", "", "")
add_cat <- function(v, labs, heading) {
  x <- to_num(raw[[v]]); tt <- table(x)
  rows1[[length(rows1) + 1]] <<- c(heading, "", "", "", "", "")
  for (k in names(tt))
    rows1[[length(rows1) + 1]] <<- c(paste0("   ", labs[[k]]), sum(!is.na(x)),
                                     sprintf("%.1f%%", 100 * tt[[k]] / sum(tt)),
                                     NA_CELL, NA_CELL, NA_CELL)
}
add_cat("gender", list("0"="Male","1"="Female"), "Gender")
add_cat("marital", list("0"="Single, never married","1"="Married","2"="Separated","3"="Divorced","4"="Widowed"), "Marital status")
add_cat("hispanic", list("0"="Hispanic","1"="Not Hispanic"), "Ethnicity")
add_cat("race", list("0"="African American","1"="Pacific Islander/Filipino/Hawaiian","2"="Asian",
                     "3"="Native American","4"="White","5"="Other"), "Race")
add_cat("imm_status", list("0"="Naturalized U.S. citizen","1"="Lawful permanent resident",
                           "2"="No official status (undocumented)","3"="DACA recipient","4"="Student visa",
                           "5"="Refugee status","6"="Asylee status","7"="Work visa"), "Immigration status")
T1 <- as.data.frame(do.call(rbind, rows1), stringsAsFactors = FALSE)
names(T1) <- c("Variable", "N", "Mean or %", "SD", "Minimum", "Maximum")

be_vars <- c(pop_density="Population density", housing_density="Housing density",
  dist_downtown_km="Distance to downtown Denver, km", pct_poverty="Percent below poverty",
  pct_non_native="Percent foreign-born", neighborhood_ses_index="Neighborhood SES index",
  walk_nat_walk_ind="EPA National Walkability Index",
  street_intdensity="Street intersection density (per sq mile)",
  urban_center_nearest_dist_m="Distance to nearest urban center, m",
  hudjob_jobs_idx="HUD Jobs Proximity Index", sidewalk_density_800="Sidewalk density within 800 m",
  bike_facility_density_800="Bicycle facility density within 800 m",
  active_corridor_density_800="Active transportation corridor density within 800 m",
  ht_t_ami="Transportation cost index", tree_tes="Tree Equity Score", tree_treecanopy="Tree canopy share",
  park_acres_half_mile="Park acres within 800 m", park_nearest_dist_m="Distance to nearest park, m",
  lc_800m_tree_canopy="Tree canopy land cover within 800 m",
  lc_800m_impervious_surfaces="Impervious surface within 800 m",
  crash_density_800="Crash density within 800 m", ped_crash_density_800="Pedestrian crash density within 800 m",
  bike_crash_density_800="Bicycle crash density within 800 m",
  short_trip_zone_share_800="Short-trip opportunity zone share within 800 m",
  div_total_diversity_resi="Residential diversity index", div_exposure_mean="Experienced diversity index")
rows2 <- list()
for (v in names(be_vars)) {
  if (!v %in% names(raw)) next
  x <- to_num(raw[[v]])
  rows2[[length(rows2) + 1]] <- c(be_vars[[v]], sum(!is.na(x)),
    sprintf("%.2f", mean(x, na.rm = TRUE)), sprintf("%.2f", sd(x, na.rm = TRUE)),
    sprintf("%.2f", min(x, na.rm = TRUE)), sprintf("%.2f", median(x, na.rm = TRUE)),
    sprintf("%.2f", max(x, na.rm = TRUE)))
}
T2 <- as.data.frame(do.call(rbind, rows2), stringsAsFactors = FALSE)
names(T2) <- c("Variable", "N", "Mean", "SD", "Min", "Median", "Max")

# -----------------------------------------------------------------------------
# Tables 3-8: regression models
# -----------------------------------------------------------------------------
IC <- individual_controls
dnames <- c("urban_form", "access_transport", "green_parks", "safety_social")
dhdr   <- c("Urban form", "Transportation", "Greenness & parks", "Safety & social")

T3 <- coef_table(list(
  make_lm("swb_z", IC, model_dat), make_lm("swb_z", c(IC, context_z), model_dat),
  make_lm("belonging_z", IC, model_dat), make_lm("belonging_z", c(IC, context_z), model_dat)),
  c("SWB: individual", "SWB: + context", "Belonging: individual", "Belonging: + context"))

T4 <- coef_table(lapply(dnames, function(d) make_lm("swb_z", unique(c(IC, context_z, domain_z[[d]])), model_dat)), dhdr)
T5 <- coef_table(lapply(dnames, function(d) make_lm("belonging_z", unique(c(IC, context_z, domain_z[[d]])), model_dat)), dhdr)
T7 <- coef_table(lapply(dnames, function(d) make_lm("swb_z", unique(c("belonging_z", IC, context_z, domain_z[[d]])), model_dat)), dhdr)

lu <- domain_z$land_use
T6 <- coef_table(list(
  make_lm("swb_z", unique(c(IC, context_z, lu)), model_dat),
  make_lm("belonging_z", unique(c(IC, context_z, lu)), model_dat),
  make_lm("swb_z", unique(c("belonging_z", IC, context_z, lu)), model_dat)),
  c("SWB", "Belonging", "SWB + belonging"))

T8 <- coef_table(list(
  make_lm("swb_z", unique(c(IC, context_z, final_be)), model_dat),
  make_lm("belonging_z", unique(c(IC, context_z, final_be)), model_dat),
  make_lm("swb_z", unique(c("belonging_z", IC, context_z, final_be)), model_dat)),
  c("SWB", "Belonging", "SWB + belonging"))

# -----------------------------------------------------------------------------
# Table 9: mediation (read from 09_mediation.R output)
# -----------------------------------------------------------------------------
med_file <- file.path(out_dir, "formal_mediation_results_domain_models.csv")
if (!file.exists(med_file)) stop("Run 09_mediation.R before this script: ", med_file, " not found.")
med <- readr::read_csv(med_file, show_col_types = FALSE)
dom_lab <- c(urban_form = "Urban form", access_transport = "Transportation",
             green_parks = "Greenness and parks", safety_social = "Safety and environment",
             land_use = "Land use")
T9 <- med %>%
  dplyr::transmute(
    Domain = dplyr::recode(domain, !!!dom_lab, .default = domain),
    Predictor = lab(predictor), N = n,
    `Indirect effect (ACME)` = sprintf("%.3f", acme),
    `95% CI` = sprintf("(%.3f, %.3f)", acme_ci_low, acme_ci_high),
    `ACME p` = sprintf("%.3f", acme_p), `ACME p (Holm)` = sprintf("%.3f", acme_p_holm),
    `Direct effect (ADE)` = sprintf("%.3f", ade), `ADE p` = sprintf("%.3f", ade_p),
    `Total effect` = sprintf("%.3f", total_effect), `Total p` = sprintf("%.3f", total_p)) %>%
  as.data.frame()

# -----------------------------------------------------------------------------
# Table 10: greenness sensitivity
# -----------------------------------------------------------------------------
specs <- list(
  "A. Both Tree Equity measures" = c("tree_tes_z","tree_treecanopy_z","park_acres_half_mile_z",
    "park_nearest_dist_m_z","lc_800m_tree_canopy_z","lc_800m_impervious_surfaces_z"),
  "B. Tree Equity Score only" = c("tree_tes_z","park_acres_half_mile_z","park_nearest_dist_m_z",
    "lc_800m_tree_canopy_z","lc_800m_impervious_surfaces_z"),
  "C. Tree Equity canopy only" = c("tree_treecanopy_z","park_acres_half_mile_z","park_nearest_dist_m_z",
    "lc_800m_tree_canopy_z","lc_800m_impervious_surfaces_z"),
  "D. Land-cover canopy only" = c("park_acres_half_mile_z","park_nearest_dist_m_z",
    "lc_800m_tree_canopy_z","lc_800m_impervious_surfaces_z"))
max_green_vif <- function(m, gv) {
  v <- try(car::vif(m), silent = TRUE); if (inherits(v, "try-error")) return(NA_real_)
  vv <- if (is.matrix(v)) stats::setNames(v[, "GVIF"]^(1 / v[, "Df"]), rownames(v)) else stats::setNames(as.numeric(v), names(v))
  # car::vif() already returns the variance inflation factor for single-df
  # terms, and the matrix branch above has converted GVIF to the comparable
  # scale. Squaring it here was a bug: it reported 179.3 where 08_domain_models.R
  # reports 13.4 for the same model.
  hit <- vv[names(vv) %in% gv]; if (!length(hit)) NA_real_ else max(hit)
}
rows10 <- list()
for (oc in c("belonging_z", "swb_z")) for (nm in names(specs)) {
  m <- make_lm(oc, unique(c(IC, context_z, specs[[nm]])), model_dat)
  s <- summary(m); co <- s$coefficients
  for (k in specs[[nm]]) if (k %in% rownames(co))
    rows10[[length(rows10) + 1]] <- c(
      ifelse(oc == "belonging_z", "Neighborhood belonging", "Subjective well-being"), nm, lab(k),
      sprintf("%.3f (%.3f)%s", co[k, 1], co[k, 2], stars(co[k, 4])),
      format(length(stats::residuals(m))), sprintf("%.3f", s$adj.r.squared),
      sprintf("%.1f", max_green_vif(m, specs[[nm]])))
}
T10 <- as.data.frame(do.call(rbind, rows10), stringsAsFactors = FALSE)
names(T10) <- c("Outcome", "Specification", "Predictor", "Estimate", "N", "Adj. R²", "Max greenness VIF")

# -----------------------------------------------------------------------------
# Write
# -----------------------------------------------------------------------------
TABLES <- list(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)
TITLES <- c(
  "Sample Characteristics and Outcome Variables",
  "Neighborhood and Built Environment Characteristics",
  "Individual Characteristics and Neighborhood Socioeconomic Context",
  "Built Environment Domains Predicting Subjective Well-Being",
  "Built Environment Domains Predicting Neighborhood Belonging",
  "Land Use and Regulation Domain Models",
  "Built Environment Domains Predicting Subjective Well-Being, Adjusting for Neighborhood Belonging",
  "Final Parsimonious Models",
  "Formal Causal Mediation Analyses of Neighborhood Belonging",
  "Greenness Measures: Specification Sensitivity")
NOTE_STARS <- paste("Standardized coefficients with standard errors in parentheses.",
  "+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001.",
  "Reference categories: never married, naturalized U.S. citizen.")
NOTES <- c(
  "Panel A reports the mean, standard deviation, minimum and maximum of each continuous or ordinal measure. Panel B reports the percentage of respondents in each category; a standard deviation, minimum and maximum are not defined for a categorical measure, and an em dash marks those cells. N is the number of respondents with a valid response to that item.",
  "Respondent-level exposures derived from geocoded residential locations. Buffer measures use the 800 m radius unless noted. Street intersection density is per square mile (ICPSR 38580).",
  NOTE_STARS, paste(NOTE_STARS, "All models additionally adjust for individual characteristics and neighborhood socioeconomic context."),
  paste(NOTE_STARS, "All models additionally adjust for individual characteristics and neighborhood socioeconomic context."),
  paste(NOTE_STARS, "Zoning category is tested jointly; see zoning_joint_tests.csv. Reference zoning category is residential low density."),
  NOTE_STARS, NOTE_STARS,
  "ACME = average causal mediation effect; ADE = average direct effect. Indirect effects estimated using 1,000 nonparametric bootstrap simulations with a fixed seed. Mediation models use the same covariate set and analytic samples as the domain models. Holm-adjusted p-values account for ten tested indirect effects.",
  "Sensitivity of the greenness domain to the collinearity between the Tree Equity Score and the canopy share from which it is derived (r = .89). Specification D recovers 66 respondents because Tree Equity coverage restricts the greenness analytic sample.")

for (i in seq_along(TABLES))
  readr::write_csv(TABLES[[i]], file.path(out_tab, sprintf("Table%02d.csv", i)))

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Tables", style = "heading 1")
for (i in seq_along(TABLES)) {
  ft <- flextable::flextable(TABLES[[i]]) %>%
    flextable::theme_booktabs() %>%
    flextable::fontsize(size = if (ncol(TABLES[[i]]) > 7) 7 else 8, part = "all") %>%
    flextable::bold(part = "header") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::autofit() %>%
    flextable::add_footer_lines(NOTES[i])
  if (ncol(TABLES[[i]]) > 1)
    ft <- flextable::align(ft, j = 2:ncol(TABLES[[i]]), align = "center", part = "all")
  doc <- officer::body_add_par(doc, sprintf("Table %d. %s", i, TITLES[i]), style = "heading 2")
  doc <- flextable::body_add_flextable(doc, ft)
  doc <- officer::body_add_par(doc, "")
}
print(doc, target = file.path(out_tab, "Manuscript_Tables.docx"))

# Machine-readable map from these table numbers to their place in the submitted
# manuscript, so the numbering can be checked without reading this file.
readr::write_csv(
  data.frame(
    table_here = 1:10,
    manuscript = c("Main text Table 1", "SI Table S1", "SI Table S14", "SI Table S15",
                   "SI Table S17", "SI Table S18", "SI Table S19", "Main text Table 2",
                   "Main text Table 3", "SI Table S16"),
    title = TITLES
  ),
  file.path(out_tab, "table_numbering.csv")
)

cat("\nWrote", length(TABLES), "tables to", out_tab, "\n")
for (i in seq_along(TABLES))
  cat(sprintf("  Table %-2d %-52s %3d rows x %d cols\n", i, TITLES[i], nrow(TABLES[[i]]), ncol(TABLES[[i]])))
