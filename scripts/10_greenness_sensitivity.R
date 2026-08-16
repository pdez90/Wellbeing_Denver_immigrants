# =============================================================================
# 10_greenness_sensitivity.R
# Resolves the collinearity between the two Tree Equity greenness measures.
#
#   INPUT : model_objects.rds  (written by 08_domain_models.R)
#   OUTPUT: greenness_sensitivity.csv / .docx
#
# WHY THIS EXISTS
# ---------------
# The greenness domain model enters tree_tes (Tree Equity Score) and
# tree_treecanopy (the canopy share the Score is computed from) together. They
# correlate at r = .89, with VIFs of 13.7 and 10.4, and they take opposite
# signs. That is the signature of two collinear predictors splitting a shared
# association, not of two distinct effects.
#
# The paper's largest built-environment coefficient -- Tree Equity Score
# predicting neighborhood belonging, beta = 0.468, p < .05 -- comes from that
# model. This script refits the greenness domain four ways to establish whether
# the finding is real or an artifact.
#
#   A. Both Tree Equity measures        (the current specification)
#   B. Tree Equity Score only
#   C. Tree Equity canopy share only
#   D. Neither -- keep only the independently measured DRCOG land-cover canopy
#
# Specification D also recovers 66 respondents (n = 304 vs 238), because Tree
# Equity coverage is what restricts the greenness sample.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
wb_require(c("tidyverse", "car", "flextable", "officer"))
source("wb_model_tables.R")

obj <- readRDS(file.path(out_dir, "model_objects.rds"))
model_dat           <- obj$model_dat
individual_controls <- obj$individual_controls
context_z           <- obj$context_z

specs <- list(
  "A. Both Tree Equity measures (current)" = c(
    "tree_tes_z", "tree_treecanopy_z", "park_acres_half_mile_z",
    "park_nearest_dist_m_z", "lc_800m_tree_canopy_z", "lc_800m_impervious_surfaces_z"),
  "B. Tree Equity Score only" = c(
    "tree_tes_z", "park_acres_half_mile_z",
    "park_nearest_dist_m_z", "lc_800m_tree_canopy_z", "lc_800m_impervious_surfaces_z"),
  "C. Tree Equity canopy only" = c(
    "tree_treecanopy_z", "park_acres_half_mile_z",
    "park_nearest_dist_m_z", "lc_800m_tree_canopy_z", "lc_800m_impervious_surfaces_z"),
  "D. Land-cover canopy only" = c(
    "park_acres_half_mile_z", "park_nearest_dist_m_z",
    "lc_800m_tree_canopy_z", "lc_800m_impervious_surfaces_z")
)

fit_spec <- function(outcome, greenness_vars) {
  make_lm(outcome, unique(c(individual_controls, context_z, greenness_vars)),
          model_dat, verbose = FALSE)
}

max_green_vif <- function(m, greenness_vars) {
  v <- try(car::vif(m), silent = TRUE)
  if (inherits(v, "try-error")) return(NA_real_)
  vv <- if (is.matrix(v)) stats::setNames(v[, "GVIF"]^(1 / v[, "Df"]), rownames(v))
        else stats::setNames(as.numeric(v), names(v))
  hit <- vv[names(vv) %in% greenness_vars]
  if (!length(hit)) NA_real_ else max(hit)
}

results <- purrr::map_dfr(c("belonging_z", "swb_z"), function(outcome) {
  purrr::imap_dfr(specs, function(gv, spec_name) {
    m <- fit_spec(outcome, gv)
    s <- summary(m); co <- s$coefficients
    purrr::map_dfr(gv, function(term) {
      if (!term %in% rownames(co)) return(NULL)
      tibble::tibble(
        outcome = outcome, spec = spec_name, term = term,
        estimate = co[term, 1], std_error = co[term, 2], p_value = co[term, 4],
        n = length(stats::residuals(m)),
        adj_r_squared = s$adj.r.squared,
        max_greenness_vif = max_green_vif(m, gv)
      )
    })
  })
})

results <- results %>%
  dplyr::mutate(
    stars = wb_stars(p_value),
    reported = sprintf("%.3f (%.3f)%s", estimate, std_error, stars)
  )

readr::write_csv(results, file.path(out_dir, "greenness_sensitivity.csv"))

# ---- console summary ---------------------------------------------------------
for (oc in c("belonging_z", "swb_z")) {
  cat("\n", strrep("=", 76), "\nOUTCOME: ", oc, "\n", strrep("=", 76), "\n", sep = "")
  sub <- results %>% dplyr::filter(outcome == oc)
  for (sp in names(specs)) {
    ss <- sub %>% dplyr::filter(spec == sp)
    if (!nrow(ss)) next
    cat(sprintf("\n%s\n   n = %d | adj R2 = %.3f | max greenness VIF = %.1f\n",
                sp, ss$n[1], ss$adj_r_squared[1], ss$max_greenness_vif[1]))
    for (i in seq_len(nrow(ss)))
      cat(sprintf("     %-32s %s\n", ss$term[i], ss$reported[i]))
  }
}

# ---- the headline check ------------------------------------------------------
tes_belong <- results %>%
  dplyr::filter(outcome == "belonging_z", term == "tree_tes_z") %>%
  dplyr::select(spec, estimate, std_error, p_value)

cat("\n", strrep("=", 76),
    "\nTree Equity Score -> belonging, across specifications\n",
    strrep("=", 76), "\n", sep = "")
print(as.data.frame(tes_belong), row.names = FALSE)

if (nrow(tes_belong) >= 2) {
  with_twin <- tes_belong$estimate[grepl("^A", tes_belong$spec)]
  alone     <- tes_belong$estimate[grepl("^B", tes_belong$spec)]
  cat(sprintf(
    "\nWith the collinear twin in the model: %.3f\nWithout it:                          %.3f\nRatio: %.1fx\n",
    with_twin, alone, with_twin / alone))
  cat("\nInterpretation: if the coefficient collapses once its collinear twin is\n",
      "removed, the original estimate was splitting a shared association rather\n",
      "than measuring an independent effect, and should not be reported.\n", sep = "")
}

# ---- Word table --------------------------------------------------------------
tab <- results %>%
  dplyr::mutate(Outcome = ifelse(outcome == "belonging_z",
                                 "Neighborhood belonging", "Subjective well-being")) %>%
  dplyr::select(Outcome, Specification = spec, Predictor = term,
                Estimate = reported, n, `Adj. R2` = adj_r_squared,
                `Max greenness VIF` = max_greenness_vif) %>%
  dplyr::mutate(`Adj. R2` = sprintf("%.3f", `Adj. R2`),
                `Max greenness VIF` = sprintf("%.1f", `Max greenness VIF`))

ft <- flextable::flextable(tab) %>%
  flextable::theme_booktabs() %>%
  flextable::fontsize(size = 8, part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::merge_v(j = c("Outcome", "Specification")) %>%
  flextable::autofit() %>%
  flextable::add_footer_lines(paste(
    "Sensitivity of the greenness domain models to the collinearity between the",
    "Tree Equity Score and the canopy share from which it is derived (r = .89).",
    "Standard errors in parentheses. + p < .10, * p < .05, ** p < .01, *** p < .001.",
    "All models adjust for individual characteristics and neighborhood",
    "socioeconomic context. Specification D recovers 66 respondents because Tree",
    "Equity coverage is what restricts the greenness analytic sample."
  ))

doc <- officer::read_docx() %>%
  officer::body_add_par("Greenness measures: specification sensitivity", style = "heading 1") %>%
  flextable::body_add_flextable(ft)
print(doc, target = file.path(out_dir, "greenness_sensitivity.docx"))

cat("\nWrote greenness_sensitivity.csv and .docx\n")
