# =============================================================================
# 31_parallel_models.R
# The v3 analysis: parallel OLS models of subjective well-being (SWB) and
# neighborhood social connectedness (NSC) on the neighborhood built environment.
#
#   INPUT : respondents_v3_analysis.csv (30_rail_stations.R), wb_domains_v3.R
#   OUTPUT: v3_coefficients.csv      every coefficient of every model, long
#           v3_model_fit.csv         n, R2, adjusted R2, F per model
#           v3_vif.csv               VIFs for the full models
#           v3_models.rds            the lm objects and the analysis frame
#           v3_variants.csv          focal NBE coefficients under each variant
#
# DESIGN. For each outcome: a baseline with individual and neighborhood
# demographic controls only; one model per NBE domain (controls + that
# domain); and a full model with all four domains. Every model is estimated on
# the same respondents (complete on both outcomes, all controls and all NBE
# measures), so coefficients are comparable across models and outcomes.
# NSC is never a predictor of SWB: the two outcomes are treated as parallel.
#
# VARIANTS (open questions from the coauthors' checklist) re-estimate the full
# models with one change each; 33 tabulates them next to the main results:
#   urban_center  adds distance to the nearest regional urban center
#   resdiv        adds residential racial-ethnic diversity to the controls
#   rail_dist     distance to the nearest rail station instead of the 800 m flag
#   belong1       the single belonging item as the NSC outcome
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
source("v3_prepare.R")
wb_require(c("dplyr", "readr", "tidyr", "car", "broom"))

fit_lm <- function(outcome, rhs, data) {
  stopifnot(all(c(outcome, rhs) %in% names(data)))
  stats::lm(stats::reformulate(rhs, response = outcome), data = data)
}
controls <- c(v3_spec$controls_ind, v3_spec$controls_nbhd)

# ---- 1. Main models ------------------------------------------------------------
MODELS <- list()
for (oc in names(V3_OUTCOMES)) {
  y <- V3_OUTCOMES[[oc]]
  MODELS[[paste(oc, "controls", sep = ":")]] <- fit_lm(y, controls, v3_dat)
  for (dn in names(v3_spec$domains))
    MODELS[[paste(oc, dn, sep = ":")]] <- fit_lm(y, c(controls, v3_spec$domains[[dn]]), v3_dat)
  MODELS[[paste(oc, "full", sep = ":")]] <- fit_lm(y, c(controls, v3_spec$all_nbe), v3_dat)
}

tidy_all <- function(models, frame_label = "main") {
  dplyr::bind_rows(lapply(names(models), function(nm) {
    m <- models[[nm]]
    parts <- strsplit(nm, ":")[[1]]
    broom::tidy(m, conf.int = TRUE) %>%
      dplyr::mutate(model = nm, outcome = parts[1], block = parts[2], frame = frame_label,
                    n = stats::nobs(m), label = v3_label(term),
                    group = dplyr::case_when(
                      term == "(Intercept)" ~ "intercept",
                      term %in% v3_spec$controls_ind ~ "Individual characteristics",
                      term %in% c(v3_spec$controls_nbhd, v3_spec$variant$resdiv) ~ "Neighborhood demographic controls",
                      term == v3_spec$variant$urban_center ~ unname(V3_DOMAIN_LABELS["density_location"]),
                      term == v3_spec$variant$rail_dist ~ unname(V3_DOMAIN_LABELS["transport_infra"]),
                      TRUE ~ unname(V3_DOMAIN_LABELS[vapply(term, function(t) {
                        w <- names(Filter(function(v) t %in% v, v3_spec$domains)); if (length(w)) w[1] else NA_character_ }, character(1))]))) %>%
      dplyr::select(frame, model, outcome, block, group, term, label, estimate, std.error,
                    statistic, p.value, conf.low, conf.high, n)
  }))
}
fit_all <- function(models, frame_label = "main") {
  dplyr::bind_rows(lapply(names(models), function(nm) {
    s <- summary(models[[nm]]); parts <- strsplit(nm, ":")[[1]]
    data.frame(frame = frame_label, model = nm, outcome = parts[1], block = parts[2],
               n = stats::nobs(models[[nm]]), k = length(stats::coef(models[[nm]])) - 1,
               r2 = s$r.squared, adj_r2 = s$adj.r.squared,
               f = unname(s$fstatistic[1]), f_p = unname(stats::pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)))
  }))
}
coefs <- tidy_all(MODELS)
fit   <- fit_all(MODELS)

# ---- 2. Variants ---------------------------------------------------------------
VAR <- list()
sub_frames <- list()
# urban center and residential diversity: one added variable, sample restricted to its coverage
for (v in c("urban_center", "resdiv")) {
  add <- v3_spec$variant[[v]]
  d <- v3_dat[!is.na(v3_dat[[add]]), ]
  sub_frames[[v]] <- d
  for (oc in names(V3_OUTCOMES)) {
    rhs <- if (v == "resdiv") c(controls, add, v3_spec$all_nbe) else c(controls, v3_spec$all_nbe, add)
    VAR[[paste(oc, paste0("full_", v), sep = ":")]] <- fit_lm(V3_OUTCOMES[[oc]], rhs, d)
  }
}
# rail distance in place of the indicator
rhs_rd <- c(controls, setdiff(v3_spec$all_nbe, "rail_station_800"), v3_spec$variant$rail_dist)
for (oc in names(V3_OUTCOMES))
  VAR[[paste(oc, "full_rail_dist", sep = ":")]] <- fit_lm(V3_OUTCOMES[[oc]], rhs_rd, v3_dat)
sub_frames$rail_dist <- v3_dat
# single-item belonging as the NSC outcome (same respondents, same predictors)
d_b1 <- v3_dat[!is.na(v3_dat$belong1_z), ]
sub_frames$belong1 <- d_b1
VAR[["belong1:full_belong1"]] <- fit_lm("belong1_z", c(controls, v3_spec$all_nbe), d_b1)
VAR[["nsc:full_belong1"]]     <- fit_lm("nsc_z",     c(controls, v3_spec$all_nbe), d_b1)  # composite on the same rows

vcoefs <- tidy_all(VAR, "variant")
vfit   <- fit_all(VAR, "variant")

# ---- 3. VIFs for the full models -----------------------------------------------
vif_tab <- dplyr::bind_rows(lapply(c("swb:full", "nsc:full"), function(nm) {
  v <- car::vif(MODELS[[nm]])
  data.frame(model = nm, term = names(v), label = v3_label(names(v)), vif = as.numeric(v))
}))
for (nm in names(VAR)[grepl("urban_center|rail_dist", names(VAR))]) {
  v <- car::vif(VAR[[nm]])
  vif_tab <- dplyr::bind_rows(vif_tab, data.frame(model = nm, term = names(v), label = v3_label(names(v)), vif = as.numeric(v)))
}

# ---- 4. Outcome correlation and descriptives of the frame ----------------------
r_sn <- stats::cor.test(v3_dat$swb_z, v3_dat$nsc_z)
r_b1 <- stats::cor.test(d_b1$nsc_z, d_b1$belong1_z)
summ <- data.frame(
  quantity = c("n_common_sample", "n_surveyed", "n_geocoded", "n_both_outcomes",
               "alpha_swb", "alpha_nsc", "cor_swb_nsc", "cor_swb_nsc_p",
               "cor_nsc_belong1", "n_belong1", "n_urban_center", "n_resdiv",
               "share_rail_800", "n_rail_400", "n_rail_800", "n_rail_1600",
               "n_geocoded_both_outcomes", "n_geocoded_both_in_enviroscreen",
               "n_geocoded_with_env_v2join", "n_geocoded_with_env_spatial",
               "df_residual_full", "sd_swb_index", "sd_nsc_index", "mean_swb_index", "mean_nsc_index"),
  value = c(nrow(v3_dat), nrow(v3_full), sum(!is.na(v3_full$latitude)),
            v3_counts[["both_outcomes"]],
            v3_alpha[["swb"]], v3_alpha[["nsc"]], unname(r_sn$estimate), r_sn$p.value,
            unname(r_b1$estimate), nrow(d_b1), nrow(sub_frames$urban_center), nrow(sub_frames$resdiv),
            mean(v3_dat$rail_station_800), sum(v3_dat$rail_station_400), sum(v3_dat$rail_station_800), sum(v3_dat$rail_station_1600),
            v3_counts[["geo_both"]], v3_counts[["geo_both_env"]],
            sum(!is.na(v3_full$latitude) & !is.na(v3_full$env_pm25_v2)), sum(!is.na(v3_full$latitude) & !is.na(v3_full$env_pm25)),
            MODELS[["swb:full"]]$df.residual, stats::sd(v3_dat$swb_index), stats::sd(v3_dat$nsc_index),
            mean(v3_dat$swb_index), mean(v3_dat$nsc_index)))

# ---- 5. Write ------------------------------------------------------------------
all_coefs <- dplyr::bind_rows(coefs, vcoefs)
readr::write_csv(all_coefs, file.path(out_dir, "v3_coefficients.csv"))
readr::write_csv(dplyr::bind_rows(fit, vfit), file.path(out_dir, "v3_model_fit.csv"))
readr::write_csv(vif_tab, file.path(out_dir, "v3_vif.csv"))
readr::write_csv(summ, file.path(out_dir, "v3_summary.csv"))
saveRDS(list(models = MODELS, variants = VAR, frame = v3_dat, sub_frames = sub_frames,
             spec = v3_spec, alpha = v3_alpha),
        file.path(out_dir, "v3_models.rds"))

# ---- 6. Console summary --------------------------------------------------------
stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "+", ""))))
cat("\n== Full models, NBE terms (n =", nrow(v3_dat), ") ==\n")
show <- all_coefs %>% dplyr::filter(frame == "main", block == "full", !group %in% c("intercept", "Individual characteristics")) %>%
  dplyr::mutate(cell = sprintf("%6.3f%-3s (%.3f)", estimate, stars(p.value), std.error)) %>%
  dplyr::select(label, outcome, cell) %>% tidyr::pivot_wider(names_from = outcome, values_from = cell)
print(as.data.frame(show), row.names = FALSE)
cat("\nFit:\n"); print(fit[, c("model", "n", "adj_r2")], row.names = FALSE)
cat(sprintf("\nSWB-NSC correlation r = %.3f (p %s)\n", r_sn$estimate, format.pval(r_sn$p.value, digits = 2)))
cat(sprintf("Max VIF, full models: SWB %.2f, NSC %.2f\n",
            max(vif_tab$vif[vif_tab$model == "swb:full"]), max(vif_tab$vif[vif_tab$model == "nsc:full"])))
cat("\nWrote v3_coefficients.csv, v3_model_fit.csv, v3_vif.csv, v3_summary.csv, v3_models.rds\n")
