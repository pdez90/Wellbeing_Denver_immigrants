# =============================================================================
# 32_robustness_v3.R
# Robustness checks for the v3 full models (supplement only).
#
#   INPUT : v3_models.rds (31_parallel_models.R)
#   OUTPUT: v3_buffer_sensitivity.csv   buffer measures at 400 / 800 / 1,600 m
#           v3_standard_errors.csv      OLS, cluster (tract, block group), Conley 1/2/5 km
#           v3_robust_mm.csv            OLS vs MM-estimator
#           v3_bootstrap.csv            percentile bootstrap CIs, 2,000 resamples
#
# Each check re-estimates the two full models (all four NBE domains, both
# outcomes) on the common sample of 31, changing one thing at a time.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
source("wb_domains_v3.R")
wb_require(c("dplyr", "readr", "tidyr", "sandwich", "MASS"))

obj <- readRDS(file.path(out_dir, "v3_models.rds"))
dat <- obj$frame; spec <- obj$spec
controls <- c(spec$controls_ind, spec$controls_nbhd)
nbe <- spec$all_nbe
fit_lm <- function(y, rhs, d) stats::lm(stats::reformulate(rhs, response = y), data = d)
cellfmt <- function(b, se, p) sprintf("%.3f%s (%.3f)", b, ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "+", "")))), se)

# ---- 1. Buffer radius ----------------------------------------------------------
# Every buffer-derived measure is swapped to its 400 m or 1,600 m version at
# once; the sample and all other terms are held fixed. Park acreage exists only
# at 800 m and stays. The alternative-radius columns were standardized on the
# same analytic sample as the 800 m ones (v3_prepare.R), so the three columns
# of the table are in the same units.
rows <- list()
for (r in c(400, 800, 1600)) {
  d <- dat
  rhs <- nbe
  for (v in names(V3_BUFFER_VARS)) {
    alt <- sprintf(V3_BUFFER_VARS[[v]], r)
    if (alt == v) next
    if (v %in% V3_BINARY) { d[[v]] <- d[[alt]] } else { d[[paste0(v, "_z")]] <- d[[paste0(alt, "_z")]] }
  }
  for (oc in names(V3_OUTCOMES)) {
    m <- fit_lm(V3_OUTCOMES[[oc]], c(controls, rhs), d)
    if (stats::nobs(m) != nrow(dat)) stop("buffer check: sample changed at ", r, " m")
    cf <- summary(m)$coefficients
    for (v in names(V3_BUFFER_VARS)) {
      tm <- if (v %in% V3_BINARY) v else paste0(v, "_z")
      rows[[length(rows) + 1]] <- data.frame(outcome = oc, measure = sub(" within 800 m", " within buffer", v3_label(v)), radius_m = r,
        n = stats::nobs(m), estimate = cf[tm, 1], std_error = cf[tm, 2], p_value = cf[tm, 4],
        cell = cellfmt(cf[tm, 1], cf[tm, 2], cf[tm, 4]), adj_r2 = summary(m)$adj.r.squared)
    }
  }
}
buf <- dplyr::bind_rows(rows)
readr::write_csv(buf, file.path(out_dir, "v3_buffer_sensitivity.csv"))

# ---- 2. Clustered and spatial standard errors ---------------------------------
conley_vcov <- function(model, lon, lat, cutoff_km) {
  X <- stats::model.matrix(model); u <- as.numeric(stats::residuals(model))
  R <- 6371; rad <- pi / 180; la <- lat * rad; lo <- lon * rad
  dlat <- outer(la, la, "-"); dlon <- outer(lo, lo, "-")
  a <- sin(dlat / 2)^2 + outer(cos(la), cos(la), "*") * sin(dlon / 2)^2
  D <- 2 * R * asin(pmin(1, sqrt(a)))
  K <- pmax(0, 1 - D / cutoff_km)
  meat <- crossprod(X, (K * outer(u, u, "*")) %*% X)
  bread <- chol2inv(chol(crossprod(X)))
  V <- bread %*% meat %*% bread; dimnames(V) <- list(colnames(X), colnames(X)); V
}
se_rows <- list()
for (oc in names(V3_OUTCOMES)) {
  m <- obj$models[[paste0(oc, ":full")]]
  V <- list(ols = stats::vcov(m),
            tract = sandwich::vcovCL(m, cluster = dat$tract_geoid, type = "HC1"),
            bg = sandwich::vcovCL(m, cluster = dat$bg_geoid, type = "HC1"),
            c1 = conley_vcov(m, dat$longitude, dat$latitude, 1),
            c2 = conley_vcov(m, dat$longitude, dat$latitude, 2),
            c5 = conley_vcov(m, dat$longitude, dat$latitude, 5))
  b <- stats::coef(m); df <- m$df.residual
  for (tm in c(nbe, spec$controls_nbhd)) {
    se <- vapply(V, function(v) sqrt(v[tm, tm]), numeric(1))
    p  <- 2 * stats::pt(abs(b[tm] / se), df, lower.tail = FALSE)
    se_rows[[length(se_rows) + 1]] <- data.frame(outcome = oc, term = tm, label = v3_label(tm), estimate = b[tm],
      se_ols = se["ols"], se_cluster_tract = se["tract"], se_cluster_bg = se["bg"],
      se_conley_1km = se["c1"], se_conley_2km = se["c2"], se_conley_5km = se["c5"],
      p_ols = p["ols"], p_cluster_tract = p["tract"], p_conley_2km = p["c2"])
  }
}
ses <- dplyr::bind_rows(se_rows); rownames(ses) <- NULL
readr::write_csv(ses, file.path(out_dir, "v3_standard_errors.csv"))

# ---- 3. MM-estimator -----------------------------------------------------------
# rlm(method = "MM") starts from a random-subsample S-estimate, so the seed
# must be set immediately before it for the table to be reproducible.
set.seed(WB_SEED)
mm_rows <- list()
for (oc in names(V3_OUTCOMES)) {
  m  <- obj$models[[paste0(oc, ":full")]]
  mm <- MASS::rlm(stats::formula(m), data = dat, method = "MM", maxit = 200)
  so <- summary(m)$coefficients; sm <- summary(mm)$coefficients
  for (tm in c(nbe, spec$controls_nbhd)) {
    p_mm <- 2 * stats::pt(abs(sm[tm, 3]), m$df.residual, lower.tail = FALSE)
    mm_rows[[length(mm_rows) + 1]] <- data.frame(outcome = oc, term = tm, label = v3_label(tm), n = nrow(dat),
      ols = cellfmt(so[tm, 1], so[tm, 2], so[tm, 4]), robust_mm = cellfmt(sm[tm, 1], sm[tm, 2], p_mm),
      ols_est = so[tm, 1], mm_est = sm[tm, 1], change = sm[tm, 1] - so[tm, 1])
  }
}
mmt <- dplyr::bind_rows(mm_rows)
readr::write_csv(mmt, file.path(out_dir, "v3_robust_mm.csv"))

# ---- 4. Nonparametric bootstrap ------------------------------------------------
set.seed(WB_SEED)
B <- 2000
boot_rows <- list()
for (oc in names(V3_OUTCOMES)) {
  m <- obj$models[[paste0(oc, ":full")]]; fm <- stats::formula(m)
  terms_keep <- c(nbe, spec$controls_nbhd)
  draws <- matrix(NA_real_, B, length(terms_keep), dimnames = list(NULL, terms_keep))
  for (b in seq_len(B)) {
    idx <- sample.int(nrow(dat), replace = TRUE)
    cb <- stats::coef(stats::lm(fm, data = dat[idx, ]))
    draws[b, ] <- cb[terms_keep]
  }
  est <- stats::coef(m)[terms_keep]
  for (tm in terms_keep) {
    q <- stats::quantile(draws[, tm], c(.025, .975), na.rm = TRUE)
    boot_rows[[length(boot_rows) + 1]] <- data.frame(outcome = oc, term = tm, label = v3_label(tm),
      estimate = est[tm], boot_se = stats::sd(draws[, tm], na.rm = TRUE),
      ci_low = q[[1]], ci_high = q[[2]], excludes_zero = (q[[1]] > 0 | q[[2]] < 0),
      ols_p = summary(m)$coefficients[tm, 4])
  }
}
bt <- dplyr::bind_rows(boot_rows); rownames(bt) <- NULL
readr::write_csv(bt, file.path(out_dir, "v3_bootstrap.csv"))

# ---- 5. Console summary --------------------------------------------------------
cat("\n== Buffer radius: NBE terms with p < .05 at any radius ==\n")
print(buf %>% dplyr::filter(measure %in% unique(buf$measure[buf$p_value < .05])) %>%
        dplyr::select(outcome, measure, radius_m, cell) %>%
        tidyr::pivot_wider(names_from = radius_m, values_from = cell) %>% as.data.frame(), row.names = FALSE)
cat("\n== SE ratio Conley 2 km / OLS: ", sprintf("%.2f-%.2f (median %.2f)", min(ses$se_conley_2km / ses$se_ols),
    max(ses$se_conley_2km / ses$se_ols), median(ses$se_conley_2km / ses$se_ols)), "\n")
flip <- ses %>% dplyr::filter((p_ols < .05) != (p_conley_2km < .05) | (p_ols < .05) != (p_cluster_tract < .05))
cat("Terms whose p < .05 status changes under tract clustering or Conley 2 km:\n"); print(flip[, c("outcome", "label", "p_ols", "p_cluster_tract", "p_conley_2km")], row.names = FALSE)
cat("\n== MM: largest changes ==\n"); print(mmt %>% dplyr::arrange(dplyr::desc(abs(change))) %>% dplyr::select(outcome, label, ols, robust_mm) %>% head(8), row.names = FALSE)
cat("\n== Bootstrap vs OLS: disagreements on p < .05 / CI excludes 0 ==\n")
print(bt %>% dplyr::filter((ols_p < .05) != excludes_zero) %>% dplyr::select(outcome, label, estimate, ci_low, ci_high, ols_p), row.names = FALSE)
cat("\nWrote v3_buffer_sensitivity.csv, v3_standard_errors.csv, v3_robust_mm.csv, v3_bootstrap.csv\n")
