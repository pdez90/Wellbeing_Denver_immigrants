# =============================================================================
# 16_robustness.R
# Four robustness checks on the reported results. None of them replaces the
# primary estimator; each asks whether a specific modelling choice is doing the
# work.
#
#   INPUT : model_objects.rds  (from 08_domain_models.R)
#   OUTPUT: robustness_common_sample.csv      SI Table S21
#           robustness_standard_errors.csv    SI Table S22
#           robustness_robust_regression.csv  SI Table S23
#           robustness_swb_leave_one_out.csv  SI Table S24
#
# 1. COMMON SAMPLE. Domain models run on 238 to 304 respondents because the
#    third-party sources have different coverage, so a difference between two
#    domains could be a difference between two sets of people. Every domain
#    model is refit on the intersection sample -- the respondents complete on
#    every domain -- so the only thing that varies is which measures are in the
#    model.
#
# 2. DEPENDENCE BETWEEN NEARBY RESPONDENTS. Respondents share census tracts and
#    live near one another, so the OLS assumption of independent errors is
#    questionable. Three alternatives are computed for the same point estimates:
#    cluster-robust standard errors by tract and by block group, and a spatial
#    heteroskedasticity- and autocorrelation-consistent (Conley) estimator that
#    lets any two respondents' residuals covary as a declining function of the
#    distance between their homes. Point estimates never change; only the
#    standard errors do.
#
# 3. OUTCOME SCALE. Both outcomes are means of bounded Likert items. An
#    MM-estimator (MASS::rlm) refits the headline models, down-weighting
#    outlying respondents, to check that OLS is not being driven by the tails.
#
# 4. ITEM OVERLAP. One subjective well-being item asks about feeling accepted,
#    which is conceptually close to the belonging scale that serves as the
#    mediator. The well-being index is rebuilt from the remaining five items and
#    the belonging models refit, to test whether the large belonging
#    coefficient is partly an artefact of shared item content.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
if (!exists("WB_LABELS")) source("wb_labels.R")
wb_require(c("tidyverse", "readr", "broom", "sandwich", "MASS"))

obj <- readRDS(file.path(out_dir, "model_objects.rds"))
if (is.null(obj$dat_derived)) stop("Re-run 08_domain_models.R: model_objects.rds is out of date.")

# 08_domain_models.R builds its analytic frame from respondents with both
# outcomes present, so the same filter is applied here; without it the samples
# below would be a few respondents larger than the models being checked.
dat  <- obj$dat_derived
dat  <- dat[!is.na(dat$swb_z) & !is.na(dat$belonging_z), , drop = FALSE]
IC   <- obj$individual_controls
CZ   <- obj$context_z
DZ   <- obj$domain_z
FBE  <- obj$final_be
set.seed(WB_SEED)

fit_lm <- function(outcome, rhs, data) {
  stats::lm(stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + "))),
            data = data, na.action = stats::na.exclude)
}
star <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**",
                   ifelse(p < .05, "*", ifelse(p < .10, "+", "")))))
cellfmt <- function(b, se, p) sprintf("%.3f%s (%.3f)", b, star(p), se)

# Terms the paper actually makes claims about.
HEADLINE <- c(
  walk_nat_walk_ind_z           = "EPA Walkability Index",
  bike_facility_density_800_z   = "Bicycle facility density",
  sidewalk_density_800_z        = "Sidewalk density",
  park_acres_half_mile_z        = "Park acreage within 800 m",
  lc_800m_tree_canopy_z         = "Tree canopy land cover",
  crash_density_800_z           = "Crash density",
  bike_crash_density_800_z      = "Bicycle crash density",
  belonging_z                   = "Neighborhood belonging",
  ind_undocumented              = "No official status (undocumented)",
  ind_hispanic                  = "Hispanic",
  pct_poverty_z                 = "Percent below poverty"
)

# -----------------------------------------------------------------------------
# 1. Common-sample sensitivity
# -----------------------------------------------------------------------------
all_domain_terms <- unique(unlist(DZ))
common_rhs <- unique(c(IC, CZ, all_domain_terms))
common_rhs <- common_rhs[common_rhs %in% names(dat)]
common_keep <- stats::complete.cases(dat[, c("swb_z", "belonging_z", common_rhs), drop = FALSE])
n_common <- sum(common_keep)
cat(sprintf("\nCommon-sample size (complete on every domain): %d\n", n_common))

rows1 <- list()
for (dn in names(DZ)) {
  rhs <- unique(c(IC, CZ, DZ[[dn]]))
  rhs <- rhs[rhs %in% names(dat)]
  for (oc in c("swb_z", "belonging_z")) {
    m_rep <- fit_lm(oc, rhs, dat)                       # as reported
    m_com <- fit_lm(oc, rhs, dat[common_keep, , drop = FALSE])
    for (tm in intersect(names(HEADLINE), rownames(summary(m_rep)$coefficients))) {
      a <- summary(m_rep)$coefficients[tm, ]
      b <- summary(m_com)$coefficients[tm, ]
      rows1[[length(rows1) + 1]] <- data.frame(
        domain = dn,
        outcome = ifelse(oc == "swb_z", "Subjective well-being", "Neighborhood belonging"),
        term = unname(HEADLINE[tm]),
        n_reported = length(stats::residuals(m_rep)[!is.na(stats::residuals(m_rep))]),
        reported = cellfmt(a[1], a[2], a[4]),
        n_common = n_common,
        common_sample = cellfmt(b[1], b[2], b[4]),
        change = sprintf("%+.3f", b[1] - a[1]),
        stringsAsFactors = FALSE)
    }
  }
}
# the integrated model as well
int_rhs <- unique(c(IC, CZ, FBE)); int_rhs <- int_rhs[int_rhs %in% names(dat)]
for (oc in c("swb_z", "belonging_z")) {
  m_rep <- fit_lm(oc, int_rhs, dat)
  m_com <- fit_lm(oc, int_rhs, dat[common_keep, , drop = FALSE])
  for (tm in intersect(names(HEADLINE), rownames(summary(m_rep)$coefficients))) {
    a <- summary(m_rep)$coefficients[tm, ]; b <- summary(m_com)$coefficients[tm, ]
    rows1[[length(rows1) + 1]] <- data.frame(
      domain = "integrated", outcome = ifelse(oc == "swb_z", "Subjective well-being", "Neighborhood belonging"),
      term = unname(HEADLINE[tm]),
      n_reported = length(stats::residuals(m_rep)[!is.na(stats::residuals(m_rep))]),
      reported = cellfmt(a[1], a[2], a[4]), n_common = n_common,
      common_sample = cellfmt(b[1], b[2], b[4]),
      change = sprintf("%+.3f", b[1] - a[1]), stringsAsFactors = FALSE)
  }
}
R1 <- do.call(rbind, rows1)
readr::write_csv(R1, file.path(out_dir, "robustness_common_sample.csv"))

# -----------------------------------------------------------------------------
# 2. Cluster-robust and spatially robust standard errors
# -----------------------------------------------------------------------------

# Conley-style spatial HAC. Residual covariance between two respondents is
# weighted by a Bartlett kernel in great-circle distance and falls to zero at
# `cutoff_km`, so nearby homes are allowed to share shocks.
conley_vcov <- function(model, lon, lat, cutoff_km) {
  X <- stats::model.matrix(model)
  u <- as.numeric(stats::residuals(model))
  ok <- !is.na(u)
  X <- X[ok, , drop = FALSE]; u <- u[ok]; lon <- lon[ok]; lat <- lat[ok]
  n <- nrow(X)
  R <- 6371                                        # earth radius, km
  rad <- pi / 180
  la <- lat * rad; lo <- lon * rad
  # great-circle distance matrix via the haversine formula
  dlat <- outer(la, la, "-"); dlon <- outer(lo, lo, "-")
  a <- sin(dlat / 2)^2 + outer(cos(la), cos(la), "*") * sin(dlon / 2)^2
  D <- 2 * R * asin(pmin(1, sqrt(a)))
  K <- pmax(0, 1 - D / cutoff_km)                  # Bartlett kernel
  meat <- crossprod(X, (K * outer(u, u, "*")) %*% X)
  bread <- chol2inv(chol(crossprod(X)))
  V <- bread %*% meat %*% bread
  dimnames(V) <- list(colnames(X), colnames(X))
  V
}

se_table <- function(model, data_rows, label, outcome) {
  cf <- stats::coef(model)
  keep <- intersect(names(HEADLINE), names(cf))
  if (!length(keep)) return(NULL)
  ok <- !is.na(stats::residuals(model))
  tract <- data_rows$tract_geoid[ok]
  bg    <- data_rows$bg_geoid[ok]
  V_ols <- stats::vcov(model)
  V_tr  <- sandwich::vcovCL(model, cluster = tract, type = "HC1")
  V_bg  <- sandwich::vcovCL(model, cluster = bg,    type = "HC1")
  V_c1  <- conley_vcov(model, data_rows$longitude[ok], data_rows$latitude[ok], 1)
  V_c2  <- conley_vcov(model, data_rows$longitude[ok], data_rows$latitude[ok], 2)
  V_c5  <- conley_vcov(model, data_rows$longitude[ok], data_rows$latitude[ok], 5)
  df <- stats::df.residual(model)
  pv <- function(V, tm) 2 * stats::pt(abs(cf[tm] / sqrt(V[tm, tm])), df, lower.tail = FALSE)
  out <- lapply(keep, function(tm) data.frame(
    model = label, outcome = outcome, term = unname(HEADLINE[tm]),
    estimate = sprintf("%.3f", cf[tm]),
    se_ols = sprintf("%.3f", sqrt(V_ols[tm, tm])),
    se_cluster_tract = sprintf("%.3f", sqrt(V_tr[tm, tm])),
    se_cluster_bg = sprintf("%.3f", sqrt(V_bg[tm, tm])),
    se_conley_1km = sprintf("%.3f", sqrt(V_c1[tm, tm])),
    se_conley_2km = sprintf("%.3f", sqrt(V_c2[tm, tm])),
    se_conley_5km = sprintf("%.3f", sqrt(V_c5[tm, tm])),
    p_ols = sprintf("%.3f", pv(V_ols, tm)),
    p_cluster_tract = sprintf("%.3f", pv(V_tr, tm)),
    p_conley_2km = sprintf("%.3f", pv(V_c2, tm)),
    stringsAsFactors = FALSE))
  do.call(rbind, out)
}

MODELS <- list()
for (dn in names(DZ)) {
  rhs <- unique(c(IC, CZ, DZ[[dn]])); rhs <- rhs[rhs %in% names(dat)]
  MODELS[[paste0("SWB: ", dn)]]       <- list(oc = "swb_z", rhs = rhs)
  MODELS[[paste0("Belonging: ", dn)]] <- list(oc = "belonging_z", rhs = rhs)
}
MODELS[["SWB: integrated"]]            <- list(oc = "swb_z", rhs = int_rhs)
MODELS[["Belonging: integrated"]]      <- list(oc = "belonging_z", rhs = int_rhs)
MODELS[["SWB: integrated + belonging"]] <- list(oc = "swb_z",
                                                rhs = unique(c("belonging_z", int_rhs)))

rows2 <- list()
for (nm in names(MODELS)) {
  sp <- MODELS[[nm]]
  used <- stats::complete.cases(dat[, c("swb_z", "belonging_z", sp$rhs), drop = FALSE])
  sub  <- dat[used, , drop = FALSE]
  m    <- fit_lm(sp$oc, sp$rhs, sub)
  tb   <- se_table(m, sub, nm, ifelse(sp$oc == "swb_z", "Subjective well-being",
                                      "Neighborhood belonging"))
  if (!is.null(tb)) rows2[[length(rows2) + 1]] <- tb
}
R2 <- do.call(rbind, rows2)
readr::write_csv(R2, file.path(out_dir, "robustness_standard_errors.csv"))

n_tr <- length(unique(dat$tract_geoid[stats::complete.cases(dat[, c("swb_z", "belonging_z", int_rhs)])]))
n_bg <- length(unique(dat$bg_geoid[stats::complete.cases(dat[, c("swb_z", "belonging_z", int_rhs)])]))
cat(sprintf("Clusters in the integrated-model sample: %d census tracts, %d block groups\n", n_tr, n_bg))

# -----------------------------------------------------------------------------
# 3. Robust regression
# -----------------------------------------------------------------------------
rows3 <- list()
for (nm in names(MODELS)) {
  sp <- MODELS[[nm]]
  used <- stats::complete.cases(dat[, c("swb_z", "belonging_z", sp$rhs), drop = FALSE])
  sub  <- dat[used, , drop = FALSE]
  m_ols <- fit_lm(sp$oc, sp$rhs, sub)
  f <- stats::as.formula(paste(sp$oc, "~", paste(sp$rhs, collapse = " + ")))
  m_rob <- try(MASS::rlm(f, data = sub, method = "MM", maxit = 100), silent = TRUE)
  if (inherits(m_rob, "try-error")) next
  co_o <- summary(m_ols)$coefficients
  co_r <- summary(m_rob)$coefficients
  for (tm in intersect(names(HEADLINE), rownames(co_o))) {
    if (!tm %in% rownames(co_r)) next
    p_r <- 2 * stats::pt(abs(co_r[tm, 3]), stats::df.residual(m_ols), lower.tail = FALSE)
    rows3[[length(rows3) + 1]] <- data.frame(
      model = nm, outcome = ifelse(sp$oc == "swb_z", "Subjective well-being", "Neighborhood belonging"),
      term = unname(HEADLINE[tm]), n = nrow(sub),
      ols = cellfmt(co_o[tm, 1], co_o[tm, 2], co_o[tm, 4]),
      robust_mm = cellfmt(co_r[tm, 1], co_r[tm, 2], p_r),
      stringsAsFactors = FALSE)
  }
}
R3 <- do.call(rbind, rows3)
readr::write_csv(R3, file.path(out_dir, "robustness_robust_regression.csv"))

# -----------------------------------------------------------------------------
# 4. Well-being scale without the "feels accepted" item
# -----------------------------------------------------------------------------
swb_items <- obj$swb_items
drop_item <- "feel_accepted"
if (!drop_item %in% swb_items) stop("expected '", drop_item, "' among the SWB items")
reduced <- setdiff(swb_items, drop_item)

itm <- sapply(dat[, swb_items, drop = FALSE], to_num)
itm_red <- sapply(dat[, reduced, drop = FALSE], to_num)
idx_red <- rowMeans(itm_red, na.rm = TRUE); idx_red[is.nan(idx_red)] <- NA
dat$swb_z_drop <- zscore(idx_red)

alpha_of <- function(M) {
  M <- M[stats::complete.cases(M), , drop = FALSE]
  k <- ncol(M); v <- sum(diag(stats::var(M))); tot <- stats::var(rowSums(M))
  (k / (k - 1)) * (1 - v / tot)
}
cat(sprintf("Cronbach's alpha, 6 items = %.3f; 5 items without '%s' = %.3f\n",
            alpha_of(itm), drop_item, alpha_of(itm_red)))
cat(sprintf("Correlation between the two well-being indices: r = %.3f\n",
            stats::cor(dat$swb_z, dat$swb_z_drop, use = "complete.obs")))

rows4 <- list()
for (nm in names(MODELS)) {
  sp <- MODELS[[nm]]
  if (sp$oc != "swb_z") next
  rhs <- unique(c("belonging_z", sp$rhs))
  used <- stats::complete.cases(dat[, c("swb_z", "swb_z_drop", "belonging_z", rhs), drop = FALSE])
  sub  <- dat[used, , drop = FALSE]
  m_full <- fit_lm("swb_z", rhs, sub)
  m_drop <- fit_lm("swb_z_drop", rhs, sub)
  a <- summary(m_full)$coefficients["belonging_z", ]
  b <- summary(m_drop)$coefficients["belonging_z", ]
  rows4[[length(rows4) + 1]] <- data.frame(
    model = nm, n = nrow(sub),
    belonging_full_scale = cellfmt(a[1], a[2], a[4]),
    belonging_five_items = cellfmt(b[1], b[2], b[4]),
    change = sprintf("%+.3f", b[1] - a[1]),
    adj_r2_full = sprintf("%.3f", summary(m_full)$adj.r.squared),
    adj_r2_five = sprintf("%.3f", summary(m_drop)$adj.r.squared),
    stringsAsFactors = FALSE)
}
R4 <- do.call(rbind, rows4)
readr::write_csv(R4, file.path(out_dir, "robustness_swb_leave_one_out.csv"))

# -----------------------------------------------------------------------------
cat("\n== Robustness summary ==\n")
cat(sprintf("1. Common sample: %d respondents; largest coefficient change %s\n",
            n_common, sprintf("%.3f", max(abs(as.numeric(R1$change))))))
se_ratio <- as.numeric(R2$se_conley_2km) / as.numeric(R2$se_ols)
cat(sprintf("2. Conley 2 km SEs are %.2f to %.2f times the OLS SEs (median %.2f)\n",
            min(se_ratio), max(se_ratio), stats::median(se_ratio)))
flip <- R2[as.numeric(R2$p_ols) < 0.05 & as.numeric(R2$p_conley_2km) >= 0.05, ]
cat(sprintf("   %d of %d significant OLS results lose significance under Conley 2 km\n",
            nrow(flip), sum(as.numeric(R2$p_ols) < 0.05)))
if (nrow(flip)) for (i in seq_len(nrow(flip)))
  cat(sprintf("     %-32s %-24s p_OLS %s -> %s\n", flip$model[i], flip$term[i],
              flip$p_ols[i], flip$p_conley_2km[i]))
cat(sprintf("4. Belonging coefficient change when the acceptance item is dropped: %s to %s\n",
            min(R4$change), max(R4$change)))
cat("\nWrote robustness_common_sample.csv, robustness_standard_errors.csv,\n",
    "      robustness_robust_regression.csv, robustness_swb_leave_one_out.csv\n")
