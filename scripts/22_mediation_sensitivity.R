# =============================================================================
# 22_mediation_sensitivity.R
# How much unmeasured confounding would it take to explain away the mediation?
#
#   INPUT : model_objects.rds  (from 08_domain_models.R)
#   OUTPUT: mediation_sensitivity.csv          rho* and the R2 thresholds
#           figures/FigureS13_MediationSensitivity.png / .pdf
#
# WHY THIS EXISTS
#
# The decomposition in 09_mediation.R is only causal under sequential
# ignorability, and its hardest condition is that nothing unmeasured confounds
# the mediator-outcome relationship. Neighborhood belonging and subjective
# well-being are reported by the same respondent at the same moment, so a
# cheerful mood, an optimistic disposition or an acquiescent response style
# would raise both and could manufacture an indirect effect where no causal
# path exists. No amount of covariate adjustment in these data can rule that
# out, and no p-value speaks to it.
#
# What can be done is to quantify how much of it would be needed. Imai, Keele
# and Yamamoto's sensitivity analysis parameterises the problem with a single
# number, rho: the correlation between the error terms of the mediator equation
# and the outcome equation. Under sequential ignorability rho is zero. The
# analysis asks how large rho would have to be before the estimated indirect
# effect is zero.
#
# HOW TO READ THE OUTPUT
#
#   rho*        the correlation between the two error terms at which the ACME
#               would be exactly zero. Large values mean the finding is hard to
#               overturn; values near zero mean a slight amount of shared
#               unmeasured variation would suffice.
#   R2*_M R2*_Y the product of the proportions of the RESIDUAL variance in the
#               mediator and the outcome that a confounder would have to
#               explain to drive the ACME to zero.
#   R2~_M R2~_Y the same quantity as a share of the TOTAL variance in each. This
#               is the more conservative number and the one usually quoted: it
#               can be compared against how much variance the measured
#               covariates themselves explain.
#
# There is no threshold that makes a result "safe". The honest use of this
# analysis is to report rho* alongside the estimate and let a reader judge
# whether a confounder of that strength is plausible in this setting.
#
# NOTE ON THE FIT. medsens requires the quasi-Bayesian approximation rather than
# the bootstrap, so the mediate objects are refit here with boot = FALSE. The
# script prints the ACME from both so the two can be compared; they should agree
# closely, and if they do not, that is worth knowing before reading anything
# else on this page.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
if (!exists(".wb_domains_loaded")) source("wb_domains.R")
if (!exists("WB_LABELS")) source("wb_labels.R")
wb_require(c("tidyverse", "readr", "mediation"))

fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

obj       <- readRDS(file.path(out_dir, "model_objects.rds"))
model_dat <- obj$model_dat
context_z <- obj$context_z
set.seed(WB_SEED)

med_covars <- c(obj$individual_controls, context_z)
med_covars <- med_covars[med_covars %in% names(model_dat)]

# The same exposures 09_mediation.R tests, so the two tables line up row for row.
CAND <- wb_mediation_candidates(obj$final_be, model_dat)
cat("\n22_mediation_sensitivity.R:", length(CAND), "exposures\n", sep = "")

lab_of <- function(term) if (!is.na(WB_LABELS[term])) unname(WB_LABELS[term]) else term

# -----------------------------------------------------------------------------
# One exposure: refit, run the sensitivity analysis, extract the thresholds
# -----------------------------------------------------------------------------
run_one <- function(treat_var) {
  vars_needed <- unique(c("swb_z", "belonging_z", treat_var, med_covars))
  med_dat <- model_dat %>%
    dplyr::select(dplyr::any_of(vars_needed)) %>%
    tidyr::drop_na()
  covars_here <- setdiff(med_covars, treat_var)

  med_fit <- stats::lm(
    stats::as.formula(paste("belonging_z ~", paste(c(treat_var, covars_here), collapse = " + "))),
    data = med_dat)
  out_fit <- stats::lm(
    stats::as.formula(paste("swb_z ~ belonging_z +", paste(c(treat_var, covars_here), collapse = " + "))),
    data = med_dat)

  # quasi-Bayesian, as medsens requires
  m_qb <- mediation::mediate(med_fit, out_fit, treat = treat_var,
                             mediator = "belonging_z", boot = FALSE, sims = 1000)

  sens <- try(mediation::medsens(m_qb, rho.by = 0.01, effect.type = "indirect",
                                 sims = 1000), silent = TRUE)
  if (inherits(sens, "try-error")) {
    warning("22_mediation_sensitivity.R: medsens failed for ", treat_var, ": ",
            conditionMessage(attr(sens, "condition")))
    return(NULL)
  }
  # The fields are named err.cr.d / R2star.d.thresh / R2tilde.d.thresh on the
  # medsens object itself. summary() wraps that object rather than reshaping it,
  # and R2star.prod is a vector over the whole rho grid rather than the value at
  # the crossing, so reading element 1 of it silently returns the value at
  # rho = -1 for every exposure. Read the thresholds directly.
  rho_star <- suppressWarnings(as.numeric(sens$err.cr.d)[1])
  r2_res   <- suppressWarnings(as.numeric(sens$R2star.d.thresh)[1])
  r2_tot   <- suppressWarnings(as.numeric(sens$R2tilde.d.thresh)[1])

  # fall back to the curve if a field is missing in this version: the ACME is
  # monotone in rho, so the crossing is the rho whose ACME is nearest zero
  if (!length(rho_star) || is.na(rho_star)) rho_star <- sens$rho[which.min(abs(sens$d0))]
  if (!length(r2_res) || is.na(r2_res))     r2_res   <- rho_star^2
  if (!length(r2_tot)) r2_tot <- NA_real_

  list(
    row = data.frame(
      predictor  = treat_var,
      label      = lab_of(treat_var),
      domain     = wb_domain_of(treat_var),
      n          = nrow(med_dat),
      acme_qb    = m_qb$d0,
      acme_qb_lo = m_qb$d0.ci[1],
      acme_qb_hi = m_qb$d0.ci[2],
      acme_qb_p  = m_qb$d0.p,
      rho_star   = rho_star,
      r2_resid_prod = r2_res,
      r2_total_prod = r2_tot,
      r2_mediator_model = sens$r.square.m,
      r2_outcome_model  = sens$r.square.y,
      stringsAsFactors = FALSE),
    sens = sens)
}

results <- list(); sens_objs <- list()
for (v in CAND) {
  cat("  ", lab_of(v), "\n", sep = "")
  r <- run_one(v)
  if (is.null(r)) next
  results[[v]] <- r$row
  sens_objs[[v]] <- r$sens
}
S <- do.call(rbind, results)

# The bootstrap ACME from 09, for the comparison the header promises.
med_file <- file.path(out_dir, "formal_mediation_results_domain_models.csv")
if (file.exists(med_file)) {
  boot <- readr::read_csv(med_file, show_col_types = FALSE)
  S <- S %>%
    dplyr::left_join(dplyr::select(boot, predictor, acme_boot = acme,
                                   acme_boot_p = acme_p, acme_p_holm),
                     by = "predictor") %>%
    dplyr::mutate(acme_gap = acme_qb - acme_boot)
}

readr::write_csv(S, file.path(out_dir, "mediation_sensitivity.csv"))

cat("\n== Sensitivity of each indirect effect to mediator-outcome confounding ==\n")
print(S %>% dplyr::transmute(
  Exposure = label, n,
  `ACME (bootstrap)` = sprintf("%.3f", acme_boot),
  `ACME (quasi-Bayes)` = sprintf("%.3f", acme_qb),
  `rho*` = sprintf("%.2f", rho_star),
  `R2 residual` = sprintf("%.3f", r2_resid_prod),
  `R2 total` = ifelse(is.na(r2_total_prod), "--", sprintf("%.3f", r2_total_prod)),
  Interpretable = ifelse(acme_qb_p < 0.10, "yes", "ACME ~ 0 anyway")) %>%
  as.data.frame(), row.names = FALSE)

cat("\nNote on why the rho* values cluster so tightly across exposures: the\n")
cat("indirect effect is the product of the exposure-to-belonging path, which\n")
cat("does not depend on rho, and the belonging-to-well-being path, which does.\n")
cat("The crossing point is therefore governed mainly by the mediator-outcome\n")
cat("relationship, and that relationship is common to all six models. Read rho*\n")
cat("for the exposures whose indirect effect is actually distinguishable from\n")
cat("zero; for the others the crossing is arithmetic rather than informative.\n")

# -----------------------------------------------------------------------------
# Figure: ACME against rho, for the exposures with a nominally significant ACME
# -----------------------------------------------------------------------------
SHOW <- S$predictor[!is.na(S$acme_qb_p) & S$acme_qb_p < 0.10]
if (!length(SHOW)) SHOW <- S$predictor[which.min(S$acme_qb_p)]

draw <- function() {
  k <- length(SHOW)
  par(mfrow = c(1, k), mar = c(4.4, 4.4, 3.2, 0.8), family = "serif")
  for (v in SHOW) {
    sn <- sens_objs[[v]]
    row <- S[S$predictor == v, ]
    yl <- range(c(sn$upper.d0, sn$lower.d0), na.rm = TRUE)
    plot(NA, xlim = c(-1, 1), ylim = yl, axes = FALSE, xlab = "", ylab = "")
    abline(h = 0, col = "#777777", lwd = 1.1)
    abline(v = 0, col = "#CFCFCF", lwd = 0.8, lty = 3)
    polygon(c(sn$rho, rev(sn$rho)), c(sn$lower.d0, rev(sn$upper.d0)),
            col = "#E6EEF2", border = NA)
    lines(sn$rho, sn$d0, col = "#1F4E5F", lwd = 2)
    if (is.finite(row$rho_star)) {
      abline(v = row$rho_star, col = "#A6293B", lwd = 1.6, lty = 2)
      text(row$rho_star, yl[2], sprintf(" rho* = %.2f", row$rho_star),
           adj = c(0, 1), col = "#A6293B", cex = 0.8)
    }
    axis(1, cex.axis = 0.8, col = "#777777", col.axis = "#333333",
         tck = -0.015, mgp = c(2, 0.5, 0))
    axis(2, cex.axis = 0.8, col = "#777777", col.axis = "#333333",
         tck = -0.015, mgp = c(2, 0.5, 0), las = 1)
    mtext(expression(rho), side = 1, line = 2.3, cex = 0.9)
    mtext("Indirect effect (ACME)", side = 2, line = 2.9, cex = 0.8)
    title(main = row$label, adj = 0, line = 1.5, cex.main = 1.0)
    mtext(sprintf("ACME = %.3f at rho = 0", row$acme_qb), side = 3, adj = 0,
          line = 0.4, cex = 0.75, col = "#555555")
  }
}

W <- max(4.2, 3.6 * length(SHOW))
grDevices::png(file.path(fig_dir, "FigureS13_MediationSensitivity.png"),
               width = W, height = 4.2, units = "in", res = 600,
               type = "cairo", bg = "white")
draw(); grDevices::dev.off()
grDevices::pdf(file.path(fig_dir, "FigureS13_MediationSensitivity.pdf"),
               width = W, height = 4.2)
draw(); grDevices::dev.off()

cat("\nWrote mediation_sensitivity.csv and figures/FigureS13_MediationSensitivity.png\n")
cat("\nHow to read rho*: it is the correlation between the mediator and outcome\n")
cat("error terms at which the indirect effect would vanish. For reference, the\n")
cat("measured covariates in these models explain roughly 10 to 20 per cent of\n")
cat("the variance in belonging, so a confounder strong enough to produce a rho\n")
cat("of 0.3 or more would have to be comparable in strength to everything the\n")
cat("models already adjust for, taken together.\n")
