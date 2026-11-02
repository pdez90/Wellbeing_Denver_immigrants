# =============================================================================
# 33_tables_figures_v3.R
# Main-text table and coefficient plot, and the supplementary tables, for the
# v3 parallel-outcome analysis.
#
#   INPUT : v3_*.csv (31, 32)
#   OUTPUT: v3_tables/Table1_main_models.csv / .docx    compact main-text table
#           v3_tables/TableS_*.csv                       supplement tables
#           v3_tables/V3_Tables.docx                     all tables, one file
#           figures/FigureV3_coefficients.png / .pdf     SWB vs NSC coefficient plot
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
source("wb_domains_v3.R")
wb_require(c("dplyr", "readr", "tidyr", "flextable", "officer"))

tab_dir <- file.path(out_dir, "v3_tables"); dir.create(tab_dir, showWarnings = FALSE)
fig_dir <- file.path(out_dir, "figures");   dir.create(fig_dir, showWarnings = FALSE)
co  <- readr::read_csv(file.path(out_dir, "v3_coefficients.csv"), show_col_types = FALSE)
fit <- readr::read_csv(file.path(out_dir, "v3_model_fit.csv"), show_col_types = FALSE)
vif <- readr::read_csv(file.path(out_dir, "v3_vif.csv"), show_col_types = FALSE)
buf <- readr::read_csv(file.path(out_dir, "v3_buffer_sensitivity.csv"), show_col_types = FALSE)
ses <- readr::read_csv(file.path(out_dir, "v3_standard_errors.csv"), show_col_types = FALSE)
mmt <- readr::read_csv(file.path(out_dir, "v3_robust_mm.csv"), show_col_types = FALSE)
bt  <- readr::read_csv(file.path(out_dir, "v3_bootstrap.csv"), show_col_types = FALSE)
sm  <- readr::read_csv(file.path(out_dir, "v3_summary.csv"), show_col_types = FALSE)
getq <- function(q) sm$value[sm$quantity == q]

stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "+", ""))))
cell  <- function(b, se, p) sprintf("%.3f%s (%.3f)", b, stars(p), se)
co$cell <- cell(co$estimate, co$std.error, co$p.value)
NOTE_STARS <- "+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001. Standardized coefficients (outcomes and continuous predictors in SD units; rail station is 0/1) with standard errors in parentheses."

# order of terms as they appear in the registry
term_order <- c(unlist(lapply(names(V3_DOMAIN_VARS), function(d) V3_DOMAIN_VARS[[d]])), V3_CONTROL_VARS)
lab_order  <- v3_label(term_order)
ctrl_order <- v3_label(c("age", "children", "income_hh", "edu_level", "engl_speak", "time_live_denver",
                         "time_live_hood", "ind_female", "ind_married", "ind_prev_married", "ind_hispanic",
                         "ind_race_white", "ind_lpr", "ind_undocumented", "ind_imm_other"))
dom_of <- unlist(lapply(names(V3_DOMAIN_VARS), function(d) setNames(rep(V3_DOMAIN_LABELS[[d]], length(V3_DOMAIN_VARS[[d]])), v3_label(V3_DOMAIN_VARS[[d]]))))

# a two-column (SWB | NSC) table from a long frame, with domain header rows
two_col <- function(d, labels, headers = TRUE) {
  w <- d %>% dplyr::select(label, outcome, cell) %>%
    tidyr::pivot_wider(names_from = outcome, values_from = cell) %>%
    dplyr::rename(`Subjective well-being` = swb, `Neighborhood social connectedness` = nsc)
  w <- w[match(labels, w$label), ]; w <- w[!is.na(w$label), ]
  if (!headers) return(as.data.frame(w))
  out <- list(); last <- ""
  for (i in seq_len(nrow(w))) {
    dm <- unname(dom_of[w$label[i]]); dm <- ifelse(is.na(dm), "Neighborhood demographic controls", dm)
    if (dm != last) { out[[length(out) + 1]] <- data.frame(label = dm, `Subjective well-being` = "", `Neighborhood social connectedness` = "", check.names = FALSE); last <- dm }
    out[[length(out) + 1]] <- as.data.frame(w[i, ])
  }
  dplyr::bind_rows(out)
}
fit_rows <- function(block) {
  f <- fit %>% dplyr::filter(frame == "main", block == !!block)
  g <- function(oc, col, fmt) sprintf(fmt, f[[col]][f$outcome == oc])
  out <- data.frame(label = c("Observations", "Adjusted R²", "Adjusted R², controls only"),
             `Subjective well-being` = c(g("swb", "n", "%d"), g("swb", "adj_r2", "%.3f"), sprintf("%.3f", fit$adj_r2[fit$model == "swb:controls"])),
             `Neighborhood social connectedness` = c(g("nsc", "n", "%d"), g("nsc", "adj_r2", "%.3f"), sprintf("%.3f", fit$adj_r2[fit$model == "nsc:controls"])),
             check.names = FALSE)
  if (block == "controls") out[-3, ] else out
}

# ---- Table 1 (main text): full models, NBE terms + demographic controls ----------
main <- co %>% dplyr::filter(frame == "main", block == "full", group != "intercept", group != "Individual characteristics")
T1 <- dplyr::bind_rows(two_col(main, lab_order), fit_rows("full"))
readr::write_csv(T1, file.path(tab_dir, "Table2_main_models.csv"))

# ---- Supplement tables ---------------------------------------------------------
# S-A: baseline (controls only), both outcomes, every coefficient
base <- co %>% dplyr::filter(frame == "main", block == "controls", !group %in% "intercept")
TSA <- dplyr::bind_rows(two_col(base, c(ctrl_order, lab_order), headers = FALSE), fit_rows("controls"))
# S-B / S-C: domain models, one table per outcome, columns = domains
dom_tab <- function(oc) {
  d <- co %>% dplyr::filter(frame == "main", outcome == oc, block %in% names(V3_DOMAIN_VARS), group != "intercept") %>%
    dplyr::mutate(block = V3_DOMAIN_LABELS[block]) %>% dplyr::select(label, block, cell) %>%
    tidyr::pivot_wider(names_from = block, values_from = cell)
  d <- d[match(c(ctrl_order, lab_order), d$label), ]; d <- d[!is.na(d$label), ]
  f <- fit %>% dplyr::filter(frame == "main", outcome == oc, block %in% names(V3_DOMAIN_VARS))
  add <- data.frame(label = c("Observations", "Adjusted R²"))
  for (b in names(V3_DOMAIN_VARS)) add[[V3_DOMAIN_LABELS[[b]]]] <- c(sprintf("%d", f$n[f$block == b]), sprintf("%.3f", f$adj_r2[f$block == b]))
  d[is.na(d)] <- ""
  dplyr::bind_rows(as.data.frame(d), add)
}
TSB <- dom_tab("swb"); TSC <- dom_tab("nsc")
# S-D: full models with every coefficient
TSD <- dplyr::bind_rows(two_col(co %>% dplyr::filter(frame == "main", block == "full", !group %in% "intercept"), c(ctrl_order, v3_label(V3_CONTROL_VARS), lab_order), headers = FALSE), fit_rows("full"))
# S-E: variants, NBE terms only, columns = main | +urban center | +residential diversity | rail distance ; per outcome
var_tab <- function(oc) {
  keep <- c(paste0(oc, ":full"), paste0(oc, ":full_urban_center"), paste0(oc, ":full_resdiv"), paste0(oc, ":full_rail_dist"))
  nm <- c("Main specification", "+ Distance to urban center", "+ Residential diversity", "Rail distance instead of 800 m flag")
  d <- co %>% dplyr::filter(model %in% keep, !group %in% c("intercept", "Individual characteristics")) %>%
    dplyr::mutate(col = nm[match(model, keep)]) %>% dplyr::select(label, col, cell) %>%
    tidyr::pivot_wider(names_from = col, values_from = cell)
  ord <- c(lab_order, v3_label(c("urban_center_nearest_dist_m", "rail_station_nearest_km", "div_total_diversity_resi")))
  d <- d[match(ord, d$label), ]; d <- d[!is.na(d$label), ]; d[is.na(d)] <- ""
  f <- fit %>% dplyr::filter(model %in% keep)
  add <- data.frame(label = c("Observations", "Adjusted R²"))
  for (i in seq_along(keep)) add[[nm[i]]] <- c(sprintf("%d", f$n[f$model == keep[i]]), sprintf("%.3f", f$adj_r2[f$model == keep[i]]))
  dplyr::bind_rows(as.data.frame(d), add)
}
TSE_swb <- var_tab("swb"); TSE_nsc <- var_tab("nsc")
# S-F: single-item belonging vs composite, same respondents
b1 <- co %>% dplyr::filter(model %in% c("nsc:full_belong1", "belong1:full_belong1"), !group %in% c("intercept", "Individual characteristics")) %>%
  dplyr::mutate(col = ifelse(outcome == "nsc", "NSC composite (4 items)", "Single belonging item")) %>%
  dplyr::select(label, col, cell) %>% tidyr::pivot_wider(names_from = col, values_from = cell)
b1 <- b1[match(lab_order, b1$label), ]; b1 <- b1[!is.na(b1$label), ]
fb <- fit %>% dplyr::filter(model %in% c("nsc:full_belong1", "belong1:full_belong1"))
TSF <- dplyr::bind_rows(as.data.frame(b1), data.frame(label = c("Observations", "Adjusted R²"),
  `NSC composite (4 items)` = c(sprintf("%d", fb$n[fb$model == "nsc:full_belong1"]), sprintf("%.3f", fb$adj_r2[fb$model == "nsc:full_belong1"])),
  `Single belonging item` = c(sprintf("%d", fb$n[fb$model == "belong1:full_belong1"]), sprintf("%.3f", fb$adj_r2[fb$model == "belong1:full_belong1"])), check.names = FALSE))
# S-G: VIF
TSG <- vif %>% dplyr::filter(model == "swb:full") %>% dplyr::transmute(Predictor = label, VIF = sprintf("%.2f", vif))
# S-H: buffer
TSH <- buf %>% dplyr::mutate(outcome = V3_OUTCOME_LABELS[outcome]) %>% dplyr::select(outcome, measure, radius_m, cell) %>%
  tidyr::pivot_wider(names_from = radius_m, values_from = cell, names_prefix = "") %>%
  dplyr::rename(Outcome = outcome, Measure = measure, `400 m` = `400`, `800 m` = `800`, `1,600 m` = `1600`)
# S-I: standard errors
TSI <- ses %>% dplyr::transmute(Outcome = V3_OUTCOME_LABELS[outcome], Predictor = label, Estimate = sprintf("%.3f", estimate),
  `SE (OLS)` = sprintf("%.3f", se_ols), `SE (tract)` = sprintf("%.3f", se_cluster_tract), `SE (block group)` = sprintf("%.3f", se_cluster_bg),
  `SE (Conley 1 km)` = sprintf("%.3f", se_conley_1km), `SE (2 km)` = sprintf("%.3f", se_conley_2km), `SE (5 km)` = sprintf("%.3f", se_conley_5km),
  `p (OLS)` = sprintf("%.3f", p_ols), `p (tract)` = sprintf("%.3f", p_cluster_tract), `p (2 km)` = sprintf("%.3f", p_conley_2km))
# S-J: MM
TSJ <- mmt %>% dplyr::transmute(Outcome = V3_OUTCOME_LABELS[outcome], Predictor = label, N = n, OLS = ols, `Robust MM` = robust_mm, Change = sprintf("%+.3f", change))
# S-K: bootstrap
TSK <- bt %>% dplyr::transmute(Outcome = V3_OUTCOME_LABELS[outcome], Predictor = label, Estimate = sprintf("%.3f", estimate),
  `Bootstrap SE` = sprintf("%.3f", boot_se), `95% percentile CI` = sprintf("(%.3f, %.3f)", ci_low, ci_high),
  `Excludes 0` = ifelse(excludes_zero, "yes", "no"), `OLS p` = sprintf("%.3f", ols_p))

TABLES <- list(
  list(id = "Table 2", title = "Neighborhood built environment and the two outcomes: full models", df = T1,
       note = paste(NOTE_STARS, sprintf("Both models are estimated on the same %d respondents and additionally adjust for the fifteen individual characteristics reported in Table S4.", getq("n_common_sample")))),
  list(id = "Table S1", title = "Controls only: individual characteristics and neighborhood demographics", df = TSA, note = NOTE_STARS),
  list(id = "Table S2", title = "Subjective well-being regressed on each NBE domain in turn", df = TSB, note = paste(NOTE_STARS, "Every column includes the individual and neighborhood demographic controls.")),
  list(id = "Table S3", title = "Neighborhood social connectedness regressed on each NBE domain in turn", df = TSC, note = paste(NOTE_STARS, "Every column includes the individual and neighborhood demographic controls.")),
  list(id = "Table S4", title = "Full models with every coefficient", df = TSD, note = NOTE_STARS),
  list(id = "Table S5", title = "Specification variants, subjective well-being", df = TSE_swb, note = paste(NOTE_STARS, "Each column changes one thing relative to the main full model; individual and neighborhood demographic controls are included throughout.")),
  list(id = "Table S6", title = "Specification variants, neighborhood social connectedness", df = TSE_nsc, note = paste(NOTE_STARS, "Each column changes one thing relative to the main full model; individual and neighborhood demographic controls are included throughout.")),
  list(id = "Table S7", title = "Single belonging item versus the four-item NSC composite as the outcome", df = TSF, note = paste(NOTE_STARS, sprintf("Same predictors on the %d respondents who answered the belonging item (%d of the analytic sample did not). The two outcomes correlate at r = %.2f.", getq("n_belong1"), getq("n_common_sample") - getq("n_belong1"), getq("cor_nsc_belong1")))),
  list(id = "Table S8", title = "Variance inflation factors, full model", df = TSG, note = "Identical for the two outcomes because the predictor set is the same."),
  list(id = "Table S9", title = "Buffer radius: buffer-derived measures at 400 m, 800 m and 1,600 m", df = TSH, note = paste(NOTE_STARS, sprintf("All six buffer measures are swapped to the stated radius at once; park acreage is available at 800 m only and is held fixed, as are the non-buffer measures and controls. In the analytic sample %d respondents have a rail station within 400 m, %d within 800 m and %d within 1,600 m.", getq("n_rail_400"), getq("n_rail_800"), getq("n_rail_1600")))),
  list(id = "Table S10", title = "Clustered and spatial standard errors, full models", df = TSI, note = "Cluster-robust (HC1) standard errors by census tract and block group, and Conley spatial HAC standard errors with a Bartlett kernel at 1, 2 and 5 km. Point estimates are unchanged; p-values use the OLS residual degrees of freedom."),
  list(id = "Table S11", title = "Robust MM-estimator, full models", df = TSJ, note = paste(NOTE_STARS, "MM p-values use the OLS residual degrees of freedom.")),
  list(id = "Table S12", title = "Nonparametric bootstrap, full models", df = TSK, note = "2,000 resamples of respondents; percentile intervals.")
)
for (t in TABLES) readr::write_csv(t$df, file.path(tab_dir, paste0(gsub(" ", "", t$id), "_", gsub("[^A-Za-z0-9]+", "_", substr(t$title, 1, 40)), ".csv")))

doc <- officer::read_docx()
for (t in TABLES) {
  ft <- flextable::flextable(t$df) %>% flextable::add_footer_lines(t$note) %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = if (ncol(t$df) > 6) 7 else 8, part = "all") %>% flextable::bold(part = "header") %>%
    flextable::align(j = 1, align = "left", part = "all") %>% flextable::autofit()
  if (ncol(t$df) > 1) ft <- flextable::align(ft, j = 2:ncol(t$df), align = "center", part = "all")
  hdr_rows <- which(t$df[[2]] == "" & !t$df[[1]] %in% c("Observations"))
  if (length(hdr_rows) && t$id %in% c("Table 2")) ft <- flextable::bold(ft, i = hdr_rows) %>% flextable::italic(i = hdr_rows)
  doc <- officer::body_add_par(doc, sprintf("%s. %s", t$id, t$title), style = "heading 2")
  doc <- flextable::body_add_flextable(doc, ft)
  doc <- officer::body_add_par(doc, "")
  if (ncol(t$df) > 6) doc <- officer::body_end_section_landscape(doc) else doc <- officer::body_end_section_portrait(doc)
}
print(doc, target = file.path(tab_dir, "V3_Tables.docx"))

# ---- Figure: SWB vs NSC, full-model NBE coefficients ----------------------------
D <- main %>% dplyr::filter(group != "Neighborhood demographic controls") %>%
  dplyr::mutate(lo90 = estimate - stats::qt(.95, getq("df_residual_full")) * std.error,
                hi90 = estimate + stats::qt(.95, getq("df_residual_full")) * std.error)
D$label <- factor(D$label, levels = rev(lab_order[lab_order %in% D$label]))
SWBCOL <- "#A6293B"; BELCOL <- "#1F4E5F"
draw <- function() {
  par(mar = c(6.2, 13.5, 3.2, 1), family = "serif")
  labs <- levels(D$label); n <- length(labs)
  doms <- unname(dom_of[labs])
  # rows are 1 unit apart within a domain and 1.9 units apart across domains,
  # leaving room for the domain label and separator
  dom_id <- as.integer(factor(doms, levels = unique(doms)))
  ypos <- seq_len(n) + 0.9 * (dom_id - 1)
  xr <- range(c(D$conf.low, D$conf.high)); xr <- xr + c(-0.05, 0.05) * diff(xr)
  plot(NA, xlim = xr, ylim = c(0.5, max(ypos) + 1.2), axes = FALSE, xlab = "", ylab = "")
  ticks <- pretty(xr, 5); abline(v = ticks, col = "#E6E6E6", lwd = .7); abline(v = 0, col = "#777777", lwd = 1.1)
  axis(1, at = ticks, labels = sprintf("%.1f", ticks), cex.axis = .8, col = "#777777", col.axis = "#333333", tck = -.012, mgp = c(2, .5, 0))
  mtext("Standardized coefficient (90% and 95% CI)", side = 1, line = 2.1, cex = .85)
  for (dm in unique(doms)) {
    ys <- ypos[doms == dm]
    text(xr[1] - .02 * diff(xr), max(ys) + .72, dm, xpd = NA, adj = c(1, .5), cex = .72, font = 3, col = "#555555")
    if (max(ys) < max(ypos)) abline(h = max(ys) + 1.15, col = "#BBBBBB", lwd = .6, lty = 2)
  }
  for (i in seq_len(n)) {
    text(xr[1] - .02 * diff(xr), ypos[i], labs[i], xpd = NA, adj = 1, cex = .82)
    for (k in c("swb", "nsc")) {
      r <- D[D$label == labs[i] & D$outcome == k, ]; y <- ypos[i] + ifelse(k == "swb", .17, -.17)
      col <- ifelse(k == "swb", SWBCOL, BELCOL)
      segments(r$conf.low, y, r$conf.high, y, col = col, lwd = 1.2)
      segments(r$lo90, y, r$hi90, y, col = col, lwd = 2.8)
      points(r$estimate, y, pch = ifelse(k == "swb", 21, 24), cex = 1.05, lwd = 1.2, col = col, bg = ifelse(r$p.value < .05, col, "white"))
    }
  }
  title(main = "Neighborhood built environment and the two outcomes", adj = 0, line = 1.9, cex.main = 1.1)
  mtext(sprintf("Full models, n = %d; a filled marker means p < 0.05", getq("n_common_sample")), side = 3, adj = 0, line = .6, cex = .85, col = "#555555")
  legend(x = xr[1], y = -2.4, legend = c("Subjective well-being", "Neighborhood social connectedness"), pch = c(21, 24), pt.bg = c(SWBCOL, BELCOL), col = c(SWBCOL, BELCOL), bty = "n", cex = .8, horiz = TRUE, xpd = NA, x.intersp = 0.8)
}
grDevices::png(file.path(fig_dir, "FigureV3_coefficients.png"), width = 8, height = 6.4, units = "in", res = 600, bg = "white"); draw(); grDevices::dev.off()
grDevices::pdf(file.path(fig_dir, "FigureV3_coefficients.pdf"), width = 8, height = 6.4); draw(); grDevices::dev.off()
readr::write_csv(D %>% dplyr::select(outcome, term, label, estimate, std.error, p.value, conf.low, conf.high), file.path(fig_dir, "FigureV3_coefficients.csv"))
cat("Wrote", length(TABLES), "tables to", tab_dir, "and FigureV3_coefficients to", fig_dir, "\n")
