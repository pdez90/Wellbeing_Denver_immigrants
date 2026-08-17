# =============================================================================
# 14_figure_coefficients.R
# Figure 2: every adjusted association in the paper on one pair of axes.
#
#   INPUT : all_model_coefficients.csv  (from 08_domain_models.R)
#   OUTPUT: figures/Figure2_Coefficients.png / .pdf
#           figures/Figure2_Coefficients.csv   (the plotted numbers)
#
# Panel A shows the individual and neighborhood socioeconomic predictors, taken
# from the "+ neighborhood context" models. Panel B shows the built-environment
# measures, each taken from its own domain model. Both outcomes appear on the
# same row for each predictor: subjective well-being and neighborhood belonging.
#
# Estimates are standardized regression coefficients with 95% confidence
# intervals. A coefficient of 0.2 means a one-standard-deviation difference in
# the predictor corresponds to a fifth of a standard deviation difference in the
# outcome.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
if (!exists("WB_LABELS")) source("wb_labels.R")
wb_require(c("readr", "dplyr"))

fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

co <- readr::read_csv(file.path(out_dir, "all_model_coefficients.csv"),
                      show_col_types = FALSE)

# -----------------------------------------------------------------------------
# Which coefficient comes from which model
# -----------------------------------------------------------------------------

PANEL_A <- list(
  "Individual characteristics" = c(
    "age", "children", "income_hh", "edu_level", "engl_speak",
    "time_live_denver", "time_live_hood", "ind_female", "ind_married",
    "ind_prev_married", "ind_hispanic", "ind_race_white", "ind_lpr",
    "ind_undocumented", "ind_imm_other"),
  "Neighborhood socioeconomic context" = c(
    "pop_density_z", "housing_density_z", "dist_downtown_km_z",
    "pct_poverty_z", "pct_non_native_z", "neighborhood_ses_index_z")
)
A_MODEL <- c(swb = "SWB: + neighborhood context", bel = "Belonging: + neighborhood context")

PANEL_B <- list(
  "Urban form" = list(model = "urban_form", terms = c(
    "walk_nat_walk_ind_z", "street_intdensity_z", "urban_center_nearest_dist_m_z")),
  "Transportation and accessibility" = list(model = "access_transport", terms = c(
    "hudjob_jobs_idx_z", "sidewalk_density_800_z", "bike_facility_density_800_z",
    "active_corridor_density_800_z", "ht_t_ami_z")),
  "Greenness and parks" = list(model = "green_parks", terms = c(
    "tree_tes_z", "tree_treecanopy_z", "park_acres_half_mile_z",
    "park_nearest_dist_m_z", "lc_800m_tree_canopy_z", "lc_800m_impervious_surfaces_z")),
  "Safety and social environment" = list(model = "safety_social", terms = c(
    "crash_density_800_z", "ped_crash_density_800_z", "bike_crash_density_800_z",
    "short_trip_zone_share_800_z", "div_total_diversity_resi_z", "div_exposure_mean_z")),
  "Land use and regulation" = list(model = "land_use", terms = c(
    "zone_categoryResidential, medium-high density", "zone_categoryMixed use",
    "zone_categoryNonresidential", "zone_adu_yes", "pfa_share_800_z"))
)

lab_of <- function(term) {
  # zoning dummies arrive with the factor level glued on
  z <- sub("^zone_category", "", term)
  if (z != term) return(paste0("Zoning: ", tolower(z)))
  if (!is.na(WB_LABELS[term])) return(unname(WB_LABELS[term]))
  term
}

pick <- function(model_name, term) {
  r <- co[co$model == model_name & co$term == term, ]
  if (!nrow(r)) return(NULL)
  data.frame(estimate = r$estimate[1], se = r$std.error[1], p = r$p.value[1],
             n = r$n[1], stringsAsFactors = FALSE)
}

rows <- list()
add <- function(panel, group, term, swb_model, bel_model) {
  s <- pick(swb_model, term); b <- pick(bel_model, term)
  if (is.null(s) && is.null(b)) {
    stop("14_figure_coefficients.R: term '", term, "' not found in ",
         swb_model, " or ", bel_model,
         ". The model specification and this figure have drifted apart.")
  }
  rows[[length(rows) + 1]] <<- data.frame(
    panel = panel, group = group, term = term, label = lab_of(term),
    swb_est = if (is.null(s)) NA_real_ else s$estimate,
    swb_se  = if (is.null(s)) NA_real_ else s$se,
    swb_p   = if (is.null(s)) NA_real_ else s$p,
    bel_est = if (is.null(b)) NA_real_ else b$estimate,
    bel_se  = if (is.null(b)) NA_real_ else b$se,
    bel_p   = if (is.null(b)) NA_real_ else b$p,
    stringsAsFactors = FALSE)
}

for (g in names(PANEL_A)) {
  for (t in PANEL_A[[g]]) add("A", g, t, A_MODEL[["swb"]], A_MODEL[["bel"]])
}
for (g in names(PANEL_B)) {
  spec <- PANEL_B[[g]]
  for (t in spec$terms) {
    add("B", g, t, paste0("SWB: ", spec$model), paste0("Belonging: ", spec$model))
  }
}

D <- do.call(rbind, rows)
D$swb_lo <- D$swb_est - 1.96 * D$swb_se; D$swb_hi <- D$swb_est + 1.96 * D$swb_se
D$bel_lo <- D$bel_est - 1.96 * D$bel_se; D$bel_hi <- D$bel_est + 1.96 * D$bel_se
readr::write_csv(D, file.path(fig_dir, "Figure2_Coefficients.csv"))

# -----------------------------------------------------------------------------
# Drawing
# -----------------------------------------------------------------------------

COL_SWB <- "#B23A48"   # subjective well-being
COL_BEL <- "#1F4E5F"   # neighborhood belonging

draw_panel <- function(d, title, xr) {
  groups <- unique(d$group)
  d$row <- NA_integer_
  y <- 0; head_rows <- list()
  for (g in groups) {
    y <- y + 1; head_rows[[g]] <- y                      # blank row for the heading
    idx <- which(d$group == g)
    for (k in idx) { y <- y + 1; d$row[k] <- y }
  }
  ymax <- y + 0.6

  graphics::plot(NA, xlim = xr, ylim = c(ymax, 0.2), axes = FALSE,
                 xlab = "Standardized coefficient (95% CI)", ylab = "",
                 cex.lab = 0.85)
  graphics::abline(v = 0, col = "#999999", lwd = 1)
  graphics::abline(h = d$row, col = "#F0F0F0", lwd = 6)   # faint banding
  graphics::abline(v = 0, col = "#999999", lwd = 1)
  graphics::axis(1, cex.axis = 0.8)

  # Variable names and group headings live in the left margin. They are drawn
  # with text() rather than axis(): axis() silently drops labels it thinks
  # overlap, which on a 25-row panel quietly deletes half of them.
  lab_x  <- xr[1] - diff(xr) * 0.03
  head_x <- graphics::grconvertX(0.004, "nfc", "user")
  graphics::text(lab_x, d$row, d$label, adj = 1, cex = 0.72, xpd = NA)
  for (g in groups) {
    graphics::text(head_x, head_rows[[g]], g, adj = 0, cex = 0.72, font = 2, xpd = NA)
  }
  graphics::mtext(title, side = 3, line = 0.9, adj = 0, at = graphics::grconvertX(0.01, "nfc", "user"),
                  font = 2, cex = 0.9, xpd = NA)

  for (k in seq_len(nrow(d))) {
    yy <- d$row[k]
    graphics::segments(d$swb_lo[k], yy - 0.19, d$swb_hi[k], yy - 0.19, col = COL_SWB, lwd = 1.5)
    graphics::points(d$swb_est[k], yy - 0.19, pch = 19, col = COL_SWB, cex = 0.75)
    graphics::segments(d$bel_lo[k], yy + 0.19, d$bel_hi[k], yy + 0.19, col = COL_BEL, lwd = 1.5)
    graphics::points(d$bel_est[k], yy + 0.19, pch = 17, col = COL_BEL, cex = 0.75)
  }
}

draw_figure <- function() {
  xr <- range(c(D$swb_lo, D$swb_hi, D$bel_lo, D$bel_hi), na.rm = TRUE)
  pad <- diff(xr) * 0.04; xr <- xr + c(-pad, pad)

  graphics::par(family = "serif", mfrow = c(2, 1), oma = c(2.0, 0, 0, 0),
                mar = c(3.6, 14.2, 2.8, 1.0), mgp = c(2.0, 0.6, 0))
  draw_panel(D[D$panel == "A", ], "A. Individual characteristics and neighborhood context", xr)
  draw_panel(D[D$panel == "B", ], "B. Built environment measures, by domain", xr)

  graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  graphics::plot.new()
  graphics::legend("bottom", horiz = TRUE, bty = "n", cex = 0.85,
                   legend = c("Subjective well-being", "Neighborhood belonging"),
                   col = c(COL_SWB, COL_BEL), pch = c(19, 17), lwd = 1.5, seg.len = 1.4)
}

# Portrait, two stacked panels: sized to sit in a single-column manuscript page
# without a landscape break.
grDevices::png(file.path(fig_dir, "Figure2_Coefficients.png"),
               width = 7.2, height = 9.6, units = "in", res = 300,
               type = "cairo", bg = "white")
draw_figure()
grDevices::dev.off()

grDevices::pdf(file.path(fig_dir, "Figure2_Coefficients.pdf"), width = 7.2, height = 9.6)
draw_figure()
grDevices::dev.off()

cat("Figure 2 written to", fig_dir, "with", nrow(D), "coefficients\n")
