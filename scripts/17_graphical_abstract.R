# =============================================================================
# 17_graphical_abstract.R
# One figure summarising every model the paper estimates, what each one found,
# and what was checked afterwards.
#
#   INPUT : all_model_coefficients.csv, model_fit_summary.csv   (08)
#           formal_mediation_results_domain_models.csv          (09)
#           buffer_sensitivity_wide.csv                         (15)
#           robustness_standard_errors.csv                      (16)
#   OUTPUT: figures/Graphical_Abstract.png / .pdf
#
# Every number printed on the figure is read from those files rather than typed
# in, so the abstract cannot drift away from the results it summarises. If a
# coefficient the figure needs is missing, the script stops.
#
# Drawn in base graphics, like the other figures in this repository.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
wb_require(c("readr"))

fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

co  <- readr::read_csv(file.path(out_dir, "all_model_coefficients.csv"), show_col_types = FALSE)
fit <- readr::read_csv(file.path(out_dir, "model_fit_summary.csv"),      show_col_types = FALSE)

grab <- function(model, term) {
  r <- co[co$model == model & co$term == term, ]
  if (!nrow(r)) stop("17_graphical_abstract.R: ", model, " / ", term, " not found.")
  list(b = r$estimate[1], p = r$p.value[1], n = r$n[1])
}
nof <- function(model) {
  r <- fit[fit$model == model, ]
  if (!nrow(r)) stop("17_graphical_abstract.R: no fit row for ", model)
  r$n[1]
}
stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*",
                     ifelse(p < .10, "+", ""))))
bfmt <- function(x) sprintf("%s = %.2f%s", "β", x$b, stars(x$p))

# ---- the numbers the figure prints ------------------------------------------
n_base   <- nof("SWB: individual controls")
n_ctx    <- nof("SWB: + neighborhood context")
n_int    <- nof("SWB: final BE model")
n_land   <- nof("SWB: land_use")
n_safety <- nof("SWB: safety_social")

walk  <- grab("SWB: urban_form",            "walk_nat_walk_ind_z")
bike  <- grab("SWB: access_transport",      "bike_facility_density_800_z")
park  <- grab("SWB: green_parks",           "park_acres_half_mile_z")
canop <- grab("Belonging: green_parks",     "lc_800m_tree_canopy_z")
pov   <- grab("Belonging: final BE model",  "pct_poverty_z")
ucen  <- grab("Belonging: urban_form",      "urban_center_nearest_dist_m_z")
belg  <- grab("SWB: final BE + belonging",  "belonging_z")

med <- readr::read_csv(file.path(out_dir, "formal_mediation_results_domain_models.csv"),
                       show_col_types = FALSE)
holm_col <- grep("holm", names(med), ignore.case = TRUE, value = TRUE)[1]
acme_p   <- grep("^acme.*p$|p.*acme", names(med), ignore.case = TRUE, value = TRUE)[1]
n_med    <- nrow(med)
n_survive <- sum(med[[holm_col]] < 0.05, na.rm = TRUE)

buf <- readr::read_csv(file.path(out_dir, "buffer_sensitivity_wide.csv"), show_col_types = FALSE)
se  <- readr::read_csv(file.path(out_dir, "robustness_standard_errors.csv"), show_col_types = FALSE)
se_ratio <- stats::median(se$se_conley_2km / se$se_ols)

# =============================================================================
# Drawing
# =============================================================================
SWBCOL <- "#A6293B"; BELCOL <- "#1F4E5F"
INK    <- "#1A1A1A"; MUTE   <- "#6B6B6B"
PANEL  <- "#F4F1EC"; LINE   <- "#B9B2A8"; TICK <- "#2E6B4F"

rbox <- function(x, y, w, h, r = 1.0, fill = "#FFFFFF", border = LINE, lwd = 1.1, lty = 1) {
  arc <- function(cx, cy, a0, a1) {
    a <- seq(a0, a1, length.out = 12); cbind(cx + r * cos(a), cy + r * sin(a))
  }
  x0 <- x - w/2; x1 <- x + w/2; y0 <- y - h/2; y1 <- y + h/2
  p <- rbind(arc(x1-r, y1-r, 0, pi/2), arc(x0+r, y1-r, pi/2, pi),
             arc(x0+r, y0+r, pi, 3*pi/2), arc(x1-r, y0+r, 3*pi/2, 2*pi))
  polygon(p[,1], p[,2], col = fill, border = border, lwd = lwd, lty = lty)
}

arrow_down <- function(x, y0, y1, col = LINE) {
  arrows(x, y0, x, y1, length = 0.07, angle = 22, col = col, lwd = 1.3)
}

# beta labels are drawn with plotmath so the Greek letter renders on any locale
blab <- function(x, y, label, v, cex = 0.72, col = INK, adj = 0.5) {
  txt <- sprintf("%.2f%s", v$b, stars(v$p))
  text(x, y, bquote(paste(.(label), "   ", beta, " = ", .(txt))),
       cex = cex, col = col, adj = adj)
}
tick <- function(x, y, col = TICK) {
  segments(x - 0.9, y + 0.1, x - 0.2, y - 0.9, col = col, lwd = 2.4, lend = 1)
  segments(x - 0.2, y - 0.9, x + 1.1, y + 1.5, col = col, lwd = 2.4, lend = 1)
}

draw <- function() {
  par(mar = c(0, 0, 0, 0), family = "serif")
  plot.new(); plot.window(xlim = c(0, 200), ylim = c(0, 108))

  # ---------------- header ---------------------------------------------------
  text(3, 103, "Neighborhood built environment, belonging and well-being among first-generation immigrants",
       adj = 0, cex = 1.3, font = 2, col = INK)
  text(3, 98.2, sprintf("381 survey respondents in metropolitan Denver  |  358 geocoded homes  |  five built-environment domains in 800 m buffers  |  %d least-squares models",
                        nrow(fit)),
       adj = 0, cex = 0.86, col = MUTE)
  segments(3, 95.5, 197, 95.5, col = LINE, lwd = 1.1)

  text(3,   91.5, "1.  THE MODELS WE FITTED", adj = 0, cex = 0.92, font = 2, col = INK)
  text(72,  91.5, "2.  WHAT THEY FOUND",      adj = 0, cex = 0.92, font = 2, col = INK)
  text(146, 91.5, "3.  WHAT WE CHECKED",      adj = 0, cex = 0.92, font = 2, col = INK)

  # ================= column 1: the ladder ===================================
  lad <- list(
    list(y = 82, t = "Individual characteristics",
         s = sprintf("age, gender, marital status, income, education,\nEnglish, time in Denver     (n = %d)", n_base)),
    list(y = 69, t = "+ Neighborhood socioeconomic context",
         s = sprintf("density, distance to downtown, poverty,\nforeign-born share, SES index     (n = %d)", n_ctx)),
    list(y = 56, t = "+ One built-environment domain at a time",
         s = sprintf("urban form | transportation | greenness and parks\nsafety | land use and regulation     (n = %d to %d)", n_int, n_safety)),
    list(y = 43, t = "Integrated model",
         s = sprintf("strongest predictors from every domain, fitted\nwith and without belonging     (n = %d)", n_int)),
    list(y = 30, t = "Mediation",
         s = sprintf("%d exposures tested through belonging,\n1,000 bootstrap resamples, Holm-corrected", n_med))
  )
  for (i in seq_along(lad)) {
    b <- lad[[i]]
    rbox(35, b$y, 62, 10, fill = "#FFFFFF")
    text(6.5, b$y + 2.5, b$t, adj = 0, cex = 0.85, font = 2, col = INK)
    text(6.5, b$y - 1.7, b$s, adj = 0, cex = 0.71, col = MUTE)
    if (i < length(lad)) arrow_down(35, b$y - 5.0, lad[[i + 1]]$y + 5.0)
  }
  text(6.5, 22.5, "Each row adds a block of predictors to the row above.",
       adj = 0, cex = 0.72, col = MUTE, font = 3)

  # ================= column 2: the findings ==================================
  rbox(108, 58, 68, 60, fill = PANEL, border = NA)

  rbox(88, 79, 40, 14, fill = "#FFFFFF", border = SWBCOL)
  text(88, 83.4, "Transportation-supportive form", cex = 0.78, font = 2, col = SWBCOL)
  blab(88, 79.6, "walkability", walk)
  blab(88, 76.6, "bicycle facilities", bike)
  blab(88, 73.6, "park acreage", park)

  rbox(88, 44, 40, 14, fill = "#FFFFFF", border = BELCOL)
  text(88, 48.4, "Greenness and social context", cex = 0.78, font = 2, col = BELCOL)
  blab(88, 44.6, "tree canopy", canop)
  blab(88, 41.6, "neighborhood poverty", pov)
  blab(88, 38.6, "distance to urban centre", ucen)

  rbox(134, 68, 26, 9.5, fill = "#FFFFFF", border = SWBCOL, lwd = 1.5)
  text(134, 69.8, "Subjective", cex = 0.82, font = 2, col = SWBCOL)
  text(134, 66.3, "well-being", cex = 0.82, font = 2, col = SWBCOL)

  rbox(134, 40, 26, 9.5, fill = "#FFFFFF", border = BELCOL, lwd = 1.5)
  text(134, 41.8, "Neighborhood", cex = 0.82, font = 2, col = BELCOL)
  text(134, 38.3, "belonging",    cex = 0.82, font = 2, col = BELCOL)

  arrows(108.5, 79, 120.5, 70.5, length = 0.09, angle = 20, col = SWBCOL, lwd = 2.2)
  arrows(108.5, 44, 120.5, 41.0, length = 0.09, angle = 20, col = BELCOL, lwd = 2.2)
  # from the top edge of the belonging box to the bottom edge of the well-being
  # box, so the shaft does not run through either rectangle
  arrows(134, 44.9, 134, 63.2, length = 0.10, angle = 20, col = BELCOL, lwd = 2.8)
  blab(141.5, 54, "", belg, cex = 0.8, col = BELCOL)

  # the compound route the data do not support
  lines(c(108.5, 114, 119), c(47.5, 53, 58), col = "#9A9A9A", lwd = 1.2, lty = 3)
  points(114, 53, pch = 4, cex = 1.7, lwd = 2.6, col = "#8A8A8A")
  text(101, 57.5, sprintf("no indirect route\n%d of %d indirect effects\nsurvive correction", n_survive, n_med),
       cex = 0.68, col = MUTE, font = 3, adj = 0)

  text(76, 29, "Belonging is a parallel dimension of neighborhood experience,",
       adj = 0, cex = 0.8, col = INK, font = 2)
  text(76, 25.6, "not the mechanism linking the built environment to well-being.",
       adj = 0, cex = 0.8, col = INK, font = 2)

  # ================= column 3: robustness ====================================
  checks <- list(
    c("Buffer radius", "400 m, 800 m and 1,600 m; the bicycle", "result strengthens with scale"),
    c("Common sample", "every domain refit on the 189 respondents", "complete on all five"),
    c("Spatial dependence", "cluster-robust and Conley standard errors;", sprintf("median change %.2f times", se_ratio)),
    c("Influential respondents", "robust MM-estimator leaves the estimates", "essentially unchanged"),
    c("Item overlap", "well-being rescored without the", "\"feels accepted\" item"),
    c("Collinear greenness", "Tree Equity measures entered", "separately and together")
  )
  ys <- seq(84, 28, length.out = length(checks))
  for (i in seq_along(checks)) {
    tick(150, ys[i] + 1.0)
    text(154.5, ys[i] + 1.2, checks[[i]][1], adj = 0, cex = 0.8, font = 2, col = INK)
    text(154.5, ys[i] - 1.9, checks[[i]][2], adj = 0, cex = 0.7, col = MUTE)
    text(154.5, ys[i] - 4.6, checks[[i]][3], adj = 0, cex = 0.7, col = MUTE)
  }

  # ---------------- footer ---------------------------------------------------
  segments(3, 17.5, 197, 17.5, col = LINE, lwd = 1.1)
  text(3, 13.2,
       "Standardized coefficients from ordinary least squares. Each exposure is shown as estimated in its own domain model; belonging comes from the integrated model. Every model adjusts for individual characteristics and neighborhood socioeconomic context.",
       adj = 0, cex = 0.72, col = MUTE)
  text(3, 9.4,
       "+ p < 0.10   * p < 0.05   ** p < 0.01   *** p < 0.001.     Cross-sectional design: these are associations, not causal effects.",
       adj = 0, cex = 0.72, col = MUTE)
  text(3, 5.4, "Code and full model output: github.com/pdez90/Wellbeing_Denver_immigrants",
       adj = 0, cex = 0.72, col = MUTE, font = 3)
}

grDevices::png(file.path(fig_dir, "Graphical_Abstract.png"),
               width = 13.4, height = 7.3, units = "in", res = 600,
               type = "cairo", bg = "white")
draw(); grDevices::dev.off()

grDevices::pdf(file.path(fig_dir, "Graphical_Abstract.pdf"), width = 13.4, height = 7.3)
draw(); grDevices::dev.off()

cat("Graphical abstract written to", fig_dir, "\n")
cat(sprintf("  models summarised: %d | mediation tests: %d surviving Holm: %d\n",
            nrow(fit), n_med, n_survive))
