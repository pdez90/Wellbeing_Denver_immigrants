# =============================================================================
# 23_figure_framework.R
# Figure 1: the conceptual framework, with neighborhood demographics shown as a
# separate block of controls rather than as a dimension of the built
# environment.
#
#   INPUT : none (the figure is a schematic of the argument, not of estimates)
#   OUTPUT: figures/Figure1_Framework.png / .pdf
#
# The distinction the figure has to carry is the one the analysis makes:
#
#   - Five BUILT ENVIRONMENT domains are the object of study. Four describe what
#     has been built; the fifth describes what regulation permits, and it is
#     drawn apart from the other four because the paper treats the contrast
#     between the two as a finding rather than an assumption.
#   - NEIGHBORHOOD DEMOGRAPHICS are not part of the built environment. They are
#     held constant so that built-environment estimates are not picking up who
#     lives nearby, and no coefficient on them is interpreted as an effect of
#     the built environment. They are drawn as a separate block feeding both
#     outcomes and the exposures, which is what adjusting for them assumes.
#   - INDIVIDUAL CHARACTERISTICS play the same role at the person level.
#
# Drawn with base graphics so the figure reproduces from a clean R installation.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
if (!exists(".wb_domains_loaded")) source("wb_domains.R")

fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Drawing helpers (shared conventions with 12_figure_mediation_dag.R)
# -----------------------------------------------------------------------------
rrect <- function(x, y, w, h, r = 1.6, border = "#222222", fill = "#FFFFFF",
                  lty = 1, lwd = 1.2) {
  arc <- function(cx, cy, a0, a1) {
    a <- seq(a0, a1, length.out = 16); cbind(cx + r * cos(a), cy + r * sin(a))
  }
  x0 <- x - w/2; x1 <- x + w/2; y0 <- y - h/2; y1 <- y + h/2
  p <- rbind(arc(x1 - r, y1 - r, 0, pi/2),
             arc(x0 + r, y1 - r, pi/2, pi),
             arc(x0 + r, y0 + r, pi, 3*pi/2),
             arc(x1 - r, y0 + r, 3*pi/2, 2*pi))
  polygon(p[, 1], p[, 2], border = border, col = fill, lty = lty, lwd = lwd)
}

wrap_text <- function(x, y, txt, w, cex, col = "#111111", font = 1) {
  words <- strsplit(txt, " ")[[1]]
  lines <- character(0); cur <- ""
  for (wd in words) {
    trial <- if (nchar(cur)) paste(cur, wd) else wd
    if (strwidth(trial, cex = cex, family = "serif") > w && nchar(cur)) {
      lines <- c(lines, cur); cur <- wd
    } else cur <- trial
  }
  lines <- c(lines, cur)
  lh <- strheight("Ag", cex = cex, family = "serif") * 1.45
  y0 <- y + (length(lines) - 1) * lh / 2
  for (i in seq_along(lines)) {
    text(x, y0 - (i - 1) * lh, lines[i], cex = cex, col = col,
         family = "serif", font = font)
  }
}

arrow_to <- function(x0, y0, x1, y1, col = "#111111", lty = 1, lwd = 1.5,
                     len = 0.11) {
  arrows(x0, y0, x1, y1, length = len, angle = 20, col = col, lty = lty, lwd = lwd)
}

curve_arrow <- function(x0, y0, x1, y1, bend = 0.18, col = "#8A8A8A",
                        lty = 2, lwd = 1.1) {
  t  <- seq(0, 1, length.out = 80)
  mx <- (x0 + x1)/2; my <- (y0 + y1)/2
  cx <- mx - bend * (y1 - y0); cy <- my + bend * (x1 - x0)
  bx <- (1-t)^2 * x0 + 2*(1-t)*t*cx + t^2*x1
  by <- (1-t)^2 * y0 + 2*(1-t)*t*cy + t^2*y1
  lines(bx, by, col = col, lty = lty, lwd = lwd)
  n <- length(t)
  arrows(bx[n-1], by[n-1], bx[n], by[n], length = 0.09, angle = 20,
         col = col, lty = 1, lwd = lwd)
}

plab <- function(x, y, txt, cex = 0.8, col = "#111111", font = 3) {
  # txt may be a string or a plotmath expression; strwidth handles both, and
  # plotmath is what keeps the prime in c-prime from failing on the pdf device,
  # where a literal U+2032 cannot be encoded.
  w <- strwidth(txt, cex = cex, font = font, family = "serif")
  h <- strheight(txt, cex = cex, font = font, family = "serif")
  rect(x - w/2 - 0.9, y - h/2 - 0.9, x + w/2 + 0.9, y + h/2 + 0.9,
       col = "white", border = NA)
  text(x, y, txt, cex = cex, font = font, col = col, family = "serif")
}

NBE_FILL  <- "#FFFFFF"
NBE_EDGE  <- "#1F4E5F"
REG_EDGE  <- "#7A5C2E"
CTRL_FILL <- "#F2F2F2"
CTRL_EDGE <- "#8A8A8A"
OUT_EDGE  <- "#A6293B"

# -----------------------------------------------------------------------------
# The figure
# -----------------------------------------------------------------------------
draw_figure <- function() {
  par(mar = c(0.3, 0.3, 0.3, 0.3), family = "serif")
  plot(NA, xlim = c(0, 160), ylim = c(0, 108), axes = FALSE, xlab = "", ylab = "",
       xaxs = "i", yaxs = "i")

  # ---- geometry, set once so the boxes and the paths cannot disagree ---------
  BX   <- 33; BW <- 48                      # domain boxes
  BH   <- 9.5
  ys   <- c(92, 80.5, 69, 57.5)             # the four "what is built" domains
  y_reg <- 44                               # regulation, set apart below them
  FR   <- list(x0 = 5, x1 = 61, y0 = 37.5, y1 = 98.5)   # the NBE frame

  x_nb <- 106; y_nb <- 84; w_nb <- 46; h_nb <- 12
  x_sw <- 132; y_sw <- 52; w_sw <- 46; h_sw <- 12

  CB <- list(x = 88, y = 15, w = 128, h = 24)           # the control block

  # ---- built environment -----------------------------------------------------
  rect(FR$x0, FR$y0, FR$x1, FR$y1, border = "#C9C4BC", lwd = 1)
  text((FR$x0 + FR$x1)/2, FR$y1 + 3.2, "Neighborhood built environment (NBE)",
       cex = 0.92, font = 2, family = "serif")

  built <- setdiff(names(WB_DOMAIN_VARS), "land_use")
  for (i in seq_along(built)) {
    rrect(BX, ys[i], BW, BH, border = NBE_EDGE, fill = NBE_FILL, lwd = 1.3)
    wrap_text(BX, ys[i], unname(WB_DOMAIN_LABELS[built[i]]), BW - 7, 0.78)
  }
  rrect(BX, y_reg, BW, BH + 1.5, border = REG_EDGE, fill = "#FBF7F0", lwd = 1.3)
  wrap_text(BX, y_reg, "Land use and regulation: what is permitted", BW - 9, 0.78,
            col = "#5C441F")

  # ---- outcomes --------------------------------------------------------------
  rrect(x_nb, y_nb, w_nb, h_nb, border = OUT_EDGE, fill = "#FFFFFF", lwd = 1.5)
  wrap_text(x_nb, y_nb, "Neighborhood belonging (NB)", w_nb - 8, 0.86)
  rrect(x_sw, y_sw, w_sw, h_sw, border = OUT_EDGE, fill = "#FFFFFF", lwd = 1.5)
  wrap_text(x_sw, y_sw, "Subjective well-being (SWB)", w_sw - 8, 0.86)

  # ---- controls: outside the NBE frame, and drawn to look it ------------------
  rrect(CB$x, CB$y, CB$w, CB$h, r = 2, border = CTRL_EDGE, fill = CTRL_FILL,
        lty = 2, lwd = 1.2)
  lines_ctl <- c(
    "Neighborhood demographics: poverty, foreign-born and non-Hispanic white shares,",
    "SES index, residential and experienced diversity",
    "Individual characteristics: age, gender, income, education, language,",
    "immigration status, length of residence")
  text(CB$x, CB$y + CB$h/2 - 3.4, "Held constant, not interpreted as built environment",
       cex = 0.74, font = 3, col = "#4A4A4A", family = "serif")
  for (i in seq_along(lines_ctl)) {
    text(CB$x, CB$y + CB$h/2 - 7.6 - (i - 1) * 3.9, lines_ctl[i], cex = 0.74,
         family = "serif")
  }

  # ---- paths -----------------------------------------------------------------
  # a: the built environment to belonging
  arrow_to(BX + BW/2 + 1.5, 80, x_nb - w_nb/2 - 1.5, y_nb - 2.5, col = NBE_EDGE)
  plab(78, 79, "a", cex = 0.95, col = NBE_EDGE)

  # c': the built environment direct to well-being
  arrow_to(BX + BW/2 + 1.5, 57, x_sw - w_sw/2 - 1.5, y_sw + 1.5, col = NBE_EDGE)
  plab(76, 55.8, expression(paste(italic("c"), minute)), cex = 0.95, col = NBE_EDGE)

  # b: belonging to well-being
  arrow_to(x_nb + w_nb/2 - 6, y_nb - h_nb/2 - 1.5,
           x_sw - 6, y_sw + h_sw/2 + 1.5, col = OUT_EDGE)
  plab(126, 69, "b", cex = 0.95, col = OUT_EDGE)

  # adjustments: from the control block to the exposures and both outcomes
  curve_arrow(CB$x - CB$w/2 + 8, CB$y + CB$h/2 + 1, BX + 6, FR$y0 - 1.5, bend = 0.06)
  curve_arrow(CB$x - 6, CB$y + CB$h/2 + 1, x_nb - 8, y_nb - h_nb/2 - 1.5, bend = -0.05)
  curve_arrow(CB$x + CB$w/2 - 10, CB$y + CB$h/2 + 1, x_sw - 4, y_sw - h_sw/2 - 1.5,
              bend = -0.08)

  # ---- reading note, in the empty quarter above the outcome boxes ------------
  text(158, 103, "Solid paths are estimated and reported; dashed paths are adjustments.",
       cex = 0.74, font = 3, col = "#4A4A4A", family = "serif", adj = 1)
}

W <- 9.2; H <- 6.2
grDevices::png(file.path(fig_dir, "Figure1_Framework.png"),
               width = W, height = H, units = "in", res = 600,
               type = "cairo", bg = "white")
draw_figure(); grDevices::dev.off()
grDevices::pdf(file.path(fig_dir, "Figure1_Framework.pdf"), width = W, height = H)
draw_figure(); grDevices::dev.off()

cat("\nFigure 1 written to", fig_dir, "\n")
cat("  five built-environment domains, with land use drawn apart from the four\n")
cat("  that describe what has been built; neighborhood demographics and\n")
cat("  individual characteristics shown as controls, outside the NBE box.\n")
