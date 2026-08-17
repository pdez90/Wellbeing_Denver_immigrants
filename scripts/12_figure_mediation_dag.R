# =============================================================================
# 12_figure_mediation_dag.R
# Figure 1: the mediation model estimated in 09_mediation.R, drawn as a path
# diagram.
#
#   INPUT : none (the figure is a schematic of the estimating equations)
#   OUTPUT: figures/Figure1_Mediation_DAG.png
#           figures/Figure1_Mediation_DAG.pdf
#
# Drawn with base graphics so the figure is reproducible from a clean R
# installation with no additional packages.
#
# The letters on the paths correspond to the two estimating equations in
# 09_mediation.R:
#
#   mediator equation  M = a*X + g1*C + e1
#   outcome  equation  Y = c'*X + b*M + g2*C + e2
#
#   ACME  = a*b        ADE = c'        total = a*b + c'
#
# No treatment-mediator interaction is fitted, so ACME does not vary with X.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")

fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Drawing helpers
# -----------------------------------------------------------------------------

# Rounded rectangle, drawn as a polygon so no extra package is needed.
rrect <- function(x, y, w, h, r = 1.4, border = "#222222", fill = "#FFFFFF",
                  lty = 1, lwd = 1.2) {
  x0 <- x - w / 2; x1 <- x + w / 2
  y0 <- y - h / 2; y1 <- y + h / 2
  th <- seq(0, pi / 2, length.out = 12)
  px <- c(x1 - r + r * cos(rev(th)),  x0 + r - r * sin(th),
          x0 + r - r * cos(rev(th)),  x1 - r + r * sin(th))
  py <- c(y1 - r + r * sin(rev(th)),  y1 - r + r * cos(th),
          y0 - r + r * sin(rev(th)) + 2 * r - 2 * r,  y0 + r - r * cos(th))
  # rebuild explicitly, corner by corner, to keep the path unambiguous
  arc <- function(cx, cy, a0, a1) {
    a <- seq(a0, a1, length.out = 14)
    cbind(cx + r * cos(a), cy + r * sin(a))
  }
  p <- rbind(
    arc(x1 - r, y1 - r, 0,          pi / 2),        # top right
    arc(x0 + r, y1 - r, pi / 2,     pi),            # top left
    arc(x0 + r, y0 + r, pi,         3 * pi / 2),    # bottom left
    arc(x1 - r, y0 + r, 3 * pi / 2, 2 * pi)         # bottom right
  )
  polygon(p[, 1], p[, 2], border = border, col = fill, lty = lty, lwd = lwd)
}

nodebox <- function(x, y, w, h, title, sub = NULL, dashed = FALSE) {
  rrect(x, y, w, h,
        fill = if (dashed) "#F5F5F5" else "#FFFFFF",
        lty  = if (dashed) 2 else 1)
  text(x, y + if (is.null(sub)) 0 else 2.0, title, cex = 0.95, family = "serif")
  if (!is.null(sub)) {
    text(x, y - 3.0, sub, cex = 0.72, col = "#3A3A3A", family = "serif")
  }
}

# Straight arrow.
path_arrow <- function(x0, y0, x1, y1, col = "#111111", lty = 1, lwd = 1.4) {
  arrows(x0, y0, x1, y1, length = 0.10, angle = 20, col = col, lty = lty, lwd = lwd)
}

# Quadratic Bezier arrow, used for the covariate paths so they do not run
# straight through the other nodes.
curve_arrow <- function(x0, y0, x1, y1, bend = 0.16, col = "#8A8A8A",
                        lty = 2, lwd = 1.1) {
  t  <- seq(0, 1, length.out = 60)
  mx <- (x0 + x1) / 2; my <- (y0 + y1) / 2
  dx <- x1 - x0;       dy <- y1 - y0
  cx <- mx - bend * dy; cy <- my + bend * dx        # control point, offset normal
  bx <- (1 - t)^2 * x0 + 2 * (1 - t) * t * cx + t^2 * x1
  by <- (1 - t)^2 * y0 + 2 * (1 - t) * t * cy + t^2 * y1
  lines(bx, by, col = col, lty = lty, lwd = lwd)
  n <- length(t)
  arrows(bx[n - 1], by[n - 1], bx[n], by[n], length = 0.09, angle = 20,
         col = col, lty = 1, lwd = lwd)
}

# Label with a white patch behind it so it reads where paths cross.
plab <- function(x, y, txt, cex = 0.95, font = 3) {
  w <- strwidth(txt, cex = cex, font = font, family = "serif")
  h <- strheight(txt, cex = cex, font = font, family = "serif")
  rect(x - w / 2 - 0.8, y - h / 2 - 0.8, x + w / 2 + 0.8, y + h / 2 + 0.8,
       col = "white", border = NA)
  text(x, y, txt, cex = cex, font = font, family = "serif")
}

# -----------------------------------------------------------------------------
# The figure
# -----------------------------------------------------------------------------
draw_figure <- function() {
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0, 140), ylim = c(0, 62), asp = 1)

  nodebox(26, 30, 48, 14, "Built environment measure  (X)",
          "800 m buffer exposure, standardized")
  nodebox(70, 52, 40, 12, "Neighborhood belonging  (M)",
          bquote("4-item scale, " * alpha * " = 0.79"))
  nodebox(114, 30, 44, 14, "Subjective well-being  (Y)",
          bquote("5-item scale, " * alpha * " = 0.91"))
  nodebox(70, 8, 82, 12, "Covariates  (C)",
          paste("Individual characteristics + neighborhood socioeconomic context",
                "(same set in the mediator and the outcome equation)", sep = "\n"),
          dashed = TRUE)

  # covariate paths first, so the structural paths sit on top of them
  curve_arrow(46, 14.2, 24, 22.6, bend =  0.20)
  curve_arrow(94, 14.2, 116, 22.6, bend = -0.20)
  curve_arrow(70, 14.2, 70, 45.6, bend = 0)

  path_arrow(48.5, 37.2, 56.0, 46.2)          # a
  path_arrow(84.0, 46.2, 91.5, 37.2)          # b
  path_arrow(50.5, 30.0, 91.5, 30.0)          # c'

  plab(49.0, 43.0, "a")
  plab(91.0, 43.0, "b")
  plab(70.0, 33.2, "c'   (direct effect, ADE)", cex = 0.85)
}

png(file.path(fig_dir, "Figure1_Mediation_DAG.png"),
    width = 9.4, height = 4.4, units = "in", res = 600, type = "cairo",
    bg = "white")
draw_figure()
dev.off()

pdf(file.path(fig_dir, "Figure1_Mediation_DAG.pdf"),
    width = 9.4, height = 4.4)
draw_figure()
dev.off()

cat("Figure 1 written to", fig_dir, "\n")
