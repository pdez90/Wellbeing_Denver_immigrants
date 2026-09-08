# =============================================================================
# 19_figure_integrated_model.R
# Figure 3: the integrated model as a coefficient plot, replacing Table 2.
#
#   INPUT : model_objects.rds  (from 08_domain_models.R)
#   OUTPUT: figures/Figure3_IntegratedModel.png / .pdf
#           figures/Figure3_IntegratedModel.csv   (the plotted numbers)
#
# The three columns of Table 2 are three regressions on the same 238
# respondents: subjective well-being, neighborhood belonging, and subjective
# well-being with belonging added as a predictor. This script refits exactly
# those three models -- the same call 11_manuscript_tables.R makes for Table 8 --
# and draws them as one forest plot, so the same content costs no words against
# the manuscript limit.
#
# Every number drawn comes from the refit; nothing is transcribed by hand.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
if (!exists("WB_LABELS")) source("wb_labels.R")
wb_require(c("readr", "dplyr"))

fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

obj       <- readRDS(file.path(out_dir, "model_objects.rds"))
model_dat <- obj$model_dat
IC        <- obj$individual_controls
context_z <- obj$context_z
final_be  <- obj$final_be

m_swb <- make_lm("swb_z",       unique(c(IC, context_z, final_be)), model_dat)
m_bel <- make_lm("belonging_z", unique(c(IC, context_z, final_be)), model_dat)
m_swb_bel <- make_lm("swb_z", unique(c("belonging_z", IC, context_z, final_be)),
                     model_dat)

# -----------------------------------------------------------------------------
# Which rows appear, and in what order
# -----------------------------------------------------------------------------
# The figure shows the built-environment terms plus belonging: the substantive
# content of Table 2. Individual controls and neighborhood socioeconomic context
# stay in the models and are reported in Section S2, but plotting 29 rows would
# make the figure unreadable.
ROWS <- c("belonging_z", final_be)

lab_of <- function(term) {
  z <- sub("^zone_category", "", term)
  if (z != term) return(paste0("Zoning: ", tolower(z)))
  if (term == "belonging_z") return("Neighborhood belonging")
  if (!is.na(WB_LABELS[term])) return(unname(WB_LABELS[term]))
  term
}

grab <- function(model, term) {
  s <- summary(model)$coefficients
  if (!term %in% rownames(s)) return(c(NA_real_, NA_real_, NA_real_))
  c(s[term, 1], s[term, 2], s[term, 4])
}

rows <- lapply(ROWS, function(t) {
  a <- grab(m_swb, t); b <- grab(m_bel, t); c3 <- grab(m_swb_bel, t)
  data.frame(term = t, label = lab_of(t),
             swb_est = a[1], swb_se = a[2], swb_p = a[3],
             bel_est = b[1], bel_se = b[2], bel_p = b[3],
             swbb_est = c3[1], swbb_se = c3[2], swbb_p = c3[3],
             stringsAsFactors = FALSE)
})
D <- do.call(rbind, rows)

# belonging is only defined in the third model; drop the two empty series
D$swb_est[D$term == "belonging_z"]  <- NA_real_
D$bel_est[D$term == "belonging_z"]  <- NA_real_

for (k in c("swb", "bel", "swbb")) {
  D[[paste0(k, "_lo")]] <- D[[paste0(k, "_est")]] - 1.96 * D[[paste0(k, "_se")]]
  D[[paste0(k, "_hi")]] <- D[[paste0(k, "_est")]] + 1.96 * D[[paste0(k, "_se")]]
}
readr::write_csv(D, file.path(fig_dir, "Figure3_IntegratedModel.csv"))

N_OBS <- stats::nobs(m_swb)
FITS <- data.frame(
  model = c("SWB", "Belonging", "SWB + belonging"),
  adj_r2 = c(summary(m_swb)$adj.r.squared, summary(m_bel)$adj.r.squared,
             summary(m_swb_bel)$adj.r.squared),
  n = c(stats::nobs(m_swb), stats::nobs(m_bel), stats::nobs(m_swb_bel)))
readr::write_csv(FITS, file.path(fig_dir, "Figure3_IntegratedModel_fit.csv"))

# -----------------------------------------------------------------------------
# Drawing. Base graphics, so the figure reproduces on any machine with R.
# -----------------------------------------------------------------------------
COL <- c(swb = "#A6293B", bel = "#1F4E5F", swbb = "#C77B30")
SER <- c(swb = "Subjective well-being", bel = "Neighborhood belonging",
         swbb = "Subjective well-being, belonging added")
PCH <- c(swb = 21, bel = 24, swbb = 22)
GRID <- "#E4E4E4"; LEADER <- "#CFCFCF"

draw <- function() {
  # The left margin has to hold the longest predictor label; 11 lines is enough
  # for "Short-trip opportunity zone share" at cex 0.82 and keeps the plotting
  # region wide enough that the three series stay legible.
  par(mar = c(7.4, 11.2, 3.4, 0.8), family = "serif")

  xr <- range(c(D$swb_lo, D$swb_hi, D$bel_lo, D$bel_hi, D$swbb_lo, D$swbb_hi),
              na.rm = TRUE)
  xr <- c(min(xr[1], -0.1), max(xr[2], 0.1))
  xr <- xr + c(-0.04, 0.04) * diff(xr)

  n <- nrow(D)
  plot(NA, xlim = xr, ylim = c(n + 0.6, 0.4), axes = FALSE, xlab = "", ylab = "")

  ticks <- pretty(xr, n = 5)
  ticks <- ticks[ticks >= xr[1] & ticks <= xr[2]]
  abline(v = ticks, col = GRID, lwd = 0.7)
  abline(v = 0, col = "#777777", lwd = 1.1)
  axis(1, at = ticks, labels = sprintf("%.1f", ticks), cex.axis = 0.8,
       col = "#777777", col.axis = "#333333", tck = -0.012, mgp = c(2, 0.5, 0))
  mtext("Standardized coefficient (95% CI)", side = 1, line = 2.1, cex = 0.85)

  # offsets so the three series never overplot one another
  off <- c(swb = -0.22, bel = 0, swbb = 0.22)

  for (i in seq_len(n)) {
    segments(xr[1], i, xr[2], i, col = LEADER, lwd = 0.4, lty = 3)
    text(xr[1] - 0.02 * diff(xr), i, D$label[i], xpd = NA, adj = 1, cex = 0.82)
    for (k in names(COL)) {
      est <- D[[paste0(k, "_est")]][i]
      if (is.na(est)) next
      lo <- D[[paste0(k, "_lo")]][i]; hi <- D[[paste0(k, "_hi")]][i]
      p  <- D[[paste0(k, "_p")]][i]
      y  <- i + off[[k]]
      segments(lo, y, hi, y, col = COL[[k]], lwd = 1.6)
      points(est, y, pch = PCH[[k]], cex = 0.95, lwd = 1.3,
             col = COL[[k]], bg = if (!is.na(p) && p < 0.05) COL[[k]] else "white")
    }
  }

  title(main = "The integrated model", adj = 0, line = 2.0, cex.main = 1.15)
  mtext(sprintf("n = %d; a filled marker means p < 0.05", N_OBS),
        side = 3, adj = 0, line = 0.7, cex = 0.85, col = "#555555")

  # The legend is drawn by hand in the bottom margin, one row of three entries
  # below the axis title. legend() placed inside the panel would sit on top of
  # the lowest intervals, and its automatic placement is unreliable on a
  # reversed y axis.
  ly <- n + 3.0
  xs <- xr[1] + c(0, 0.32, 0.63) * diff(xr)
  for (k in seq_along(COL)) {
    nm <- names(COL)[k]
    points(xs[k], ly, pch = PCH[[nm]], cex = 0.95, lwd = 1.3, xpd = NA,
           col = COL[[nm]], bg = COL[[nm]])
    text(xs[k] + 0.022 * diff(xr), ly, SER[[nm]], adj = 0, cex = 0.72, xpd = NA)
  }
}

grDevices::png(file.path(fig_dir, "Figure3_IntegratedModel.png"),
               width = 8.0, height = 6.2, units = "in", res = 600,
               type = "cairo", bg = "white")
draw(); grDevices::dev.off()
grDevices::pdf(file.path(fig_dir, "Figure3_IntegratedModel.pdf"),
               width = 8.0, height = 6.2)
draw(); grDevices::dev.off()

cat("\nFigure 3 written to", fig_dir, "\n")
cat(sprintf("  %d terms plotted, n = %d\n", nrow(D), N_OBS))
print(FITS)
cat("\nA filled marker means p < 0.05; an open marker means it does not reach it.\n")
