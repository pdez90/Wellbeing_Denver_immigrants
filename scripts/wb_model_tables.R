# =============================================================================
# wb_model_tables.R
# Build regression tables as Word documents WITHOUT pandoc.
#
# modelsummary writes .docx by handing off to pandoc, an external binary that
# is not installed with R and that R launched from a terminal does not inherit
# from RStudio. officer writes the Word XML directly from R, so it has no
# external dependency at all. This file reimplements the modelsummary table
# layout on top of officer + flextable, both of which are already required by
# 09_mediation.R.
#
# Sourced by 08_domain_models.R. Not run on its own.
# =============================================================================

# Significance stars, matching modelsummary's default cutoffs.
wb_stars <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.001, "***",
      ifelse(p < 0.01, "**",
        ifelse(p < 0.05, "*",
          ifelse(p < 0.10, "+", "")))))
}

# Assemble a coefficient table: one row per term for the estimate, one
# unlabelled row beneath it for the standard error, then goodness-of-fit rows.
# Term order follows first appearance across the models, which keeps related
# predictors adjacent instead of alphabetising them apart.
wb_build_model_table <- function(models, digits = 3) {

  tidied <- lapply(names(models), function(nm) {
    m <- models[[nm]]
    co <- summary(m)$coefficients
    data.frame(
      model    = nm,
      term     = rownames(co),
      estimate = co[, 1],
      se       = co[, 2],
      p        = co[, 4],
      stringsAsFactors = FALSE
    )
  })
  tidied <- do.call(rbind, tidied)

  terms <- unique(tidied$term)
  model_names <- names(models)

  fmt <- paste0("%.", digits, "f")

  # Accumulate rows in a list and rbind once. Growing a data.frame by index
  # assignment silently introduces NA-filled rows when the frame starts empty.
  rows <- list()

  for (tm in terms) {
    est_row <- character(0)
    se_row  <- character(0)
    for (mn in model_names) {
      hit <- tidied[tidied$model == mn & tidied$term == tm, ]
      if (nrow(hit) == 0) {
        est_row <- c(est_row, "")
        se_row  <- c(se_row, "")
      } else {
        est_row <- c(est_row, paste0(sprintf(fmt, hit$estimate[1]), wb_stars(hit$p[1])))
        se_row  <- c(se_row,  paste0("(", sprintf(fmt, hit$se[1]), ")"))
      }
    }
    rows[[length(rows) + 1]] <- c(tm, est_row)
    rows[[length(rows) + 1]] <- c("", se_row)
  }

  gof_row <- function(label, f, fmt_str) {
    c(label, vapply(model_names, function(mn) sprintf(fmt_str, f(models[[mn]])), character(1)))
  }
  rows[[length(rows) + 1]] <- gof_row("Num.Obs.", function(m) length(stats::residuals(m)), "%.0f")
  rows[[length(rows) + 1]] <- gof_row("R2",       function(m) summary(m)$r.squared,        "%.3f")
  rows[[length(rows) + 1]] <- gof_row("R2 Adj.",  function(m) summary(m)$adj.r.squared,    "%.3f")

  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c(" ", model_names)
  rownames(out) <- NULL
  out
}

# Write one table to .docx via officer. No pandoc involved.
wb_write_model_docx <- function(models, path, title, digits = 3) {
  tab <- wb_build_model_table(models, digits = digits)
  n_gof <- 3
  gof_start <- nrow(tab) - n_gof + 1

  ft <- flextable::flextable(tab)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft, size = 8, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  if (ncol(tab) > 1) {
    ft <- flextable::align(ft, j = 2:ncol(tab), align = "center", part = "all")
  }
  ft <- flextable::hline(ft, i = gof_start - 1,
                         border = officer::fp_border(color = "black", width = 1))
  ft <- flextable::autofit(ft)
  ft <- flextable::add_footer_lines(
    ft,
    paste0("Standard errors in parentheses. ",
           "+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001. ",
           "Continuous neighborhood variables are standardized.")
  )

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  doc <- flextable::body_add_flextable(doc, ft)
  print(doc, target = path)

  invisible(path)
}

# CSV twin of the same layout, so the exact table can be pasted anywhere.
wb_write_model_csv <- function(models, path, digits = 3) {
  utils::write.csv(wb_build_model_table(models, digits = digits),
                   path, row.names = FALSE, na = "")
  invisible(path)
}
