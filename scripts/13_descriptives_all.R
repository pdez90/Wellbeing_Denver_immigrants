# =============================================================================
# 13_descriptives_all.R
# A descriptive table and a distribution figure for EVERY variable used in the
# analysis -- outcomes, the items the two scales are built from, individual
# characteristics, and all five built-environment domains.
#
#   INPUT : model_objects.rds  (from 08_domain_models.R; supplies `dat_derived`,
#           the derived respondent frame, so the recoding is defined in exactly
#           one place)
#   OUTPUT: descriptives/Table_A1 ... Table_A9  (.csv)
#           descriptives/figures/<variable>.png       one per variable
#           descriptives/figures/Panel_<group>.png    all variables in a group
#           descriptives/Descriptive_Statistics_Appendix.docx
#
# Variables are described on their ORIGINAL scales, not the z-scores the models
# use, because a standardized mean of 0 and SD of 1 tells the reader nothing.
# The z-scores are a linear rescaling of these columns; script 08 builds them.
#
# Continuous variables: N, missing, mean, SD, min, P25, median, P75, max.
# Binary indicators and categorical variables: count and percentage per level,
# with the denominator being non-missing responses.
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
wb_require(c("tidyverse", "readr", "flextable", "officer"))

out_desc <- file.path(out_dir, "descriptives")
out_fig  <- file.path(out_desc, "figures")
dir.create(out_fig, showWarnings = FALSE, recursive = TRUE)

obj <- readRDS(file.path(out_dir, "model_objects.rds"))
if (is.null(obj$dat_derived)) {
  stop("model_objects.rds predates 13_descriptives_all.R. Re-run 08_domain_models.R ",
       "so the derived respondent frame is saved.")
}
dat <- obj$dat_derived

# -----------------------------------------------------------------------------
# 1. Variable registry
#
# Every row is one variable that enters the analysis. `group` controls which
# table and which figure panel it appears in; `unit` is printed in the tables so
# the reader can interpret the mean without hunting through the methods section.
# -----------------------------------------------------------------------------

V <- function(name, label, unit, group, type = "continuous") {
  data.frame(name = name, label = label, unit = unit, group = group,
             type = type, stringsAsFactors = FALSE)
}

VARS <- rbind(
  # ---- outcomes -------------------------------------------------------------
  V("swb_index",       "Subjective well-being index",  "1-5 scale", "Outcomes"),
  V("belonging_index", "Neighborhood belonging index", "1-5 scale", "Outcomes"),

  # ---- the items the two scales are built from ------------------------------
  V("sat_stand_liv",     "Satisfaction: standard of living",   "1-5", "Scale items"),
  V("sat_health",        "Satisfaction: health",               "1-5", "Scale items"),
  V("sat_life_achieve",  "Satisfaction: life achievement",     "1-5", "Scale items"),
  V("feel_safe",         "Feels safe",                         "1-5", "Scale items"),
  V("feel_accepted",     "Feels accepted",                     "1-5", "Scale items"),
  V("feel_conf_finance", "Confident about finances",           "1-5", "Scale items"),
  V("hood_belong",       "Feels a sense of belonging",         "1-5", "Scale items"),
  V("hood_trust_people", "Trusts people in the neighborhood",  "1-5", "Scale items"),
  V("hood_borrow",       "Could borrow from a neighbor",       "1-5", "Scale items"),
  V("hood_contacts",     "Has contacts in the neighborhood",   "1-5", "Scale items"),

  # ---- individual characteristics ------------------------------------------
  V("age",              "Age",                    "years",           "Individual characteristics"),
  V("children",         "Number of children",     "count",           "Individual characteristics"),
  V("income_hh",        "Household income",       "ordinal band",    "Individual characteristics"),
  V("edu_level",        "Education",              "ordinal level",   "Individual characteristics"),
  V("engl_speak",       "English-speaking proficiency", "1-5",       "Individual characteristics"),
  V("time_live_denver", "Time in Denver metro",   "ordinal band",    "Individual characteristics"),
  V("time_live_hood",   "Time in current neighborhood", "ordinal band", "Individual characteristics"),

  V("ind_female",       "Female",                                 "0/1", "Individual characteristics", "binary"),
  V("ind_married",      "Married",                                "0/1", "Individual characteristics", "binary"),
  V("ind_prev_married", "Previously married (sep./div./widowed)", "0/1", "Individual characteristics", "binary"),
  V("ind_hispanic",     "Hispanic",                               "0/1", "Individual characteristics", "binary"),
  V("ind_race_white",   "Race: White",                            "0/1", "Individual characteristics", "binary"),
  V("ind_lpr",          "Lawful permanent resident",              "0/1", "Individual characteristics", "binary"),
  V("ind_undocumented", "No official status (undocumented)",      "0/1", "Individual characteristics", "binary"),
  V("ind_imm_other",    "DACA, student/work visa, refugee, asylee", "0/1", "Individual characteristics", "binary"),

  # ---- neighborhood socioeconomic context ----------------------------------
  V("pop_density",            "Population density",     "persons/km\u00b2",       "Neighborhood socioeconomic context"),
  V("housing_density",        "Housing density",        "housing units/km\u00b2", "Neighborhood socioeconomic context"),
  V("dist_downtown_km",       "Distance to downtown Denver", "km",           "Neighborhood socioeconomic context"),
  V("pct_poverty",            "Population below poverty",    "%",            "Neighborhood socioeconomic context"),
  V("pct_non_native",         "Foreign-born population",     "%",            "Neighborhood socioeconomic context"),
  V("neighborhood_ses_index", "Neighborhood SES index",      "index",        "Neighborhood socioeconomic context"),

  # ---- urban form -----------------------------------------------------------
  V("walk_nat_walk_ind",          "EPA National Walkability Index", "index, 1-20",           "Urban form"),
  V("street_intdensity",          "Street intersection density",    "intersections/sq mile", "Urban form"),
  V("urban_center_nearest_dist_m","Distance to nearest urban center", "m",                   "Urban form"),

  # ---- transportation and accessibility -------------------------------------
  V("hudjob_jobs_idx",             "HUD Jobs Proximity Index",    "index, 0-100", "Transportation and accessibility"),
  V("sidewalk_density_800",        "Sidewalk density, 800 m",     "m/km\u00b2",        "Transportation and accessibility"),
  V("bike_facility_density_800",   "Bicycle facility density, 800 m", "m/km\u00b2",    "Transportation and accessibility"),
  V("active_corridor_density_800", "Active corridor density, 800 m",  "m/km\u00b2",    "Transportation and accessibility"),
  V("ht_t_ami",                    "Transportation cost at area median income", "% of income", "Transportation and accessibility"),

  # ---- greenness and parks --------------------------------------------------
  V("tree_tes",                    "Tree Equity Score",                 "score, 0-100", "Greenness and parks"),
  V("tree_treecanopy",             "Tree canopy share (Tree Equity)",   "share, 0-1",   "Greenness and parks"),
  V("park_acres_half_mile",        "Park acreage within 800 m",         "acres",        "Greenness and parks"),
  V("park_nearest_dist_m",         "Distance to nearest park",          "m",            "Greenness and parks"),
  V("lc_800m_tree_canopy",         "Tree canopy land cover, 800 m",     "share of buffer", "Greenness and parks"),
  V("lc_800m_impervious_surfaces", "Impervious surface land cover, 800 m", "share of buffer", "Greenness and parks"),

  # ---- safety and social environment ----------------------------------------
  V("crash_density_800",        "Crash density, 800 m",            "crashes/km\u00b2",     "Safety and social environment"),
  V("ped_crash_density_800",    "Pedestrian crash density, 800 m", "crashes/km\u00b2",     "Safety and social environment"),
  V("bike_crash_density_800",   "Bicycle crash density, 800 m",    "crashes/km\u00b2",     "Safety and social environment"),
  V("short_trip_zone_share_800","Short-trip opportunity zone share, 800 m", "share of buffer", "Safety and social environment"),
  V("div_total_diversity_resi", "Residential diversity",           "index, 0-1",      "Safety and social environment"),
  V("div_exposure_mean",        "Experienced diversity",           "index, 0-1",      "Safety and social environment"),

  # ---- land use and regulation ----------------------------------------------
  V("zone_category",   "Zoning category",                     "category", "Land use and regulation", "categorical"),
  V("zone_adu_yes",    "Accessory dwelling units permitted",  "0/1",      "Land use and regulation", "binary"),
  V("pfa_share_800",   "Pedestrian focus area share, 800 m",  "share of buffer", "Land use and regulation")
)

GROUPS <- unique(VARS$group)

missing_vars <- setdiff(VARS$name, names(dat))
if (length(missing_vars)) {
  stop("13_descriptives_all.R: these registry variables are absent from the ",
       "derived data:\n  ", paste(missing_vars, collapse = ", "),
       "\nRe-run the pipeline from 01, or correct the registry.")
}

# Anything the models use that the registry forgot. Better a loud failure here
# than a silently incomplete appendix.
model_side <- setdiff(
  unique(c(obj$individual_controls, obj$context_vars, unlist(obj$domain_vars),
           "swb_index", "belonging_index")),
  VARS$name
)
if (length(model_side)) {
  stop("13_descriptives_all.R: variables used in the models but missing from ",
       "the registry:\n  ", paste(model_side, collapse = ", "))
}

# -----------------------------------------------------------------------------
# 2. Summary tables
# -----------------------------------------------------------------------------

fmt <- function(x) {
  ifelse(is.na(x), "",
         ifelse(abs(x) >= 1000, formatC(x, format = "f", big.mark = ",", digits = 2),
                formatC(x, format = "f", digits = 2)))
}

describe_continuous <- function(v) {
  x  <- suppressWarnings(as.numeric(dat[[v$name]]))
  ok <- x[!is.na(x)]
  q  <- if (length(ok)) stats::quantile(ok, c(.25, .5, .75), names = FALSE) else rep(NA_real_, 3)
  data.frame(
    Variable = v$label, Unit = v$unit,
    N = length(ok), Missing = sum(is.na(x)),
    Mean = fmt(mean(ok)), SD = fmt(stats::sd(ok)),
    Min = fmt(min(ok)), P25 = fmt(q[1]), Median = fmt(q[2]),
    P75 = fmt(q[3]), Max = fmt(max(ok)),
    stringsAsFactors = FALSE
  )
}

describe_levels <- function(v) {
  x <- dat[[v$name]]
  if (v$type == "binary") {
    x <- factor(ifelse(is.na(x), NA, ifelse(x == 1, "Yes", "No")), levels = c("No", "Yes"))
  } else {
    x <- factor(x)
  }
  tab <- table(x, useNA = "no")
  data.frame(
    Variable = c(v$label, rep("", max(0, length(tab) - 1))),
    Category = names(tab),
    N        = as.integer(tab),
    Percent  = fmt(100 * as.numeric(tab) / sum(tab)),
    Missing  = c(sum(is.na(x)), rep(NA_integer_, max(0, length(tab) - 1))),
    stringsAsFactors = FALSE
  )
}

cont_tables <- list()
cat_tables  <- list()

for (g in GROUPS) {
  rows <- VARS[VARS$group == g, ]
  cont <- rows[rows$type == "continuous", ]
  cats <- rows[rows$type != "continuous", ]

  if (nrow(cont)) {
    tb <- do.call(rbind, lapply(seq_len(nrow(cont)), function(i) describe_continuous(cont[i, ])))
    cont_tables[[g]] <- tb
  }
  if (nrow(cats)) {
    tb <- do.call(rbind, lapply(seq_len(nrow(cats)), function(i) describe_levels(cats[i, ])))
    tb$Missing[is.na(tb$Missing)] <- ""
    cat_tables[[g]] <- tb
  }
}

safe <- function(s) gsub("[^A-Za-z0-9]+", "_", s)

tab_i <- 0
table_index <- data.frame()
for (g in GROUPS) {
  if (!is.null(cont_tables[[g]])) {
    tab_i <- tab_i + 1
    f <- file.path(out_desc, sprintf("Table_A%d_%s_continuous.csv", tab_i, safe(g)))
    readr::write_csv(cont_tables[[g]], f)
    table_index <- rbind(table_index, data.frame(table = sprintf("A%d", tab_i),
                                                 group = g, kind = "continuous", file = basename(f)))
  }
  if (!is.null(cat_tables[[g]])) {
    tab_i <- tab_i + 1
    f <- file.path(out_desc, sprintf("Table_A%d_%s_categorical.csv", tab_i, safe(g)))
    readr::write_csv(cat_tables[[g]], f)
    table_index <- rbind(table_index, data.frame(table = sprintf("A%d", tab_i),
                                                 group = g, kind = "categorical", file = basename(f)))
  }
}
readr::write_csv(table_index, file.path(out_desc, "Table_index.csv"))

# ---- coverage: how many respondents each variable is observed for ------------
coverage <- do.call(rbind, lapply(seq_len(nrow(VARS)), function(i) {
  x <- dat[[VARS$name[i]]]
  data.frame(Variable = VARS$label[i], Column = VARS$name[i], Group = VARS$group[i],
             Observed = sum(!is.na(x)), Missing = sum(is.na(x)),
             Percent_missing = fmt(100 * mean(is.na(x))), stringsAsFactors = FALSE)
}))
readr::write_csv(coverage, file.path(out_desc, "Variable_coverage.csv"))

# -----------------------------------------------------------------------------
# 3. Figures -- one per variable, plus one panel per group
# -----------------------------------------------------------------------------

draw_one <- function(v, cex_axis = 0.9, main_cex = 1.0) {
  x <- dat[[v$name]]
  if (v$type == "continuous") {
    x <- suppressWarnings(as.numeric(x)); x <- x[!is.na(x)]
    if (!length(x)) { plot.new(); title(v$label, cex.main = main_cex); return(invisible()) }
    br <- max(8, min(30, ceiling(sqrt(length(x)))))
    h  <- graphics::hist(x, breaks = br, plot = FALSE)
    graphics::plot(h, col = "#D9E2EC", border = "#5B7185", freq = TRUE,
                   main = "", xlab = v$unit, ylab = "Respondents",
                   cex.axis = cex_axis, cex.lab = cex_axis * 1.02)
    graphics::abline(v = mean(x),           col = "#B23A48", lwd = 2)
    graphics::abline(v = stats::median(x),  col = "#1F4E5F", lwd = 2, lty = 2)
    graphics::rug(x, col = "#7A7A7A", lwd = 0.4)
    graphics::title(main = v$label, cex.main = main_cex, font.main = 1)
    # put the legend over the emptier half of the plot
    side <- if (sum(h$counts[seq_len(ceiling(length(h$counts) / 2))]) >
                sum(h$counts[-seq_len(ceiling(length(h$counts) / 2))]))
              "topright" else "topleft"
    graphics::legend(side, bty = "n", cex = cex_axis * 0.82,
                     legend = c(sprintf("mean %s", fmt(mean(x))),
                                sprintf("median %s", fmt(stats::median(x))),
                                sprintf("n = %d", length(x))),
                     col = c("#B23A48", "#1F4E5F", NA), lty = c(1, 2, NA), lwd = c(2, 2, NA))
  } else {
    if (v$type == "binary") {
      x <- factor(ifelse(is.na(x), NA, ifelse(x == 1, "Yes", "No")), levels = c("No", "Yes"))
    } else {
      x <- factor(x)
    }
    tab <- table(x, useNA = "no")
    if (!length(tab)) { plot.new(); title(v$label, cex.main = main_cex); return(invisible()) }
    lab <- names(tab)
    lab <- ifelse(nchar(lab) > 18, paste0(substr(lab, 1, 16), "..."), lab)
    bp <- graphics::barplot(as.numeric(tab), names.arg = lab, col = "#D9E2EC",
                            border = "#5B7185", ylab = "Respondents", las = 1,
                            cex.names = cex_axis * 0.9, cex.axis = cex_axis,
                            cex.lab = cex_axis * 1.02,
                            ylim = c(0, max(tab) * 1.20))
    graphics::text(bp, as.numeric(tab), pos = 3, cex = cex_axis * 0.85,
                   labels = sprintf("%d (%s%%)", as.integer(tab),
                                    fmt(100 * as.numeric(tab) / sum(tab))))
    graphics::title(main = v$label, cex.main = main_cex, font.main = 1)
  }
}

# one file per variable
for (i in seq_len(nrow(VARS))) {
  v <- VARS[i, ]
  grDevices::png(file.path(out_fig, sprintf("%s.png", v$name)),
                 width = 5.2, height = 4.0, units = "in", res = 400,
                 type = "cairo", bg = "white")
  graphics::par(mar = c(4.2, 4.2, 2.6, 1.0), family = "serif")
  draw_one(v)
  grDevices::dev.off()
}

# one multi-panel figure per group
panel_files <- character(0)
panel_dims  <- list()
for (g in GROUPS) {
  rows <- VARS[VARS$group == g, ]
  n    <- nrow(rows)
  # At most three columns. A four-column panel has to be shrunk to about half
  # size to fit the page, which is what made the axis text unreadable; three
  # columns of 2.5 in sit on the page at close to their drawn size.
  ncol  <- if (n <= 2) n else 3
  nrow_ <- ceiling(n / ncol)
  w_in  <- 2.5 * ncol
  h_in  <- 2.25 * nrow_
  f <- file.path(out_fig, sprintf("Panel_%s.png", safe(g)))
  grDevices::png(f, width = w_in, height = h_in, units = "in",
                 res = 400, type = "cairo", bg = "white")
  graphics::par(mfrow = c(nrow_, ncol), mar = c(3.9, 3.9, 2.5, 0.9),
                mgp = c(2.3, 0.6, 0), family = "serif")
  for (i in seq_len(n)) draw_one(rows[i, ], cex_axis = 0.9, main_cex = 1.0)
  grDevices::dev.off()
  panel_files <- c(panel_files, f)
  names(panel_files)[length(panel_files)] <- g
  panel_dims[[g]] <- c(w = w_in, h = h_in)
}

# -----------------------------------------------------------------------------
# 4. Word appendix: every table and every panel figure, in order
# -----------------------------------------------------------------------------

ft <- function(df, size = 8) {
  f <- flextable::flextable(df)
  f <- flextable::fontsize(f, size = size, part = "all")
  f <- flextable::font(f, fontname = "Times New Roman", part = "all")
  f <- flextable::bold(f, part = "header")
  f <- flextable::padding(f, padding = 2, part = "all")
  f <- flextable::autofit(f)
  flextable::fit_to_width(f, max_width = 9.2)
}

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Appendix: Descriptive Statistics for Every Analysis Variable",
                             style = "heading 1")
doc <- officer::body_add_par(doc, paste(
  "Every variable that enters any model in the paper is described below on its",
  "original measurement scale. Continuous variables report the number of",
  "respondents with a valid value, the number missing, and the mean, standard",
  "deviation, minimum, quartiles and maximum. Binary and categorical variables",
  "report counts and percentages of non-missing responses. Each group of",
  "variables is followed by a panel of distribution plots: histograms for",
  "continuous variables, with the mean marked by a solid line and the median by",
  "a dashed line, and bar charts for categorical variables.",
  "Generated by 13_descriptives_all.R."), style = "Normal")

n_tab <- 0
for (g in GROUPS) {
  doc <- officer::body_add_par(doc, g, style = "heading 2")

  if (!is.null(cont_tables[[g]])) {
    n_tab <- n_tab + 1
    doc <- officer::body_add_par(doc, sprintf("Table A%d. %s: continuous variables.", n_tab, g),
                                 style = "Normal")
    doc <- flextable::body_add_flextable(doc, ft(cont_tables[[g]]))
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }
  if (!is.null(cat_tables[[g]])) {
    n_tab <- n_tab + 1
    doc <- officer::body_add_par(doc, sprintf("Table A%d. %s: categorical and binary variables.", n_tab, g),
                                 style = "Normal")
    doc <- flextable::body_add_flextable(doc, ft(cat_tables[[g]]))
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }

  f <- panel_files[[g]]
  if (!is.null(f) && file.exists(f)) {
    d    <- panel_dims[[g]]
    w_in <- 6.5
    h_in <- w_in * d[["h"]] / d[["w"]]
    if (h_in > 8.4) { w_in <- w_in * 8.4 / h_in; h_in <- 8.4 }
    doc <- officer::body_add_img(doc, src = f, width = w_in, height = h_in)
    doc <- officer::body_add_par(doc, sprintf("Figure A%d. Distributions: %s.",
                                              which(GROUPS == g), tolower(g)),
                                 style = "Normal")
  }
}

doc <- officer::body_add_par(doc, "Variable coverage", style = "heading 2")
doc <- officer::body_add_par(doc, paste(
  "Coverage differs across sources. Buffer-derived measures are available for",
  "every geocoded respondent, while three block-group sources -- Tree Equity",
  "Score, NaNDA street connectivity and the HUD Jobs Proximity Index -- cover",
  "fewer. This table is the reason model samples differ by domain."),
  style = "Normal")
doc <- flextable::body_add_flextable(doc, ft(coverage[, c("Variable", "Group", "Observed",
                                                          "Missing", "Percent_missing")], size = 7))

print(doc, target = file.path(out_desc, "Descriptive_Statistics_Appendix.docx"))

cat(sprintf("\n13_descriptives_all.R complete.\n  %d variables described\n  %d tables\n  %d figures\n  -> %s\n",
            nrow(VARS), n_tab, nrow(VARS) + length(panel_files), out_desc))
