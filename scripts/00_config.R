# =============================================================================
# 00_config.R
# Shared configuration for the Denver immigrant well-being / built environment
# analysis. EVERY other script starts by sourcing this file.
#
# Run this once at the top of a fresh R session, or let each numbered script
# source it automatically.
# =============================================================================

.wb_config_loaded <- TRUE

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------
# NO ABSOLUTE PATHS ANYWHERE IN THIS PROJECT. The data folder is resolved at
# runtime, in this order:
#
#   1. the WELLBEING_DIR environment variable, if set
#   2. ../data, data/, or . -- whichever contains swb_data.csv
#
# So a collaborator clones the repo, drops the data folder next to `scripts/`,
# opens the .Rproj (or setwd() to `scripts/`), and everything resolves.
#
# To point somewhere else without editing this file:
#   Sys.setenv(WELLBEING_DIR = "/path/to/Wellbeing"); source("00_config.R")

wb_find_data_dir <- function() {
  ev <- Sys.getenv("WELLBEING_DIR", unset = "")
  if (nzchar(ev)) {
    if (!dir.exists(ev)) {
      stop("WELLBEING_DIR is set to '", ev, "' but that directory does not exist.")
    }
    return(ev)
  }
  # swb_data.csv is the one file the whole pipeline starts from -- use it as
  # the marker for "this is the data directory".
  candidates <- c(
    file.path("..", "data"),        # scripts/ sits beside data/   (recommended)
    "data",                         # run from the project root
    "..",                           # scripts/ sits inside the data folder
    ".",                            # run from inside the data folder
    file.path("..", ".."),          # project/scripts/ unzipped inside the data folder
    file.path("..", "..", "data")
  )
  for (p in candidates) {
    if (dir.exists(p) && file.exists(file.path(p, "swb_data.csv"))) return(p)
  }
  stop(
    "Could not locate the data directory.\n",
    "Looked for swb_data.csv in: ", paste(candidates, collapse = ", "), "\n",
    "Working directory is: ", getwd(), "\n\n",
    "Fix by either:\n",
    "  (a) setwd() to the `scripts/` folder with the data in ../data, or\n",
    "  (b) Sys.setenv(WELLBEING_DIR = \"/path/to/your/data\") before sourcing.\n"
  )
}

out_dir <- normalizePath(wb_find_data_dir(), mustWork = TRUE)

map_dir       <- file.path(out_dir, "maps_outputs")
table_dir     <- file.path(out_dir, "word_ready_tables")
model_tab_dir <- file.path(out_dir, "word_ready_model_tables")

for (d in c(map_dir, table_dir, model_tab_dir)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# Projected CRS used for every buffer / distance / area calculation.
# NAD83 / UTM zone 13N -- metres. Do not change without re-checking all
# buffer radii, which are specified in metres.
target_crs <- 26913

# Buffer radii (metres). 800 m is the primary exposure; 400 m and 1600 m are
# reported as sensitivity analyses.
buffers_m         <- c(400, 800, 1600)
primary_buffer_m  <- 800

# Survey year, used to convert year of birth into age.
survey_year <- 2019

# -----------------------------------------------------------------------------
# 2. Packages
# -----------------------------------------------------------------------------
# Spatial scripts (01-06) need the geospatial stack. Analysis scripts (07-09)
# need only the modelling/table packages. Load what you need via wb_require().

wb_packages_spatial <- c(
  "tidyverse", "sf", "terra", "tigris", "tidycensus",
  "janitor", "readr", "foreign", "haven"
)

wb_packages_analysis <- c(
  "tidyverse", "janitor", "readr", "psych", "car", "broom",
  "modelsummary", "mediation", "flextable", "officer"
)

# Package versions are pinned in renv.lock. The intended workflow is:
#
#   install.packages("renv")
#   renv::restore()      # installs the exact versions used for the paper
#
# wb_require() then simply attaches them. If renv has not been restored and a
# package is genuinely absent, it stops with instructions rather than silently
# installing whatever version CRAN serves today -- which is what makes results
# drift over time.
wb_require <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing packages: ", paste(missing, collapse = ", "), "\n\n",
      "This project pins package versions with renv. To install them:\n",
      "    install.packages(\"renv\")\n",
      "    renv::restore()\n\n",
      "To install the current CRAN versions instead (results may differ from\n",
      "those reported in the paper):\n",
      "    install.packages(c(", paste0("\"", missing, "\"", collapse = ", "), "))",
      call. = FALSE
    )
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

options(tigris_use_cache = TRUE)

# tidycensus needs an API key for script 01 only. Run once, ever:
#   tidycensus::census_api_key("YOUR_KEY", install = TRUE, overwrite = TRUE)
# Then restart R. Scripts 02-09 do not touch the Census API.

# -----------------------------------------------------------------------------
# 3. Canonical file names
# -----------------------------------------------------------------------------
# The pipeline is a chain: each script reads the previous script's output.
# Names are recorded here so the chain is visible in one place.

wb_files <- list(
  survey          = "swb_data.csv",
  s01_acs         = "respondents_with_acs.csv",
  s02_enviro      = "respondents_with_acs_enviro_zoning_sld_walkability_jobs_landcover.csv",
  s03_tree_parks  = "respondents_with_all_built_environment_tree_parks.csv",
  s04_transport   = "respondents_with_full_transport_connectivity_crash_urban_density.csv",
  s05_final       = "respondents_with_transport_ht_segpoi_stoz_diversity.csv"
)

wb_path <- function(key) file.path(out_dir, wb_files[[key]])

# The file every analysis script reads.
analysis_file <- wb_path("s05_final")

# -----------------------------------------------------------------------------
# 4. Small helpers used across scripts
# -----------------------------------------------------------------------------

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

zscore <- function(x) as.numeric(scale(x))

# Census GEOIDs arrive as numbers, factors, and strings with stray decimals.
# Force to a zero-padded character key of fixed width (12 = block group,
# 11 = tract). Every spatial join in this project keys on the result.
clean_geoid <- function(x, width = 12) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\.0$", "")
  x <- stringr::str_replace_all(x, "[^0-9]", "")
  stringr::str_pad(x, width = width, side = "left", pad = "0")
}

prefix_except <- function(df, prefix, keep = "geoid") {
  names(df) <- ifelse(names(df) %in% keep, names(df), paste0(prefix, names(df)))
  df
}

buffer_area_km2 <- function(buffer_m) pi * buffer_m^2 / 1e6

# Fit lm(), failing loudly if any requested predictor is missing or constant.
#
# A model in the manuscript specifies an exact set of predictors. If one of them
# vanishes -- because an upstream join silently failed, or a column was renamed
# -- the correct behaviour for archival code is to stop, not to quietly fit a
# smaller model that still looks plausible. `allow_drop = TRUE` restores the
# permissive behaviour, and is used only where a specification is deliberately
# probing which variables exist (never for a reported model).
make_lm <- function(outcome, rhs, data, allow_drop = FALSE) {

  requested <- rhs
  absent    <- setdiff(requested, names(data))
  present   <- setdiff(requested, absent)
  constant  <- present[!vapply(data[present],
                               function(x) dplyr::n_distinct(x, na.rm = TRUE) > 1,
                               logical(1))]

  if (length(c(absent, constant)) && !allow_drop) {
    stop(
      "make_lm(", outcome, "): refusing to fit a model that is missing predictors.\n",
      if (length(absent))
        paste0("  Not present in the data: ", paste(absent, collapse = ", "), "\n") else "",
      if (length(constant))
        paste0("  Present but constant:    ", paste(constant, collapse = ", "), "\n") else "",
      "  This usually means an upstream join failed. Check the script that\n",
      "  produces the input file before re-running. To fit anyway (not for a\n",
      "  reported model), pass allow_drop = TRUE.",
      call. = FALSE
    )
  }

  if (length(c(absent, constant))) {
    message("  make_lm(", outcome, "): dropped ",
            paste(c(absent, constant), collapse = ", "), " (allow_drop = TRUE)")
  }

  keep <- setdiff(present, constant)
  if (!length(keep)) stop("make_lm(", outcome, "): no usable predictors.", call. = FALSE)

  stats::lm(stats::as.formula(paste(outcome, "~", paste(keep, collapse = " + "))),
            data = data)
}

# -----------------------------------------------------------------------------
# 5. Reproducibility
# -----------------------------------------------------------------------------

# Every stochastic step in this project -- the bootstrap resampling in
# 09_mediation.R -- draws from this seed, so the reported confidence intervals
# are exactly reproducible.
WB_SEED <- 20260622
set.seed(WB_SEED)

# Record the exact R and package versions that produced a run.
wb_write_session_info <- function(file = file.path(out_dir, "sessionInfo.txt")) {
  writeLines(
    c(paste("Run completed:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
      paste("Seed:", WB_SEED), "",
      utils::capture.output(utils::sessionInfo())),
    file
  )
  cat("Wrote", file, "\n")
}

# Fail early if a required raw input is missing, rather than producing a
# silently empty join part-way through a long spatial script.
wb_check_inputs <- function(...) {
  paths <- c(...)
  missing <- paths[!file.exists(file.path(out_dir, paths))]
  if (length(missing)) {
    stop("Missing required input(s) in ", out_dir, ":\n  - ",
         paste(missing, collapse = "\n  - "),
         "\nSee DATA_MANIFEST.md for where each file comes from.",
         call. = FALSE)
  }
  invisible(TRUE)
}

cat("Config loaded. out_dir =", out_dir, "\n")
