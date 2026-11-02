# =============================================================================
# v3_prepare.R
# Build the v3 analysis frame: outcomes, individual controls, neighborhood
# demographic controls and the four NBE domains, standardized, on one common
# sample. Sourced by 31_parallel_models.R and 32_robustness_v3.R.
#
# Returns (into the calling environment):
#   v3_dat         the analysis frame, one row per respondent in the common sample
#   v3_full        the full frame before the common-sample filter (for counts)
#   v3_spec        list: controls_ind, controls_nbhd, domains (z names), all_nbe
#   v3_alpha       Cronbach's alpha for the two scales
# =============================================================================

if (!exists(".wb_config_loaded")) source("00_config.R")
source("wb_domains_v3.R")
wb_require(c("dplyr", "readr", "psych"))

in_file <- file.path(out_dir, "respondents_v3_analysis.csv")
if (!file.exists(in_file)) stop("v3_prepare.R: run 30_rail_stations.R first (", in_file, ")")
raw <- readr::read_csv(in_file, show_col_types = FALSE, guess_max = 5000)

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
zscore <- function(x) as.numeric(scale(x))

# ---- 1. Survey items -> readable names (same map as 08_domain_models.R) ------
rename_map <- c(
  year_born = "q3_1", gender = "q3_2", marital = "x2", children = "q3_4",
  hispanic = "q3_5", race = "q3_6", imm_status = "q3_7", income_hh = "q3_10",
  engl_speak = "q3_11_1", edu_level = "q3_14",
  time_live_denver = "q5_9", time_live_hood = "q5_10",
  sat_stand_liv = "q8_1_1", sat_health = "q8_1_2", sat_life_achieve = "q8_1_3",
  feel_safe = "q8_1_4", feel_accepted = "q8_1_5", feel_conf_finance = "q8_1_6",
  hood_belong = "q9_7_1", hood_trust_people = "q9_7_2",
  hood_borrow = "q9_7_3", hood_contacts = "q9_7_4"
)
for (nm in names(rename_map)) {
  old <- rename_map[[nm]]
  if (old %in% names(raw) && !nm %in% names(raw)) names(raw)[names(raw) == old] <- nm
}

# ---- 2. Outcomes -------------------------------------------------------------
swb_items <- c("sat_stand_liv", "sat_health", "sat_life_achieve",
               "feel_safe", "feel_accepted", "feel_conf_finance")
nsc_items <- c("hood_belong", "hood_trust_people", "hood_borrow", "hood_contacts")
dat <- raw %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(c(swb_items, nsc_items)), to_num)) %>%
  dplyr::mutate(
    swb_index  = rowMeans(dplyr::across(dplyr::all_of(swb_items)), na.rm = TRUE),
    nsc_index  = rowMeans(dplyr::across(dplyr::all_of(nsc_items)), na.rm = TRUE),
    swb_index  = ifelse(is.nan(swb_index), NA_real_, swb_index),
    nsc_index  = ifelse(is.nan(nsc_index), NA_real_, nsc_index),
    belong1    = hood_belong,
    age        = survey_year - to_num(year_born)
  )
v3_alpha <- c(
  swb = unname(psych::alpha(dat[, swb_items], warnings = FALSE)$total$raw_alpha),
  nsc = unname(psych::alpha(dat[, nsc_items], warnings = FALSE)$total$raw_alpha)
)

# ---- 3. Individual controls (indicators, as in 08) ---------------------------
dat <- dat %>%
  dplyr::mutate(
    ind_female       = as.numeric(to_num(gender)     == 1),
    ind_married      = as.numeric(to_num(marital)    == 1),
    ind_prev_married = ifelse(is.na(to_num(marital)), NA_real_,
                              as.numeric(to_num(marital) %in% c(2, 3, 4))),
    ind_hispanic     = as.numeric(to_num(hispanic)   == 0),
    ind_race_white   = as.numeric(to_num(race)       == 4),
    ind_lpr          = as.numeric(to_num(imm_status) == 1),
    ind_undocumented = as.numeric(to_num(imm_status) == 2),
    ind_imm_other    = ifelse(is.na(to_num(imm_status)), NA_real_,
                              as.numeric(to_num(imm_status) %in% c(3, 4, 5, 6, 7)))
  )
controls_ind <- c("age", "children", "income_hh", "edu_level", "engl_speak",
                  "time_live_denver", "time_live_hood",
                  "ind_female", "ind_married", "ind_prev_married", "ind_hispanic",
                  "ind_race_white", "ind_lpr", "ind_undocumented", "ind_imm_other")

# ---- 4. Neighborhood variables: registry check, numeric, standardized --------
variant_vars <- c("urban_center_nearest_dist_m", "div_total_diversity_resi",
                  "rail_station_nearest_km")
buffer_alt <- unlist(lapply(c(400, 1600), function(r) sprintf(V3_BUFFER_VARS, r)))
registered <- unique(c(V3_CONTROL_VARS, unlist(V3_DOMAIN_VARS), variant_vars, buffer_alt))
absent <- setdiff(registered, names(dat))
if (length(absent)) stop("v3_prepare.R: predictors missing from the analysis file: ",
                         paste(absent, collapse = ", "), call. = FALSE)

numeric_vars <- setdiff(registered, V3_BINARY)
dat <- dat %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(c(controls_ind, registered)), to_num))
zn <- function(v) ifelse(v %in% V3_BINARY, v, paste0(v, "_z"))

v3_spec <- list(
  controls_ind  = controls_ind,
  controls_nbhd = zn(V3_CONTROL_VARS),
  domains       = lapply(V3_DOMAIN_VARS, zn),
  variant       = list(urban_center = zn("urban_center_nearest_dist_m"),
                       resdiv       = zn("div_total_diversity_resi"),
                       rail_dist    = zn("rail_station_nearest_km"))
)
v3_spec$all_nbe <- unlist(v3_spec$domains, use.names = FALSE)

# ---- 5. Common sample, then standardization ----------------------------------
# One sample for every model: respondents with both outcomes, every control and
# every NBE measure of the main specification. The variants add one variable
# each and are estimated on the subset complete on it as well; 31 reports both.
# Standardization happens AFTER the sample is fixed, so that "one standard
# deviation" in every coefficient means one SD among the respondents in the
# models, and the 400 m / 1,600 m versions used by 32 are on the same footing
# as the 800 m versions. (The *_z columns that 20_new_variables.R wrote for a
# few variables are overwritten here on purpose.)
core_raw <- c("swb_index", "nsc_index", controls_ind, V3_CONTROL_VARS, unlist(V3_DOMAIN_VARS))
v3_full <- dat
v3_dat  <- dat[stats::complete.cases(dat[, core_raw]), ]
v3_dat  <- v3_dat %>%
  dplyr::mutate(
    swb_z     = zscore(swb_index),
    nsc_z     = zscore(nsc_index),
    belong1_z = zscore(belong1),
    dplyr::across(dplyr::all_of(numeric_vars), zscore, .names = "{.col}_z")
  )
core <- c("swb_z", "nsc_z", controls_ind, v3_spec$controls_nbhd, v3_spec$all_nbe)

# constant predictors would make a specification silently different from the
# one written down
const <- names(v3_dat[, core])[vapply(v3_dat[, core], function(x) length(unique(x)) < 2, logical(1))]
if (length(const)) stop("v3_prepare.R: constant in the common sample: ", paste(const, collapse = ", "))

v3_counts <- c(
  surveyed       = nrow(dat),
  geocoded       = sum(!is.na(dat$latitude)),
  both_outcomes  = sum(!is.na(dat$swb_index) & !is.na(dat$nsc_index)),
  geo_both       = sum(!is.na(dat$latitude) & !is.na(dat$swb_index) & !is.na(dat$nsc_index)),
  geo_both_env   = sum(!is.na(dat$latitude) & !is.na(dat$swb_index) & !is.na(dat$nsc_index) & !is.na(dat$env_pm25)),
  analytic       = nrow(v3_dat)
)
cat(sprintf("v3 analysis frame: %d respondents (of %d surveyed; %d geocoded; %d geocoded with both outcomes; %d of those inside EnviroScreen coverage; %d complete on every covariate)\n",
            v3_counts[["analytic"]], v3_counts[["surveyed"]], v3_counts[["geocoded"]], v3_counts[["geo_both"]],
            v3_counts[["geo_both_env"]], v3_counts[["analytic"]]))
cat(sprintf("Cronbach's alpha: SWB %.3f (6 items), NSC %.3f (4 items)\n", v3_alpha["swb"], v3_alpha["nsc"]))
