# Neighborhood Built Environment, Belonging, and Subjective Well-Being Among First-Generation Immigrants in Metro Denver

Replication code for the Data & Methods and Results sections of the paper.

The analysis is organized into standalone R scripts corresponding to the stages
of data preparation and analysis. Each script reads its input from disk and
writes named outputs, so the pipeline can be run end to end or resumed at any
stage.

**No data is included in this repository.** The respondent survey contains
approximate residential locations and is not publicly redistributable; the
third-party spatial datasets are large and separately licensed. See
[`DATA_MANIFEST.md`](DATA_MANIFEST.md) for every input and where to obtain it.

---

## Quick start

```bash
git clone https://github.com/pdez90/Wellbeing_Denver_immigrants.git
cd Wellbeing_Denver_immigrants

# restore the exact package versions used for the paper
Rscript -e 'install.packages("renv"); renv::restore()'

# place the data folder (see DATA_MANIFEST.md) next to scripts/, then:
cd scripts
Rscript run_all.R --analysis     # models and tables, ~5 min
Rscript run_all.R                # full pipeline including spatial joins
```

Expected layout:

```
Wellbeing_Denver_immigrants/
├── renv.lock
├── scripts/
│   ├── 00_config.R
│   ├── 01_geocode_acs.R
│   ├── ...
│   └── run_all.R
└── data/             <- all raw inputs AND all outputs (not in this repo)
    ├── swb_data.csv
    ├── Zoning/
    ├── DRCOG_2020_Landcover/
    └── ...
```

`00_config.R` locates the data folder by looking for `swb_data.csv`, checking
`../data`, `data/`, `..`, and `.` in that order. To point elsewhere:

```r
Sys.setenv(WELLBEING_DIR = "/path/to/data")
```

It stops with an explicit error rather than proceeding with empty joins.

---

## The pipeline

Each script reads the previous script's output. Run them in order.

| # | Script | Stage | Reads | Writes |
|---|--------|-------|-------|--------|
| 00 | `00_config.R` | Paths, packages, seed, shared helpers | — | — |
| 01 | `01_geocode_acs.R` | Geocode respondents, attach tract and block group, merge 2015–2019 ACS | `swb_data.csv` | `respondents_with_acs.csv` |
| 02 | `02_enviro_zoning_walkability_landcover.R` | EnviroScreen, zoning overlay, EPA Smart Location Database, National Walkability Index, HUD Jobs Proximity, DRCOG land cover in 400/800/1600 m buffers | 01 output | `..._enviro_zoning_sld_walkability_jobs_landcover.csv` |
| 03 | `03_tree_parks.R` | Tree Equity Score, park distance and acreage | 02 output | `..._all_built_environment_tree_parks.csv` |
| 04 | `04_transport_connectivity_crash.R` | NaNDA street connectivity, sidewalk / bicycle / active-corridor density, pedestrian focus areas, crash densities, urban centers | 03 output | `..._full_transport_connectivity_crash_urban_density.csv` |
| 05 | `05_ht_poi_stoz_diversity.R` | H+T Affordability Index, point-of-interest measures, short-trip opportunity zones, diversity and exposure | 04 output | `..._transport_ht_segpoi_stoz_diversity.csv` **(analysis file)** |
| 06 | `06_maps_descriptives.R` | Respondent maps, block group and tract summaries | 05 output | `maps_outputs/`, `word_ready_tables/` |
| 07 | `07_tables_descriptive.R` | Tables 1 and 2 | 05 output | `word_ready_model_tables/` |
| 08 | `08_domain_models.R` | Scale reliability, base and context models, five domain models, final model, VIF diagnostics | 05 output | `all_model_coefficients.csv`, `model_fit_summary.csv`, `vif_diagnostics.csv`, `zoning_joint_tests.csv`, `domain_models_*.csv/.docx/.html`, `model_objects.rds` |
| 09 | `09_mediation.R` | Formal causal mediation | 08 output | `formal_mediation_results_domain_models.csv`, `Table8_Formal_Mediation.csv/.docx` |
| 10 | `10_greenness_sensitivity.R` | Greenness specification sensitivity analysis | 08 output | `greenness_sensitivity.csv/.docx` |

`wb_model_tables.R` is a helper sourced by 08; it builds the Word regression
tables directly through `officer`, so no external pandoc installation is needed.

---

## Which script produces which part of the paper

| Paper section | Script |
|---|---|
| §2.1 sample and geocoding | 01 |
| §2.2–2.3 SWB and belonging scales, Cronbach's α | 08 |
| §2.4 built environment measures | 02, 03, 04, 05 |
| §3.1 Tables 1–2, descriptive statistics | 07 |
| §3.2 Table 3, individual characteristics and neighborhood context | 08 |
| §3.3–3.5 Tables 4–6, domain models | 08 |
| §3.6–3.7 Tables 7–8, belonging and final models | 08 |
| §3.8 Table 9, mediation | 09 |
| §3.4 greenness sensitivity analysis | 10 |

---

## Analytic notes

**Buffers.** Measures describing features around each residence are computed in
circular buffers of 400 m, 800 m, and 1,600 m. The 800 m buffer, roughly a
ten-minute walk, is the primary exposure; the other two support sensitivity
analyses. All buffer and distance computations use NAD83 / UTM zone 13N
(EPSG:26913), in metres.

**Analytic samples differ by domain.** Buffer-derived measures are available for
all 358 geocoded respondents, while three block-group sources — Tree Equity
Score, NaNDA street connectivity, and the HUD Jobs Proximity Index — are
available for 278. After listwise deletion, model samples range from 238 to 320.
Sample sizes are reported in every table.

**Greenness measures are collinear by construction.** The Tree Equity Score is
derived from tree canopy cover, so `tree_tes` and `tree_treecanopy` correlate at
r = .89, with variance inflation factors of 13.7 and 10.4.
`10_greenness_sensitivity.R` refits the greenness domain with each measure
entered alone; the independently measured DRCOG land-cover canopy share is used
as the primary greenness exposure. See §3.4 of the paper.

**Mediation covariate set.** Mediation models adjust for age, number of
children, household income, education, English proficiency, years in the Denver
metro area, years in the current neighborhood, and neighborhood socioeconomic
context. They do not include the categorical demographic controls used in the
domain models, because bootstrap resampling can produce factor levels with no
observations. Mediation estimates therefore rest on different samples
(n = 249–319) and a different adjustment set than the domain models, and are not
directly comparable to them. This is stated in §2.6 and in the Table 9 note.

**Model specifications are strict.** `make_lm()` stops if a requested predictor
is missing or constant, rather than fitting a reduced model, so an upstream join
failure surfaces immediately instead of silently changing a reported
specification.

**Random seed.** All bootstrap procedures draw from `WB_SEED` in `00_config.R`,
so the mediation confidence intervals are exactly reproducible.

---

## Requirements

R ≥ 4.2 (developed and run under 4.5.0). Package versions are pinned in
`renv.lock`; `renv::restore()` installs them. `sessionInfo.txt` is written to the
data folder after each full run.

Analysis stage (07–10): `tidyverse`, `janitor`, `readr`, `psych`, `car`,
`broom`, `modelsummary`, `mediation`, `flextable`, `officer`

Spatial stage (01–06) additionally: `sf`, `terra`, `tigris`, `tidycensus`,
`foreign`, `haven`

**Script 01 requires a Census API key** (free, from
<https://api.census.gov/data/key_signup.html>):

```r
tidycensus::census_api_key("YOUR_KEY", install = TRUE)
```

Restart R afterwards. Scripts 02–10 do not use the Census API, so an existing
`respondents_with_acs.csv` lets you skip script 01.

**Script 02 is the long-running step.** It extracts land cover from a 6.6 GB
raster for three buffer radii around each respondent — roughly 45 minutes, and
about 8 GB of free disk.

---

## Citation

If you use this code, please cite the paper. Code is released under the MIT
License (see [`LICENSE`](LICENSE)).
