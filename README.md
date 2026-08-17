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
| 11 | `11_manuscript_tables.R` | Assembles the ten model tables and writes the map from these numbers to the manuscript's | 08, 09 output | `manuscript_tables/Table01–10.csv`, `Manuscript_Tables.docx`, `table_numbering.csv` |
| 12 | `12_figure_mediation_dag.R` | Figure 1, the mediation path diagram | — | `figures/Figure1_Mediation_DAG.png/.pdf` |
| 13 | `13_descriptives_all.R` | A descriptive table and a distribution figure for every variable used in any model, plus a Word appendix | 08 output | `descriptives/Table_A*.csv`, `descriptives/figures/`, `Descriptive_Statistics_Appendix.docx` |
| 14 | `14_figure_coefficients.R` | Figure 2, every adjusted association on one pair of axes | 08 output | `figures/Figure2_Coefficients.png/.pdf/.csv` |

`wb_model_tables.R` is a helper sourced by 08; it builds the Word regression
tables directly through `officer`, so no external pandoc installation is needed.
`wb_labels.R` holds the printed label for every model term, and is sourced by
11, 13 and 14 so a variable is never called two different things in two
different tables.

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
| §3.8 mediation | 09 |
| §3.4 greenness sensitivity analysis | 10 |
| all model tables, and the map to the manuscript's numbering | 11 |
| Figure 1, the mediation diagram | 12 |
| SI Section S2, descriptives for every variable | 13 |
| Figure 2, the coefficient plot | 14 |

**Where each table and figure ends up.** The manuscript keeps five items in the
main text and moves the rest to the Supplementary Information:

| Manuscript | Content | Script |
|---|---|---|
| Table 1 | Sample characteristics and outcome variables | 11 |
| Figure 1 | Mediation model estimated for each exposure | 12 |
| Figure 2 | All adjusted associations, both outcomes | 14 |
| Table 2 | Final parsimonious models | 11 |
| Table 3 | Formal causal mediation analyses | 09, 11 |
| Tables S1–S3, S5–S7 | Full model output | 11 |
| Table S4 | Greenness specification sensitivity | 10, 11 |
| Tables S8–S19, Figures S1–S9 | Descriptives for every analysis variable | 13 |

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
r = .89, with a maximum variance inflation factor of 13.4.
`10_greenness_sensitivity.R` refits the greenness domain with each measure
entered alone; the independently measured DRCOG land-cover canopy share is used
as the primary greenness exposure. See §3.4 of the paper.

**Demographic controls.** The categorical demographic variables are collapsed
into binary indicators computed once on the full sample rather than entered as
factors. Several response categories are very sparse (race includes cells of 2,
4, and 5; immigration status includes cells of 5 and 10), which both consumes
degrees of freedom on analytic samples of 238–320 and makes bootstrap resampling
unstable. Codes come from the survey codebook: Hispanic is coded Yes = 0,
marital status is single = 0 / married = 1 / separated, divorced, widowed = 2–4,
and immigration status runs naturalized citizen = 0 through work visa = 7.
Reference categories are never married and naturalized U.S. citizen. See
`demographic_labels` in `08_domain_models.R`.

**Mediation covariate set.** The mediation models use the same covariate set and
the same analytic samples as the domain models, so Table 9 is directly
comparable to Tables 4–8.

**Model specifications are strict.** `make_lm()` stops if a requested predictor
is missing or constant, rather than fitting a reduced model, so an upstream join
failure surfaces immediately instead of silently changing a reported
specification.

**Units.** NaNDA street connectivity measures are per square mile, not per
square kilometre: `intdensity` is defined as `n_realnodes / tract_area_sqmiles`
(ICPSR 38580, DS0003). Buffer-derived densities computed in scripts 03-05 are
per square kilometre.

**Random seed.** All bootstrap procedures draw from `WB_SEED` in `00_config.R`,
so the mediation confidence intervals are exactly reproducible.

---

## Requirements

R ≥ 4.2 (developed and run under 4.5.0). Package versions are pinned in
`renv.lock`; `renv::restore()` installs them. `sessionInfo.txt` is written to the
data folder after each full run.

Analysis stage (07–14): `tidyverse`, `janitor`, `readr`, `psych`, `car`,
`broom`, `modelsummary`, `mediation`, `flextable`, `officer`. Figures use base
graphics only, so nothing beyond a working `cairo` PNG device is needed.

Spatial stage (01–06) additionally: `sf`, `terra`, `tigris`, `tidycensus`,
`foreign`, `haven`

**Script 01 requires a Census API key** (free, from
<https://api.census.gov/data/key_signup.html>):

```r
tidycensus::census_api_key("YOUR_KEY", install = TRUE)
```

Restart R afterwards. Scripts 02–14 do not use the Census API, so an existing
`respondents_with_acs.csv` lets you skip script 01.

**Script 02 is the long-running step.** It extracts land cover from a 6.6 GB
raster for three buffer radii around each respondent — roughly 45 minutes, and
about 8 GB of free disk.

---

## Citation

If you use this code, please cite the paper. Code is released under the MIT
License (see [`LICENSE`](LICENSE)).
