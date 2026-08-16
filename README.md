# Neighborhood Built Environment, Belonging, and Subjective Well-Being Among First-Generation Immigrants in Metro Denver

Reproduction code for the Data & Methods and Results sections of the paper.

This replaces `Wellbeing.Rmd`. The notebook has been split into numbered R
scripts that run in dependency order, read their inputs from disk, and write
named outputs. No absolute paths, no hidden session state, no manual chunk
ordering.

---

## Quick start

```bash
git clone <this repo>
cd <repo>
# put the data folder next to scripts/ (see DATA_MANIFEST.md)
cd scripts
Rscript run_all.R --analysis     # models + tables, ~5 min
Rscript run_all.R                # full pipeline incl. spatial joins, 1-3 hrs
```

Expected layout:

```
project/
├── scripts/          <- this folder
│   ├── 00_config.R
│   ├── 01_geocode_acs.R
│   ├── ...
│   └── run_all.R
└── data/             <- all raw inputs AND all outputs
    ├── swb_data.csv
    ├── Zoning/
    ├── DRCOG_2020_Landcover/
    └── ...
```

If your data lives elsewhere:

```r
Sys.setenv(WELLBEING_DIR = "/path/to/Wellbeing")
source("00_config.R")
```

`00_config.R` finds the data folder by looking for `swb_data.csv`. It stops
with an explicit error rather than silently producing empty joins.

---

## The pipeline

Each script reads the previous script's output. Run them in order.

| # | Script | Does | Reads | Writes |
|---|--------|------|-------|--------|
| 00 | `00_config.R` | Paths, packages, seed, shared helpers | — | — |
| — | `wb_model_tables.R` | Builds regression tables as `.docx`/`.csv` via officer, no pandoc. Sourced by 08. | — | — |
| 01 | `01_geocode_acs.R` | Geocode respondents, attach tract + block group, merge 2015–2019 ACS | `swb_data.csv` | `respondents_with_acs.csv` |
| 02 | `02_enviro_zoning_walkability_landcover.R` | EnviroScreen, **zoning overlay**, EPA Smart Location DB, National Walkability Index, HUD Jobs Proximity, DRCOG land cover in 400/800/1600 m buffers | 01 output | `..._enviro_zoning_sld_walkability_jobs_landcover.csv` |
| 03 | `03_tree_parks.R` | Tree Equity Score, park distance and acreage | 02 output | `..._all_built_environment_tree_parks.csv` |
| 04 | `04_transport_connectivity_crash.R` | NaNDA street connectivity, sidewalk/bike/active-corridor density, **pedestrian focus areas**, crash densities, urban centers | 03 output | `..._full_transport_connectivity_crash_urban_density.csv` |
| 05 | `05_ht_poi_stoz_diversity.R` | H+T Affordability Index, POI segregation/travel data, short-trip opportunity zones, diversity & exposure | 04 output | `..._transport_ht_segpoi_stoz_diversity.csv` **(analysis file)** |
| 06 | `06_maps_descriptives.R` | Respondent maps, block group / tract summaries | 05 output | `maps_outputs/`, `word_ready_tables/` |
| 07 | `07_tables_descriptive.R` | Paper Tables 1 & 2 | 05 output | `word_ready_model_tables/` |
| 08 | `08_domain_models.R` | Alphas, base/context models, five domain models, final model, **VIF diagnostics** | 05 output | `all_model_coefficients.csv`, `model_fit_summary.csv`, `vif_diagnostics.csv`, `zoning_joint_tests.csv`, `domain_models_*.csv/.docx/.html`, `model_objects.rds` |
| 09 | `09_mediation.R` | Formal causal mediation, Table 8 | 08 output | `Table8_Formal_Mediation.*` |
| 10 | `10_greenness_sensitivity.R` | Refits the greenness domain four ways to test the Tree Equity collinearity | 08 output | `greenness_sensitivity.csv/.docx` |
| 99 | `99_legacy_perceived_be_models.R` | **ARCHIVED — do not cite.** Models of *perceived* built environment from survey items. Superseded by 08. | `swb_data.csv` | `model1-3_*.csv`, `sem_mediation_results.csv` |

`08_models_mediation_ORIGINAL.R` is the unedited original chunk, kept only so
you can diff it against `08_domain_models.R` + `09_mediation.R`.

---

## Which script produces which part of the paper

| Paper section | Script |
|---|---|
| §2.1 sample, geocoding | 01 |
| §2.2–2.3 SWB and belonging scales, Cronbach's α | 08 |
| §2.4 built environment measures | 02, 03, 04, 05 |
| §3.1 Tables 1–2 descriptives | 07 |
| §3.2 Table 3 individual + context | 08 |
| §3.3–3.5 Tables 4–6 domain models | 08 |
| §3.6 Table 7 final model | 08 |
| §3.7 Table 8 mediation | 09 |

---

## What changed from `Wellbeing.Rmd`

Substantive changes, all of which affect what the paper can claim:

1. **Zoning is now modelled.** Script 02 always built `zone_gen_zone2`,
   `zone_adu`, and `zone_jurisd` from the Denver MSA zoning shapefile, and
   script 04 always built pedestrian focus area shares — then no model used
   any of them, even though Methods §2.4 says respondents were "linked to
   zoning classifications, pedestrian focus areas." `08_domain_models.R` adds a
   fifth `land_use` domain so the paper reports the result instead of omitting
   it. (The result is a null; see `AUDIT.md`.)

2. **Zoning is tested jointly.** A four-level factor spread across three dummy
   coefficients needs an F test, not three t tests. `zoning_joint_tests.csv`.

3. **VIFs are actually computed.** Methods claims VIFs were examined and
   multicollinearity was not a concern. Nothing in the original pipeline
   called `car::vif()`. It is now computed for every model and written to
   `vif_diagnostics.csv`. Two terms exceed the conventional threshold of 10 —
   see `AUDIT.md`.

4. **The bootstrap is seeded.** `09_mediation.R` sets `WB_SEED` before
   resampling. The original had no seed, so the published confidence intervals
   could not be reproduced exactly, even by the author.

5. **Multiplicity is reported.** Ten indirect effects are tested; Holm-adjusted
   p-values sit beside the raw ones.

6. **Domain model tables are exported.** The original fit the models and only
   wrote out the mediation table, so Tables 3–7 had to be read off the console.

Reproducibility changes, which do not affect results:

7. **No absolute paths.** `/Users/priyanka/Wellbeing` appeared in nine places.
8. **No file globbing for data sources.** Crash data and the HUD Jobs Proximity
   Index were selected with `list.files(pattern = ...)[1]`, which depends on
   filesystem ordering — `crash_2019.csv` and `crash_2019/drcog_crash_2019.shp`
   both matched. Both are now pinned by name.
9. **No cross-script session state.** Scripts 03–05 opened with
   `if (!exists("dat"))`, silently reusing whatever was in memory. `run_all.R`
   gives each script a fresh environment.
10. **`sessionInfo.txt`** is written after every full run.
11. **Outputs are saved before formatting.** `08_domain_models.R` writes
    `model_objects.rds`, `all_model_coefficients.csv`, and
    `model_fit_summary.csv` *before* attempting any table export, and each
    export is individually wrapped. A failed table export no longer takes down
    the script or strands `09_mediation.R`.
12. **No external binaries.** Word output no longer depends on pandoc being
    installed and visible to a terminal R session.

---

## Requirements

R ≥ 4.2. Packages install automatically on first run via `wb_require()`.

Analysis stage (07–09):
`tidyverse`, `janitor`, `readr`, `psych`, `car`, `broom`, `modelsummary`,
`mediation`, `flextable`, `officer`

Spatial stage (01–06) additionally:
`sf`, `terra`, `tigris`, `tidycensus`, `foreign`, `haven`

**Script 01 needs a Census API key** (free, instant):

```r
tidycensus::census_api_key("YOUR_KEY", install = TRUE)
```

Then restart R. Scripts 02–09 do not touch the Census API, so if you already
have `respondents_with_acs.csv` you can skip 01 entirely.

**No pandoc required.** Word tables are written with `officer`/`flextable`,
which generate the document XML directly from R. (`modelsummary`'s own `.docx`
path shells out to pandoc, an external binary that R launched from a terminal
does not inherit from RStudio — so `wb_model_tables.R` reimplements the table
layout without it. `.html` versions still come from `modelsummary`, which needs
no external tool.)

**Script 02 is the slow one.** It extracts land cover from a 6.6 GB raster for
three buffer radii around each respondent. Expect 1–2 hours and ~8 GB of free
disk.

---

## Known issues, ranked

Full detail in `AUDIT.md`. The three that affect published claims:

1. **RESOLVED — `tree_tes` and `tree_treecanopy` are collinear** (r = .89, VIFs
   13.7 and 10.4) and enter the greenness model together with opposite signs.
   `10_greenness_sensitivity.R` refits the domain with each measure alone: the
   "Tree Equity Score → belonging, β = 0.468, p = .028" result **collapses to
   β = 0.087, p = .404** once its twin is removed — a fivefold reduction — and
   the raw bivariate correlation with belonging is slightly *negative*. It was
   a collinearity artifact and should not be reported. The independently
   measured land-cover canopy share is stable across all four specifications
   (β = 0.187–0.239) and is the defensible greenness finding.
2. **Domain models run on different samples** (n = 238 for urban form,
   transport, and greenness; n = 304 for safety) because Tree Equity, NaNDA,
   and HUD Jobs coverage stops at 278 of 358 geocoded respondents. Coefficients
   are compared across domains in the text as though the samples matched.
3. **`segpoi_*_distance` is not proximity.** Methods describes these as
   "proximity to grocery stores, schools, healthcare facilities, parks." The
   source file pairs every `*_distance` with a `*_segregation` column — these
   are average travel distances from an experienced-segregation dataset, not
   home-to-nearest-destination distances. Confirm against the source codebook
   before describing them either way.
