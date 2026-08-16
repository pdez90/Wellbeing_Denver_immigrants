# Data manifest

Every raw input the pipeline reads, which script reads it, and where it came
from. Drop all of these in the `data/` folder (or whatever `WELLBEING_DIR`
points at), preserving the subfolder names in the Path column — the scripts
build paths from those names.

Total on-disk size is roughly 9 GB, dominated by the DRCOG land-cover raster.

## Survey

| Path | Used by | Source |
|---|---|---|
| `swb_data.csv` | 01, 99 | 2019 survey of foreign-born adults, Denver metro. **Not publicly redistributable — contains approximate residential coordinates.** Restricted access. |

## Census / ACS

| Path | Used by | Source |
|---|---|---|
| *(API)* | 01 | ACS 2015–2019 five-year block group and tract estimates via `tidycensus`. Needs a free Census API key. |
| *(API)* | 01, 06 | Tract and block group boundaries via `tigris`. Cached locally. |

## Built environment and land use

| Path | Used by | Source |
|---|---|---|
| `Zoning/ALL_DenverMSA_10.3.23.shp` | 02 | Denver MSA harmonized zoning, Oct 2023 vintage. Provides `zone_gen_zone2`, `zone_adu`, `zone_jurisd`. |
| `SmartLocationDatabaseV3/SmartLocationDatabase.gdb` | 02 | EPA Smart Location Database v3. |
| `WalkabilityIndex/Natl_WI.gdb` | 02 | EPA National Walkability Index (June 2021). |
| `Jobs_Proximity_Index_*.csv` | 02 | HUD Jobs Proximity Index. Pinned by filename prefix. |
| `Enviroscreen.csv` | 02 | Colorado EnviroScreen v2, block group. |
| `DRCOG_2020_Landcover/LULC_2020_DRCOG.tif` | 02 | DRCOG 2020 land cover raster, 6.6 GB. **The slow step.** |
| `co_shp/co_tes.shp` | 03 | Tree Equity Score, Colorado (American Forests). |
| `pros_2024/pros_2024.shp` | 03 | DRCOG Parks and Open Space 2024. |
| `ICPSR_38580/DS0003/38580-0003-Data.rda` | 04 | NaNDA street connectivity by tract, 2020. ICPSR study 38580. |
| `planimetrics_2022_centerline_sidewalks_total/*.shp` | 04 | DRCOG 2022 sidewalk centerlines. |
| `bicycle_facility_inventory/*.shp` | 04 | DRCOG bicycle facility inventory. |
| `active_transportation_corridors/*.shp` | 04 | DRCOG active transportation corridors. |
| `pedestrian_focus_areas/*.shp` | 04 | DRCOG pedestrian focus areas. |
| `urban_centers_2019/*.shp` | 04 | DRCOG urban centers, 2019. |
| `crash_2019.csv` | 04 | DRCOG regional crash records, 2019. A shapefile version exists at `crash_2019/drcog_crash_2019.shp`; the CSV is the one used. |
| `short_trip_opportunity_zones/*.shp` | 05 | DRCOG short-trip opportunity zones. |
| `htaindex2019_data_blkgrps_08.csv` | 05 | CNT Housing + Transportation Affordability Index, 2019, Colorado block groups. |
| `seg_poi.csv` | 05 | Experienced segregation / POI travel dataset, 94 MB. See caution below. |
| `diversity_CO.csv` | 05 | Residential and experienced diversity, Colorado. |

## Caution: `seg_poi.csv`

The column layout is `cbgid`, `All_segregation`, `All_distance`, `race`,
`median_household_income`, `classification`, `residential_segregation`, then a
`*_segregation` / `*_distance` pair for each of 16 POI categories (Culture,
Entertainment, Grocery, Healthcare, Hospital, Hotel, LifeService, market, park,
personalcare, religious, restaurant, school, socialassistance, sport,
storeshopping).

The `*_distance` columns average 39–85 with a maximum of 461. That is not a
home-to-nearest-destination distance, and the pairing with `*_segregation`
indicates these are **average travel distances of visits** to that POI
category by residents of the block group — a mobility measure derived from
mobile-device traces, not a built-environment proximity measure.

Methods §2.4 currently describes them as "proximity to grocery stores, schools,
healthcare facilities, parks, and other destinations using SafeGraph points of
interest datasets." **Confirm the units and definition against the source
codebook before keeping that sentence.** Nothing in the currently published
results depends on these columns — no `segpoi_*` variable enters any reported
model — so this is a Methods-text problem, not a results problem, unless you
choose to model them.

## Files in the data folder that are NOT pipeline inputs

Outputs of previous runs, kept for comparison. Safe to delete; they will be
regenerated.

`respondents_with_*.csv`, `landcover_*_long.csv`, `model_data_*.csv`,
`formal_mediation_results*.csv`, `Table*_*.docx`, `domain_models_*.html`,
`swb_*.html`, `maps_outputs/`, `word_ready_tables/`, `word_ready_model_tables/`

Stale outputs from the archived perceived-environment analysis, which the paper
should **not** cite: `Table1_Descriptive_Statistics.docx`,
`Table2_Model_Results.docx`, `Table3_Mediation_Results.docx`,
`swb_model_results.html`, `sem_mediation_results.csv`,
`model1_built_environment_swb.csv`, `model2_built_environment_belonging.csv`,
`model3_belonging_swb.csv`, `model_data_cleaned.csv`, `SWB_Word_Ready_Tables.docx`.
