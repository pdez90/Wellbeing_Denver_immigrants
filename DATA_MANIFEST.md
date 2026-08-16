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
| `ICPSR_38580/DS0003/38580-0003-Data.rda` | 04 | NaNDA street connectivity by tract, 2020. ICPSR study 38580. Note the units: `intdensity` is `n_realnodes / tract_area_sqmiles`, i.e. intersections per **square mile** (national tract mean 94.5); `strnetdensity` is likewise per square mile. |
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

## Note on `seg_poi.csv`

Block-group file with `cbgid`, `All_segregation`, `All_distance`, `race`,
`median_household_income`, `classification`, `residential_segregation`, and a
`*_segregation` / `*_distance` pair for each of 16 point-of-interest categories
(Culture, Entertainment, Grocery, Healthcare, Hospital, Hotel, LifeService,
market, park, personalcare, religious, restaurant, school, socialassistance,
sport, storeshopping).

These are **behavioural mobility measures, not accessibility measures**. The
`*_distance` columns record how far residents of a block group travel to reach
destinations of each category; they are not distances from a home to the nearest
such facility. Three checks establish this:

- `segpoi_park_distance` correlates at r = .05 with `park_nearest_dist_m`, the
  home-to-nearest-park distance computed independently in script 03.
- `park_nearest_dist_m` ranges up to 938 m across the sample, whereas
  `segpoi_park_distance` runs to 399 in its own units.
- The columns are essentially uncorrelated with distance to downtown (r < .04)
  and negatively correlated with population density, the pattern expected of
  trip lengths rather than of proximity.

The 16-category taxonomy matches the activity-site classification used in the
experienced-segregation and travel-behaviour literature built on SafeGraph 2019
weekly patterns. Consult the source codebook before using these columns; the
units of the distance measures are not self-evident from the file.

Script 05 merges these columns and retains them in the analysis file. **No
`segpoi_*` variable enters any model reported in the paper.**

## Files written by the pipeline

The scripts write their outputs into the same folder as the inputs. These are
regenerated on each run and do not need to be retained:

`respondents_with_*.csv`, `landcover_*_long.csv`, `model_data_*.csv`,
`all_model_coefficients.csv`, `model_fit_summary.csv`, `vif_diagnostics.csv`,
`zoning_joint_tests.csv`, `greenness_sensitivity.*`,
`formal_mediation_results*.csv`, `Table*_*.docx`, `domain_models_*`,
`maps_outputs/`, `word_ready_tables/`, `word_ready_model_tables/`,
`sessionInfo.txt`.
