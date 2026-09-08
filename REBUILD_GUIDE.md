# Rebuilding the analysis on a new machine

Written 8 September 2026, after the laptop holding the working copy failed.

The survey is recovered and byte-faithful. The code is recovered in full. What
cannot be recovered as it was is the third-party geospatial input, and that
turns out to matter more than expected. **Read the vintage section before
downloading anything.**

---

## 1. The finding that shapes everything else

Between the original run and now, several of the upstream sources have been
revised, and in most of those cases the vintage the paper used is no longer
published. Checked against the live portals on 8 September 2026:

| Source | Variable(s) it feeds | Status |
|---|---|---|
| DRCOG **Urban Centers** | `urban_center_nearest_dist_m` | **2019 vintage not published anywhere.** Live layer modified 6 Jan 2026. |
| DRCOG **Short Trip Opportunity Zones** | `short_trip_zone_share_800` | **Replaced.** Current layer created Nov 2025, derived from the ATP adopted Jan 2026. No prior version published. |
| American Forests **Tree Equity Score** | `tree_tes`, `tree_treecanopy` | **TES 1.0 replaced by TES 2.0** (2023). New canopy data, revised boundaries, and adjusted biome targets for grassland- and desert-adjacent cities — which is Colorado specifically. |
| CNT **H+T Affordability Index** | `ht_t_ami` | **2019 release withdrawn.** Old file URLs 404. Current release uses 2022 ACS. Download now requires registration; CNT directs longitudinal requests to `info@cnt.org`. |
| DRCOG **Bicycle Facility Inventory** | `bike_facility_density_800` | Undated and continuously updated; DRCOG does not publish dated snapshots. |
| DRCOG **Parks and Open Space** | `park_acres_half_mile`, `park_nearest_dist_m` | 2024 vintage unconfirmed; the only vintage-tagged slug found is 2019, and the ArcGIS layer was rebuilt Jan 2025. |
| **NaNDA** street connectivity | `street_intdensity` | ICPSR 38580 is now at **V3**. Contents may differ from the version used. |
| **MODIS** MOD13Q1 | `ndvi_800` | Collection **6.0 decommissioned 31 July 2023**; only v061 is served. |
| HUD **Jobs Proximity Index** | `hudjob_jobs_idx` | Two block-group layers now published, with different feature counts (217,339 vs 242,037) and different vintages. |
| DRCOG **Pedestrian Focus Areas** | `pfa_share_800` | Current layer is a Jan 2025 refresh, **but an explicit archive layer from Jan 2023 is preserved.** |

### Why this is serious

Of the six exposures carried into the mediation analysis, five draw on a source
in that table:

| Exposure | ACME | Source at risk? |
|---|---|---|
| **Distance to urban center** | **0.087** | **yes — vintage gone** |
| Bicycle facility density | 0.045 | yes — undated source |
| Park acreage within 800 m | 0.049 | yes — vintage unconfirmed |
| Bicycle crash density | −0.004 | no (crash 2019 appears retained) |
| Pedestrian focus area share | −0.014 | partly — Jan 2023 archive exists |
| ADU permitted | −0.131 | separate problem, see §2 |

**The headline result is the most exposed one.** Distance to the nearest urban
center carries the paper's main finding, and the 2019 Urban Centers layer that
defines it is not currently downloadable. A rebuild using the January 2026 layer
would produce a number, and that number would not be this paper's number.

So a re-run is worth doing, but it should be understood as *re-estimating on
current data*, not as *reproducing the published result*, unless DRCOG can
supply the archived layers. Which leads to the first action.

### Do this first, because it has the longest lead time

Email **geospatial@drcog.org** and ask whether archived copies exist of:

- Urban Centers, 2019 vintage
- Short Trip Opportunity Zones, the version predating the Jan 2026 ATP
- Parks and Open Space, 2024 vintage
- Bicycle Facility Inventory, a snapshot from the original run period

Send that before starting any download. Everything else can proceed in parallel,
but this is the critical path for reproducibility.

Also worth an email: **CNT** (`info@cnt.org`) for the 2019 H+T block-group file,
which is no longer on the public download path.

---

## 2. The one input with no external source at all

`Zoning/ALL_DenverMSA_10.3.23.shp` — "Denver MSA harmonized zoning, October 2023
vintage" — feeds `zone_adu_yes` and `zone_category`. The data manifest names no
portal for it, which suggests it was harmonized by the project team rather than
downloaded.

If no copy survives elsewhere, `zone_adu_yes` cannot be rebuilt, and it is one of
the six mediation exposures. **Check with Jeremy and Alessandro before assuming
it is lost** — a co-author, a student, or a shared drive may well have it. This
is worth asking about today.

---

## 3. Sources verified as still available

Checked 8 September 2026. Sizes are as stated by the publisher.

| Dataset | Where | Notes |
|---|---|---|
| EPA Smart Location Database v3 | `https://edg.epa.gov/EPADataCommons/public/OA/SLD/SmartLocationDatabaseV3.zip` | ~527 MB, unchanged since 2021-06-08. Still current; no v4. |
| EPA National Walkability Index | `https://edg.epa.gov/EPADataCommons/public/OA/WalkabilityIndex.zip` | ~425 MB, file date 2021-06-08 matches the June 2021 release. |
| Colorado EnviroScreen v2 | `https://cdphe.colorado.gov/enviroscreen` | v2 is still current; only v1.0 is archived. |
| HUD Jobs Proximity Index | `https://hudgis-hud.opendata.arcgis.com/datasets/HUD::jobs-proximity-index/about` | Pin down which of the two layers matches the original before using either. |
| NaNDA street connectivity | `https://www.icpsr.umich.edu/web/ICPSR/studies/38580` | Public use, no institutional affiliation needed; a free MyData account is required to download. Now V3. DS3 is the 2020 tract file. |
| Diversity (Xu et al. 2024) | `https://osf.io/x94gj` | Public, no login. Colorado file follows the documented pattern `diversity_intervals_CO.csv`. |
| Segregation/POI (Zhou & Lu 2025) | `https://figshare.com/projects/Travel_Behaviour_and_Income_Segregation/266242` | Live, CC BY 4.0. The block-group table is inside `Source Data.xlsx` (~340 MB) — the project's only data file. Enters no model in the paper. |
| Census API key | `https://api.census.gov/data/key_signup.html` | Free. ACS 2015–2019 five-year is still served. |
| MODIS MOD13Q1 | `https://modis.ornl.gov/data/modis_webservice.html` | The ORNL service `MODISTools` calls. **No Earthdata login needed** — MODISTools uses ORNL, not AppEEARS. v061 only. |
| DRCOG catalog | `https://data.drcog.org/data` | Fully public, CC BY 3.0. The catalog is a JavaScript app, so browse it in a real browser; several layers are distributed through DRCOG's ArcGIS organisation rather than the catalog. |

Note on the land cover raster: DRCOG distributes it as a **File Geodatabase**,
not as the 6.6 GB GeoTIFF the scripts expect. The `.tif` was a local export, so
that conversion has to be re-run.

---

## 4. The rebuild itself

```bash
# 1. code
git clone https://github.com/pdez90/Wellbeing_Denver_immigrants.git
cd Wellbeing_Denver_immigrants

# 2. packages, pinned to the versions the analysis used
R -e 'install.packages("renv"); renv::restore()'

# 3. data folder -- swb_data.csv plus the subfolder names in DATA_MANIFEST.md
mkdir -p ~/Wellbeing/data
cp /path/to/recovered/swb_data.csv ~/Wellbeing/data/

# 4. Census key, once
R -e 'tidycensus::census_api_key("YOUR_KEY", install = TRUE)'

# 5. run
cd scripts
WELLBEING_DIR=~/Wellbeing/data Rscript run_all.R
WELLBEING_DIR=~/Wellbeing/data Rscript 21_ndvi.R      # excluded from run_all: external API

# 6. check the rebuild against the published numbers BEFORE touching the paper
WELLBEING_DIR=~/Wellbeing/data Rscript 25_verify_rebuild.R
```

The slow step is `02`, which reads the land cover raster; budget an hour or
more. `run_all.R` accepts `--analysis` to skip the spatial stages once the
`respondents_with_*` files exist, which is what you want on every run after the
first.

---

## 5. Checking the rebuild

`25_verify_rebuild.R` compares a re-run against `reference/ref_*.csv`, which
ships in this repository and holds the published values.

It checks descriptive statistics **before** it checks any coefficient, and that
order is the point. If a data source changed vintage, the variable's mean and
spread move immediately, whereas a coefficient can absorb a moderate change in
an input and still look entirely plausible. So:

- **descriptives drift → the input changed.** Find out which vintage you have
  and report it.
- **descriptives match but a coefficient moved → the processing changed.** That
  is a bug, and it is in the code.

The script separates drift with a *known* upstream cause from drift without one,
because the second kind is the one that should worry you.

Means are compared against 5 per cent of the published standard deviation rather
than 5 per cent of the published mean — several variables are centred near zero,
and a relative-to-mean test on the neighborhood SES index (mean 0.02, SD 0.90)
would flag a shift of 0.001 as a 7 per cent error.

Tolerances are deliberately tight. If output drifts, the response is to explain
it, not to widen them.

---

## 6. If the archived layers cannot be recovered

Then the paper has a provenance problem to disclose rather than a result to
re-estimate. The defensible options, roughly in order of preference:

1. **Recover the layers from a co-author or a shared drive.** Alessandro and
   Jeremy may hold copies; so may any student who worked on the project. Cheapest
   by far, and the only route that preserves the published numbers exactly.
2. **Report both.** Publish the original estimates, and add an SI section giving
   the same models on current vintages, stating plainly which layers changed.
   This is more work but is stronger than either number alone.
3. **Re-estimate on current data throughout** and describe the paper as using
   2025–26 vintages. Honest, but it changes the headline number and invalidates
   the current draft's results section.

What should not happen is a rebuild on current layers reported as though it were
the original. The verification script exists so that this cannot happen by
accident.
