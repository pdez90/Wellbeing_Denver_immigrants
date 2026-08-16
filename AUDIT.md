# Audit: paper text vs. what the code produces

Every statistic in Data & Methods and Results was re-estimated from
`respondents_with_transport_ht_segpoi_stoz_diversity.csv` and compared against
the manuscript. Verdicts below are from a full re-run, not a read-through.

**Headline: the numbers are almost all correct.** Every coefficient, standard
error, R², sample size, and Cronbach's α in the Results section reproduces
exactly. The problems are not arithmetic. They are (a) three claims the code
does not support, (b) systematic use of R² where adjusted R² is the honest
figure, and (c) measures built and then silently dropped.

---

## A. Verified correct

Reproduced to the third decimal:

- α = 0.909 (SWB) and 0.796 (belonging) → paper's 0.91 and 0.79 ✓
- n = 381 total, 368 with SWB, 370 with belonging ✓
- Every descriptive statistic in Tables 1 and 2 ✓ (spot-checked all 35 rows)
- Every coefficient and SE reported in §3.2 through §3.7 ✓
- Every mediation ACME, ADE, total effect, and p-value in §3.7 ✓
- R² values: 0.086, 0.117, 0.142, 0.168, 0.288, 0.311 ✓

---

## B. Claims the code does not support

### B1. VIFs were never computed — and two exceed 10

> §2.6: "Variance inflation factors (VIFs) were examined for all multivariable
> models to assess multicollinearity. VIF values indicated that
> multicollinearity was not a substantial concern in the final domain-specific
> models."

No script in the pipeline that produced Tables 3–8 calls `car::vif()`. The
only VIF check in the entire `.Rmd` is in the archived perceived-environment
chunk, on different models with different variables.

Computed now, for the greenness model:

| Term | VIF |
|---|---|
| `tree_tes_z` | **13.69** |
| `tree_treecanopy_z` | **10.38** |
| `pop_density_z` | 6.53 |
| `housing_density_z` | 6.00 |

Both greenness terms exceed the conventional threshold of 10. (Multi-degree-
of-freedom factors such as `imm_status` show large raw GVIF values simply
because they have seven levels; the comparable quantity for those is
GVIF^(1/df), which stays under 1.5. `vif_diagnostics.csv` reports both, so
factor terms are not mistaken for a collinearity problem.) `tree_tes` and
`tree_treecanopy` correlate at **r = .89** — Tree Equity Score is *computed
from* canopy cover, so they are close to the same variable entered twice. They
carry opposite signs in the belonging model (+0.468\* and −0.369\*), which is
the textbook signature of collinear predictors splitting a shared effect.

**This undercuts the §3.4 finding that "Tree Equity Score was positively
associated with belonging (β = 0.468, p < 0.05)"** — the largest built
environment coefficient in the paper. Fit the model with one canopy measure at
a time before making that claim.

**Fix:** either drop the sentence, or drop one of the two measures and report
what survives. `08_domain_models.R` now writes `vif_diagnostics.csv` so this
is checkable.

### B2. Domain models are compared across different samples

Sample sizes are never stated in the Results text:

| Domain model | n |
|---|---|
| Urban form | 238 |
| Transportation | 238 |
| Greenness and parks | 238 |
| Safety and social | **304** |
| Land use *(new)* | 245 |

The gap traces to coverage: Tree Equity Score, NaNDA intersection density, and
HUD Jobs Proximity are present for only 278 of 358 geocoded respondents, versus
358 for the buffer-derived measures. §3.3 compares effect sizes across domains
("Transportation infrastructure demonstrated some of the strongest
associations") as though the samples matched. They don't.

**Fix:** report n in every table (now automatic), and add one sentence in §2.4
about differential coverage.

### B3. Mediation models adjust for a different covariate set

Table 8 models drop gender, marital status, ethnicity, race, and immigration
status. This was deliberate — a bootstrap resample can empty a factor level and
crash `mediation::mediate()` — and it is defensible, but it is not disclosed.
Consequences: n = 319 rather than 238–304, and the confounder set is not the
one §2.5 describes.

**Fix:** state it in §2.6 and in the Table 8 note. Both are now in the code.

---

## C. Overstated but not wrong

### C1. R² is reported where adjusted R² is the honest number

> §3.6: "The final subjective well-being model explained approximately 29% of
> the variation in subjective well-being (R² = 0.288)."

With ~35 predictors and n = 238, R² = 0.288 corresponds to **adjusted
R² = 0.148**. The belonging model's R² = 0.311 is adjusted R² = 0.179. The
domain models are starker: three of four have adjusted R² at or below zero.

| Model | R² | Adj. R² |
|---|---|---|
| SWB: urban form | 0.142 | **−0.002** |
| SWB: transportation | 0.174 | 0.026 |
| SWB: greenness | 0.151 | **−0.006** |
| SWB: safety | 0.168 | 0.052 |
| SWB: final + belonging | 0.288 | 0.148 |

§3.3 already says "adjusted R² values ranged from approximately 0.00 to 0.05,"
which quietly rounds two negative values up to 0.00. §3.6 then switches to
unadjusted R² for the headline. Use one measure throughout — adjusted.

### C2. "Across every model" is not accurate

> §3.4: "Respondents who had lived longer in their current neighborhoods
> reported significantly stronger neighborhood belonging across every model
> (β ≈ 0.17, p < 0.05)."

In the safety/social model it is β = 0.126, p < 0.10. Four of five, not five.

> §3.4: "neighborhood socioeconomic status also demonstrated positive
> associations (β = 0.255–0.303, p < 0.01)"

The stated range excludes the safety model's β = 0.089, ns. Say "in three of
four domain models."

### C3. Safety measures were not attenuated by belonging

> §3.5: "associations for parks, greenness, and safety measures were attenuated
> after inclusion of neighborhood belonging."

True for parks and greenness. False for safety: crash density went from
−0.185 (p < 0.10) to −0.180 (**p < 0.05**) — it strengthened — and bicycle
crash density stayed significant at p < 0.01.

### C4. §3.6 mixes two model columns without saying so

"EPA walkability also remained independently associated (β = 0.157, p < 0.05)"
is from the *with-belonging* column. In the final model without belonging it is
β = 0.208. Both are real; the text should name which column each number is from.

### C5. Sample range is slightly off

§2.1 says analytic samples "ranged from approximately 240 to 320." Actual range
is 238–320.

---

## D. Built but never used

Script 02 and 04 construct these, and no reported model contains any of them:

| Measure | Columns | Coverage |
|---|---|---|
| Zoning category | `zone_gen_zone2` | 338 / 381 |
| ADU permitted | `zone_adu` | 290 / 381 |
| Zoning jurisdiction | `zone_jurisd` | 338 / 381 |
| Pedestrian focus areas | `pfa_share_400/800/1600` | 358 / 381 |
| POI travel/segregation | 45 `segpoi_*` columns | 349 / 381 |
| EnviroScreen indicators | 105 `env_*` columns | 278 / 381 |

Methods §2.4 explicitly promises three of these: "Respondents were also linked
to zoning classifications, pedestrian focus areas, and neighborhood
recreational opportunities through spatial overlays," and "proximity to grocery
stores, schools, healthcare facilities, parks … using SafeGraph points of
interest datasets." A reader looking for those results will not find them.

### The new land-use domain result

`08_domain_models.R` now models zoning, ADU permission, and pedestrian focus
area share (n = 245). **Nothing is significant.**

| Predictor | SWB | Belonging |
|---|---|---|
| Zoning category (joint F test, 3 df) | F = 0.13, p = .942 | F = 1.04, p = .375 |
| ADU permitted | −0.071 (0.162) | −0.163 (0.160) |
| Pedestrian focus area share | 0.076 (0.097) | 0.060 (0.095) |

Max VIF 9.16, so this is a clean null rather than a collinearity artifact.

This is a *reportable* result and a better outcome than silence: regulatory
land-use category does not predict well-being or belonging once individual
characteristics and neighborhood socioeconomic context are held constant,
whereas physical infrastructure (walkability, bicycle facilities, parks) does.
That distinction — form matters, zoning label doesn't — is a genuine planning
finding, and it strengthens the paper's argument that the *material* built
environment is what functions as a context of reception.

### The POI measures — do not write these up yet

Modelling `segpoi_*_distance` produces two significant indirect effects through
belonging (school p = .022, grocery p = .032), which would contradict the
paper's central mediation null. **I am not recommending you report them**,
because the variable is probably not what Methods says it is — see
`DATA_MANIFEST.md`. The `*_distance` columns are paired one-to-one with
`*_segregation` columns and average 39–85 with a maximum of 461, which is
inconsistent with home-to-nearest-destination distance. They look like average
travel distances from an experienced-segregation dataset.

Resolve the codebook question first. If they *are* travel distances, they
belong in a mobility framing, not an accessibility one — and the finding is
interesting enough to be worth the check.

---

## E. Smaller items

1. **Intersection density units.** §3.1 reports "165 intersections/km²." The
   NaNDA source carries `street_tract_area_sqmiles`, which suggests the density
   is per square *mile*. Verify against the NaNDA codebook (ICPSR 38580).
2. **Two different tree canopy variables** appear in the same model.
   `tree_treecanopy` (Tree Equity, block group) and `lc_800m_tree_canopy`
   (DRCOG land cover, 800 m buffer) are separate measures. §3.3 and §3.4 refer
   to "tree canopy within the residential buffer" for the land-cover one
   without ever naming the other, whose coefficient is significantly *negative*
   in the belonging model (−0.369, p < 0.05). Report both or drop one.
3. **No seed on the bootstrap.** The published Table 8 CIs were not exactly
   reproducible. Fixed.
4. **Ten mediation tests, no multiplicity adjustment.** Holm-adjusted p-values
   are now reported alongside. This does not change the paper's conclusion,
   which is a null — it strengthens it.
5. **Two data sources selected by glob.** Crash data and HUD Jobs Proximity
   were picked with `list.files(pattern = ...)[1]`. `crash_2019.csv` and
   `crash_2019/drcog_crash_2019.shp` both matched. Now pinned by name.
6. **Stale outputs in the data folder.** `Table1_Descriptive_Statistics.docx`,
   `Table2_Model_Results.docx`, `Table3_Mediation_Results.docx`,
   `sem_mediation_results.csv`, and `swb_model_results.html` are from the
   archived perceived-environment analysis and do not correspond to anything in
   the current paper. Listed in `DATA_MANIFEST.md`.

---

## Priority

**Before submission:**
- B1 (VIF claim is false and one headline finding is unstable)
- B2 (state sample sizes)
- B3 (disclose the mediation covariate set)
- C1 (use adjusted R² consistently)

**Should fix:**
- C2, C3, C4, C5 — small text corrections
- D — add the land-use null, or cut the Methods sentences promising it

**Verify, then decide:**
- E1 (intersection density units)
- The `segpoi_*` codebook question
