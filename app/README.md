# Interactive explorer (Shiny)

An aggregate-only companion to the paper. It reads the model output,
descriptive tables and census-geography aggregates that the pipeline writes,
and nothing else: no respondent row is loaded, no residential location is
mapped, and respondent counts by tract are suppressed below five.

## Build the inputs (once per pipeline run)

```r
install.packages(c("shiny", "DT", "leaflet", "sf", "tigris", "ggplot2", "dplyr", "readr", "janitor"))
```

```bash
cd ~/Wellbeing/Wellbeing_Denver_immigrants
WELLBEING_DIR=~/Wellbeing/data Rscript app/prepare_app_data.R
```

This fills `app/data/` (tables and `geo.rds`) and `app/www/` (figures) and
writes `app/data/MANIFEST.md` listing every file the app can read. Needs
internet the first time for the census geometry.

## Run

```r
shiny::runApp("app")
```

## Sharing

`app/data/` and `app/www/` contain only aggregates and are safe to deploy
(shinyapps.io, Posit Connect, or a lab server). They are generated files and
are not tracked in git; regenerate them from the data folder.

## What is in it

- Overview: design, verification status, graphical abstract, model fit.
- Domain models: the coefficient plot by variable group, with 90/95% intervals and the table behind it.
- Integrated model and mediation, with the confounding sensitivity.
- Sensitivity checks: buffer radius, greenness specification, common sample, spatial standard errors, robust regression, item overlap.
- Descriptives: every variable group with its distribution panel.
- Map: respondents per tract (suppressed < 5) and public area-level measures by tract or block group.
- Variables: definitions, units, geography, vintage and source.
