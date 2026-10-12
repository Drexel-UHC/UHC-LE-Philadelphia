# UHC-LE-Philadelphia

Code for **"A Bayesian Approach to Estimating Changes in Small Area Life Expectancy Disparities in Philadelphia"** (Anfuso G, Quick H, Schnake-Mahl A, Bilal U).

We estimate gender-specific life expectancy (LE) for Philadelphia census tracts (2010 boundaries) before (2017–2019) and during (2020–2022) the COVID-19 pandemic using a Bayesian Poisson TOPALS model with spatial (CAR) and non-spatial random effects, then summarize total and income-based (SII/RII) disparities in LE.

**Interactive dashboard:** <https://drexel-uhc.github.io/UHC-LE-Philadelphia/>

## Repository structure

| Path | Description |
|---|---|
| `LE-Philadelphia-01-Crosswalks.R` | Census tract shapefiles, 2020-to-2010 tract crosswalk, low population/special land use tracts, and tract/age/sex/period lookup tables |
| `LE-Philadelphia-02-Deaths.R` | Cleans death records and computes death counts by tract, period, sex, and single-year age (0–85+) |
| `LE-Philadelphia-03-Population.R` | Reads SEER tract population estimates, fixes unexpected zero populations (Table S1), and graduates age groups to single-year ages using PCLM |
| `LE-Philadelphia-04-Mortality-Rates.R` | Caps deaths at population for each tract × gender × age × period group, computes tract- and city-level mortality rates, and smooths the city-wide standard schedule |
| `LE-Philadelphia-05-Models.R` | Fits the NIMBLE models (one per sex × period), checks convergence, and computes life tables for each posterior iteration |
| `LE-Philadelphia-06-Analysis.R` | Median household income (ACS), disparity measures, and all tables and figures in the paper and supplement |
| `Helpers/` | Functions sourced by the scripts above (each documented with a roxygen-style header) |
| `Quarto/` | Source (`index.qmd`) and data for the interactive dashboard |
| `docs/` | Rendered dashboard (served via GitHub Pages) |
| `Data/` | Census tract shapefiles (2010 and 2020 boundaries) |

Scripts should be run in order (01 → 06). Each script loads the outputs of earlier scripts from disk, and sections within scripts can generally be run independently once those outputs exist.

## Data

| Data | Source | Availability |
|---|---|---|
| Death records, 2017–2022 | Bureau of Health Statistics and Registries, Pennsylvania Department of Health | **Not publicly available.** Read from a restricted Drexel network location (`dir_name` in the scripts). |
| Census tract population estimates | NCI SEER, *Populations – Total U.S. (2006–2023), Census Tract Estimates by Race/Origin, Vintage 2023 (2010 tract geographies)* (<https://seer.cancer.gov/censustract-pops/>) | Public. Download and save as `Data/us.2006_2023.tract.level.pops.txt`. |
| 2020-to-2010 census tract crosswalk | Longitudinal Tract Data Base (LTDB), Brown University (<https://s4.ad.brown.edu/projects/diversity/Researcher/Bridging.htm>) | Public. Save as `Data/ltdb_ct20toct10_xwalk_files/ct20_ct10_xwalk.csv`. |
| Median household income | 2015–2019 and 2018–2022 American Community Survey 5-year estimates (table B19013), via `tidycensus` | Public. Requires a Census API key (`tidycensus::census_api_key()`). |
| Census tract shapefiles | Included in `Data/` | Public |

The Pennsylvania Department of Health specifically disclaims responsibility for any analyses, interpretations, or conclusions.

## Reproducing the analysis

1. Clone the repository and open `UHC-LE-Philadelphia.Rproj`.
2. Restore the package environment (R 4.6.1):
   ```r
   renv::restore()
   ```
3. Download the public data listed above into `Data/`.
4. Create the output folders (they are not tracked by git):
   ```r
   dir.create("Data - Cleansed")
   dir.create("Results/Working Results", recursive = TRUE)
   dir.create("Results/Main Text")
   dir.create("Results/Appendix")
   ```
5. Run the scripts in order. Intermediate files are saved with a date stamp (`date <- "20260716"` at the top of each script); update it in every script if you re-run the pipeline.

Approximate runtimes: the NIMBLE models in `05-Models` take about 4 hours (main analysis) and 3.7 hours (no population adjustments), and the life table calculations take about 5.5 hours each. Both steps run in parallel across sex × period strata.

Because the death records are restricted, the pipeline cannot be fully reproduced without access to them. The dashboard data in `Quarto/Data/` contain the tract-level LE estimates shown in the paper.

## Citation

> Anfuso G, Quick H, Schnake-Mahl A, Bilal U. A Bayesian approach to estimating changes in small area life expectancy disparities in Philadelphia. *Am J Epidemiol*. [In press / year; volume(issue):pages. doi:]

## Contact

Giancarlo Anfuso (ga437@drexel.edu), Urban Health Collaborative, Drexel University Dornsife School of Public Health
