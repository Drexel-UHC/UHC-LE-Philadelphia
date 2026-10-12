# Load Libraries & Data ---------------------------------------------------


rm(list = ls())
library(here)
library(tidyverse)

# Create variable to house directory paths
results <- here("Results")
working_results <- here(results, "Working Results")
main_text <- here(results, "Main Text")
appendix <- here(results, "Appendix")
path_onedrive <- here("Data - Cleansed")
dir_name <- here("//files.drexel.edu/encrypted/SOPH/UHC/SchnakeMahl_PAVitalStat/Data")

# Load data created in 01-Crosswalks
load(file = here(path_onedrive, "sex_years.Rdata"))
load(file = here(path_onedrive, "low_pop_tracts.Rdata"))

# Date for file saving/loading
date <- "20260716"

# Load data created in 02-Deaths and 03-Population
load(file = here(path_onedrive, paste0("phila_pop_", date, ".Rdata")))
load(here(dir_name, "Philly LE", paste0("phila_death_counts_", date, ".Rdata")))

# Load helper functions
source(here("Helpers", "compute_smooth_rates.R"))
source(here("Helpers", "print_message.R"))


# Mortality Rates ---------------------------------------------------------


phila_death_rates <- compute_smooth_rates(
  phila_death_counts_nolow,
  phila_pop$pop_age_all_fixes
)

# unique GEOIDs: 384
# GEOID x years x sex x age combinations where deaths were capped: 50
# deaths removed because deaths > pop: 81 (0.1% of total)
# deaths removed because deaths > pop (2017-2019): 40
# deaths removed because deaths > pop (2020-2022): 41
# deaths in low pop tracts (should be 0): 0
# pop in low pop tracts (should be 0): 0
# total deaths: 90,306
# total pop: 9,442,990

phila_death_rates_no_pop_adjust <- compute_smooth_rates(
  phila_death_counts_nolow,
  phila_pop$pop_age_no_low
)

# unique GEOIDs: 384
# GEOID x years x sex x age combinations where deaths were capped: 61
# deaths removed because deaths > pop: 102 (0.1% of total)
# deaths removed because deaths > pop (2017-2019): 47
# deaths removed because deaths > pop (2020-2022): 55
# deaths in low pop tracts (should be 0): 0
# pop in low pop tracts (should be 0): 0
# total deaths: 90,285
# total pop: 9,442,531

save(
  phila_death_rates,
  phila_death_rates_no_pop_adjust,
  file = here(dir_name, "Philly LE", paste0("phila_death_rates_", date, ".Rdata"))
)
