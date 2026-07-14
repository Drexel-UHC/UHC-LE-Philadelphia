# Load Libraries & Data ---------------------------------------------------


rm(list = ls())
library(here)
library(tidyverse)

# Create variable to house directory paths
dir_name <- here("//files.drexel.edu/encrypted/SOPH/UHC/SchnakeMahl_PAVitalStat/Data")
results <- here("Results")
working_results <- here(results, "Working Results/")
main_text <- here(results, "Main Text/")
appendix <- here(results, "Appendix/")
path_onedrive <- here("Data - Cleansed")

# Load data created in 01-Crosswalks
load(file = here(path_onedrive, "phila_sf.Rdata"))
load(file = here(path_onedrive, "low_pop_tracts.Rdata"))
load(file = here(path_onedrive, "years_cross.Rdata"))
load(file = here(path_onedrive, "age_grp.Rdata"))
load(file = here(path_onedrive, "phila_tracts_ages_sexes_years.Rdata"))

# All tracts in Philly
phila_tracts <- unique(phila_sf$GEOID)
N_tracts <- length(phila_tracts)

# Age limit for age at death
age_limit <- 85

# Today's date, formatted for file saving
date <- format(Sys.Date(), "%Y%m%d")

# Load helper functions
source(here("Helpers", "print_message.R"))


# Death Data --------------------------------------------------------------


# Columns we need for mortality files
keep_death <- c(
  "SEX",
  "GEOID10",
  "AGE_DEATH",
  "RACE1215",
  "HISPANIC"
)

# Data types for columns
col_spec <- cols(
  .default  = col_character(),
  SEX       = col_double(),
  GEOID10   = col_character(),
  Age_death = col_double(),
  RACE1215  = col_character(),
  HISPANIC  = col_double()
)

# Import and combine mortality data
deaths <- map_dfr(2017:2022, function(i) {
  df <- read_csv(
    here(dir_name, paste0("deathgeo", i, ".csv")),
    col_types = col_spec,
    show_col_types = FALSE
  )

  names(df) <- toupper(names(df))

  df %>%
    select(all_of(keep_death)) %>%
    mutate(
      year = i,
      GEOID = GEOID10
    )
})

print_message(
  "% of deaths that could not be geocoded (2017-2019)" = nrow(filter(deaths, year <= 2019, is.na(GEOID))) / nrow(filter(deaths, year <= 2019)) * 100,
  "% of deaths that could not be geocoded (2020-2022)" = nrow(filter(deaths, year >= 2020, is.na(GEOID))) / nrow(filter(deaths, year >= 2020)) * 100,
  "% of deaths that could not be geocoded (2017-2022)" = nrow(filter(deaths, is.na(GEOID))) / nrow(deaths) * 100
)
# % of deaths that could not be geocoded (2017-2019): 3.382928
# % of deaths that could not be geocoded (2020-2022): 3.041299
# % of deaths that could not be geocoded (2017-2022): 3.201812

# Modified dataframe of PA death data with demographic information
deaths_modified <- deaths %>%
  mutate(
    sex = case_when(
      SEX == 1 ~ "Men",
      SEX == 2 ~ "Women",
      TRUE ~ "Unknown"
    ),
    race_num = case_when(
      as.integer(RACE1215) == 1 ~ 1,
      as.integer(RACE1215) == 2 ~ 2,
      as.integer(RACE1215) == 3 ~ 3,
      as.integer(RACE1215) >= 4 & as.integer(RACE1215) <= 14 ~ 4,
      TRUE ~ 5
    ),
    race = factor(
      race_num,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "White",
        "Black",
        "American Indian, Alaska Native",
        "Asian American Pacific Islander",
        "Other"
      )
    ),
    hisp_num = case_when(
      HISPANIC == 1 ~ 1,
      HISPANIC >= 2 & HISPANIC <= 5 ~ 2,
      TRUE ~ 3
    ),
    hisp = factor(
      hisp_num,
      levels = c(1, 2, 3),
      labels = c("Non-Hispanic", "Hispanic", "Unknown")
    ),
    racehisp_num = case_when(
      hisp_num == 2 ~ 1,
      hisp_num == 1 & race_num == 1 ~ 2,
      hisp_num == 1 & race_num == 2 ~ 3,
      hisp_num == 1 & race_num == 3 ~ 4,
      hisp_num == 1 & race_num == 4 ~ 5,
      hisp_num == 1 & race_num == 5 ~ 6,
      TRUE ~ 7
    ),
    racehisp = factor(
      racehisp_num,
      levels = 1:7,
      labels = c(
        "Hispanic",
        "Non-Hispanic White",
        "Non-Hispanic Black",
        "Non-Hispanic American Indian, Alaska Native",
        "Non-Hispanic Asian American Pacific Islander",
        "Non-Hispanic Other",
        "Unknown"
      )
    ),
    phila = substr(GEOID, 1, 5) == "42101",
    geocoded = case_when(
      is.na(GEOID) ~ "Non-Geocoded",
      TRUE ~ "Geocoded"
    )
  ) %>%
  select(-race_num, -hisp_num, -racehisp_num)

print_message(
  "% of deaths missing sex (2017-2019)" = nrow(deaths_modified %>% filter(year <= 2019, SEX == 9)) / nrow(deaths_modified %>% filter(year <= 2019)) * 100,
  "% of deaths missing sex (2020-2022)" = nrow(deaths_modified %>% filter(year >= 2020, SEX == 9)) / nrow(deaths_modified %>% filter(year >= 2020)) * 100,
  "% of deaths missing sex (2017-2022)" = nrow(deaths_modified %>% filter(SEX == 9)) / nrow(deaths_modified) * 100,
  "% of deaths missing age at death (2017-2019)" = nrow(deaths_modified %>% filter(year <= 2019, is.na(AGE_DEATH))) / nrow(deaths_modified %>% filter(year <= 2019)) * 100,
  "% of deaths missing age at death (2020-2022)" = nrow(deaths_modified %>% filter(year >= 2020, is.na(AGE_DEATH))) / nrow(deaths_modified %>% filter(year >= 2020)) * 100,
  "% of deaths missing age at death (2017-2022)" = nrow(deaths_modified %>% filter(is.na(AGE_DEATH))) / nrow(deaths_modified) * 100,
  "% of Philly deaths not fully geocoded (2017-2019)" =
    nrow(deaths_modified %>% filter(year <= 2019, substr(GEOID, 1, 5) == 42101, nchar(GEOID) < 11)) / nrow(deaths_modified %>% filter(year <= 2019, substr(GEOID, 1, 5) == 42101)) * 100,
  "% of Philly deaths not fully geocoded (2020-2022)" =
    nrow(deaths_modified %>% filter(year >= 2020, substr(GEOID, 1, 5) == 42101, nchar(GEOID) < 11)) / nrow(deaths_modified %>% filter(year >= 2020, substr(GEOID, 1, 5) == 42101)) * 100,
  "% of Philly deaths not fully geocoded (2017-2022)" =
    nrow(deaths_modified %>% filter(substr(GEOID, 1, 5) == 42101, nchar(GEOID) < 11)) / nrow(deaths_modified %>% filter(substr(GEOID, 1, 5) == 42101)) * 100
)
# % of deaths missing sex (2017-2019): 0.0007346207
# % of deaths missing sex (2020-2022): 0.00303826
# % of deaths missing sex (2017-2022): 0.001955902
# % of deaths missing age at death (2017-2019): 0.02252837
# % of deaths missing age at death (2020-2022): 0.008463725
# % of deaths missing age at death (2017-2022): 0.01507195
# % of Philly deaths not fully geocoded (2017-2019): 0
# % of Philly deaths not fully geocoded (2020-2022): 0
# % of Philly deaths not fully geocoded (2017-2022): 0


# Philadelphia Deaths -----------------------------------------------------

# Philly deaths helper function
phila_deaths_df <- function(df) {
  df %>%
    filter(GEOID %in% phila_tracts) %>%
    mutate(
      years = case_when(
        year >= 2017 & year <= 2019 ~ "2017-2019",
        year >= 2020 & year <= 2022 ~ "2020-2022"
      ),
      sex = case_when(
        SEX == 1 ~ "Men",
        SEX == 2 ~ "Women",
        SEX == 9 ~ "Unknown"
      ),
      age = case_when(
        floor(AGE_DEATH) >= age_limit ~ age_limit,
        floor(AGE_DEATH) < age_limit ~ floor(AGE_DEATH)
      )
    ) %>%
    inner_join(age_grp)
}

# Philly deaths by tract x sex x age group
phila_deaths <- deaths %>%
  phila_deaths_df()

print_message(
  "Deaths from low population/special land use tracts" = nrow(filter(phila_deaths, GEOID %in% low_pop)),
  "% of deaths from low population/special land use tracts" = nrow(filter(phila_deaths, GEOID %in% low_pop)) / nrow(phila_deaths) * 100
)
# Deaths from low population/special land use tracts: 301
# % of deaths from low population/special land use tracts: 0.3318926

# Philadelphia deaths with low population census tracts removed
phila_deaths_nolow <- phila_deaths %>%
  filter(!(GEOID %in% low_pop))

# Philadelphia deaths with demographic information
phila_deaths_modified <- deaths_modified %>%
  phila_deaths_df()

# Philadelphia deaths with demographic information - low population/special land use tracts removed
phila_deaths_nolow_modified <- phila_deaths_modified %>%
  filter(!(GEOID %in% low_pop))

save(
  phila_deaths,
  phila_deaths_nolow,
  phila_deaths_nolow_modified,
  file = here(dir_name, "Philly LE", paste0("phila_deaths_", date, ".Rdata"))
)

print_message(
  "Deaths missing sex data in final sample" = nrow(filter(phila_deaths_nolow_modified, SEX == 9)),
  "% of deaths missing sex data in final sample" = nrow(filter(phila_deaths_nolow_modified, SEX == 9)) / nrow(phila_deaths_nolow_modified) * 100,
  "Deaths missing age at death data in final sample" = nrow(filter(phila_deaths_nolow_modified, is.na(AGE_DEATH))),
  "% of deaths missing age at death data in final sample" = nrow(filter(phila_deaths_nolow_modified, is.na(AGE_DEATH))) / nrow(phila_deaths_nolow_modified) * 100
)
# Deaths missing sex data in final sample: 4
# % of deaths missing sex data in final sample: 0.004425219
# Deaths missing age at death data in final sample: 0
# % of deaths missing age at death data in final sample: 0

# Philly death counts - exclude entries missing data for sex and age
phila_death_counts <- phila_deaths %>%
  filter(SEX != 9, !is.na(age)) %>%
  mutate(sex = as.factor(sex)) %>%
  group_by(GEOID, sex, years, age) %>%
  summarise(
    deaths = n(),
    .groups = "drop"
  ) %>%
  full_join(phila_tracts_ages_sexes_years, by = c("GEOID", "years", "sex", "age")) %>%
  complete(GEOID, sex, years, age, fill = list(deaths = 0)) %>%
  filter(sex %in% c("Men", "Women"), !is.na(years), !is.na(age), GEOID %in% phila_tracts) %>%
  arrange(sex, years, tract, age)

print_message(
  "Total deaths in sample" = sum(phila_death_counts$deaths),
  "% of final sample with 0 deaths" = nrow(filter(phila_death_counts, deaths == 0)) / nrow(phila_death_counts) * 100
)
# Total deaths in sample: 90,688
# % of final sample with 0 deaths: 67.96572

# FINAL SAMPLE - Philly death counts by age with low population/special land use census tracts removed
phila_death_counts_nolow <- phila_death_counts %>%
  mutate(
    deaths = case_when(
      GEOID %in% low_pop ~ 0,
      TRUE ~ deaths
    )
  )

print_message(
  "Total deaths before removing deaths from low population tracts" = sum(phila_death_counts$deaths),
  "% of sample with 0 deaths" = nrow(filter(phila_death_counts, deaths == 0)) / nrow(phila_death_counts) * 100,
  "Total deaths in final sample" = sum(phila_death_counts_nolow$deaths),
  "Deaths removed from low population tracts" = sum(phila_death_counts$deaths) - sum(phila_death_counts_nolow$deaths)
)
# Total deaths before removing deaths from low population tracts: 90,688
# % of sample with 0 deaths: 67.96572
# Total deaths in final sample: 90,387
# Deaths removed from low population tracts: 301

# Death counts by period in final sample
phila_death_counts_nolow %>%
  group_by(years) %>%
  summarise(deaths = sum(deaths))

# Philly death counts by age with low population census tracts removed - includes unknown
phila_death_count_nolow_unknown <- phila_deaths %>%
  filter(!is.na(age)) %>%
  mutate(sex = as.factor(sex)) %>%
  group_by(GEOID, sex, years, age) %>%
  summarise(
    deaths = n(),
    .groups = "drop"
  ) %>%
  full_join(phila_tracts_ages_sexes_years, by = c("GEOID", "years", "sex", "age")) %>%
  complete(GEOID, sex, years, age, fill = list(deaths = 0)) %>%
  filter(!is.na(years), !is.na(age), (GEOID %in% phila_tracts)) %>%
  arrange(sex, years, tract, age) %>%
  mutate(
    deaths = case_when(
      GEOID %in% low_pop ~ 0,
      TRUE ~ deaths
    )
  )

# Death counts by period (with unknown sex)
phila_death_count_nolow_unknown %>%
  group_by(years) %>%
  summarise(deaths = sum(deaths))

# Philly death counts by age group with low population census tracts removed
phila_death_counts_nolow_age_grp <- phila_death_counts_nolow %>%
  group_by(GEOID, age_grp, sex, years) %>%
  summarise(
    deaths = sum(deaths),
    .groups = "drop"
  )

save(
  phila_death_counts,
  phila_death_counts_nolow,
  phila_death_count_nolow_unknown,
  phila_death_counts_nolow_age_grp,
  file = here(dir_name, "Philly LE", paste0("phila_death_counts_", date, ".Rdata"))
)
