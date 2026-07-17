# Load Libraries & Data ---------------------------------------------------


rm(list = ls())
library(here)
library(tidyverse)
library(DemoTools)
library(sf)
library(data.table)

# Create variable to house directory paths
data <- here("Data")
results <- here("Results")
working_results <- here(results, "Working Results")
main_text <- here(results, "Main Text/")
appendix <- here(results, "Appendix/")
path_onedrive <- here("Data - Cleansed")

# Load relevant crosswalks
load(file = here(path_onedrive, "geoid_crosswalk.Rdata"))
load(file = here(path_onedrive, "phila_tracts_age_grp_sexes_years.Rdata"))
load(file = here(path_onedrive, "phila_tracts_ages_sexes_years.Rdata"))
load(file = here(path_onedrive, "age_grp.Rdata"))
load(file = here(path_onedrive, "phila_sf.Rdata"))
load(file = here(path_onedrive, "phila_sf_cross.Rdata"))
load(file = here(path_onedrive, "low_pop_tracts.Rdata"))

# All tracts in Philly
phila_tracts <- phila_sf$GEOID %>% unique()
N_tracts <- length(phila_tracts)

# Age limit for population
age_limit <- 85

# All ages to be included in analysis
ages <- seq(from = 0, to = age_limit, by = 1)
N_ages <- length(ages)
N_age_grp <- length(unique(phila_tracts_age_grp_sexes_years$age_grp))

# Number of periods in analysis
N_periods <- length(unique(phila_tracts_age_grp_sexes_years$years))

# Number of sex groups in analysis
N_sexes <- length(unique(phila_tracts_age_grp_sexes_years$sex))

# Date for file saving/loading
date <- "20260716"

# Load helper functions
source(here("Helpers", "print_message.R"))


# SEER Population --------------------------------------------------------------


cat("\nReading SEER population data...\n")

data_file <- here("Data", "us.2006_2023.tract.level.pops.txt")

t_start <- Sys.time()

# Read raw file
dt <- fread(data_file, header = FALSE, col.names = "raw")

# Parse GEOID and year first to filter
dt[, `:=`(
  GEOID = substr(raw, 7, 17),
  year  = as.integer(substr(raw, 1, 4))
)]

# Filter data
dt <- dt[
  GEOID %in% phila_tracts &
    year >= 2017 & year <= 2022
]

# Parse remaining fields
dt[, `:=`(
  sex = substr(raw, 19, 19),
  age = as.integer(substr(raw, 20, 21)),
  pop = as.numeric(substr(raw, 22, 30)) / 10000
)]

# Create years variable and make sex a string
dt[, years := fifelse(year <= 2019, "2017-2019", "2020-2022")]
dt[, sex := fifelse(sex == "1", "Men", "Women")]

# Create age-group lower bounds for graduation
dt[, `:=`(
  age_grp = fcase(
    age == 0, 0, # 0-1
    age == 1, 1, # 1-4
    age == 2, 5, # 5-9
    age == 3, 10, # 10-14
    age == 4, 15,
    age == 5, 20,
    age == 6, 25,
    age == 7, 30,
    age == 8, 35,
    age == 9, 40,
    age == 10, 45,
    age == 11, 50,
    age == 12, 55,
    age == 13, 60,
    age == 14, 65,
    age == 15, 70,
    age == 16, 75,
    age == 17, 80,
    age == 18, 85
  )
)]

# Group and summarise data
base_age_grp <- dt[, .(pop = sum(pop)),
  by = .(GEOID, age_grp, years, sex)
]

# Make sure we have all combinations of GEOID x age groups x years x sex
setDT(phila_tracts_age_grp_sexes_years)
setkey(base_age_grp, GEOID, age_grp, years, sex)
setkey(
  phila_tracts_age_grp_sexes_years,
  GEOID, age_grp, years, sex
)

phila_pop_age_grp <- base_age_grp[
  phila_tracts_age_grp_sexes_years
]

phila_pop_age_grp[is.na(pop), pop := 0]

t_end <- Sys.time()
elapsed_sec <- as.numeric(difftime(t_end, t_start, units = "secs"))

print_message(
  "Finished in sec" = elapsed_sec,
  "Finished in min" = elapsed_sec / 60,
  "Total pop" = phila_pop_age_grp[, sum(pop)]
)
# Finished in sec: 632.4657
# Finished in min: 10.5411
# Total pop: 9,495,886

# Free unused memory
gc()

# Check (should be TRUE)
nrow(phila_pop_age_grp) == N_tracts * N_age_grp * N_periods * N_sexes

# Total population in tracts to be excluded
print_message(
  "Total population in tracts to be excluded" =
    phila_pop_age_grp[
      GEOID %in% low_pop,
      sum(pop)
    ]
)
# Total population in tracts to be excluded: 53,354.81

# Zero out population from UHC-identified tracts
phila_pop_age_grp_no_low <- phila_pop_age_grp %>%
  mutate(
    pop = case_when(
      GEOID %in% low_pop ~ 0,
      TRUE ~ pop
    )
  )

print_message(
  "Total population by age group after removing tracts" =
    sum(phila_pop_age_grp_no_low$pop),
  "Total population in tracts removed" =
    sum(phila_pop_age_grp$pop) - sum(phila_pop_age_grp_no_low$pop),
  sep = "\n"
)
# Total population by age group after removing tracts: 9,442,531
# Total population in tracts removed: 53,354.81

# Population by age group, no population in UHC-identified tracts, non-structural zeros adjusted
phila_pop_age_grp_all_fixes <- phila_pop_age_grp_no_low %>%
  ungroup() %>%
  group_by(GEOID, years, sex) %>%
  mutate(
    ID = cur_group_id(),
    lag_pop = lag(pop, n = 1),
    lead_pop = lead(pop),
    sandwich_zero = (pop == 0) & (lag_pop != 0) & (lead_pop != 0),
    beginning_zero = is.na(lag_pop) & pop == 0 & lead_pop != 0,
    end_zero = is.na(lead_pop) & pop == 0 & lag_pop != 0
  ) %>%
  ungroup() %>%
  mutate(
    pop = case_when(
      sandwich_zero ~ (lag_pop + lead_pop) / 2,
      beginning_zero ~ lead_pop,
      end_zero ~ lag_pop,
      !sandwich_zero ~ pop,
      is.na(sandwich_zero) ~ pop
    )
  )

print_message(
  "Total population after adjustments" =
    sum(phila_pop_age_grp_all_fixes$pop),
  "Sandwich zeros" =
    phila_pop_age_grp_all_fixes %>%
      ungroup() %>%
      pull(sandwich_zero) %>%
      sum(na.rm = TRUE),
  "Added population from sandwich zero adjustments" =
    sum(phila_pop_age_grp_all_fixes$pop[which(phila_pop_age_grp_all_fixes$sandwich_zero)]),
  "% of total population" =
    sum(phila_pop_age_grp_all_fixes$pop[which(phila_pop_age_grp_all_fixes$sandwich_zero)]) / sum(phila_pop_age_grp_no_low$pop) * 100,
  "Beginning zeros" =
    phila_pop_age_grp_all_fixes %>%
      ungroup() %>%
      pull(beginning_zero) %>%
      sum(na.rm = TRUE),
  "Added population from beginning zero adjustments" =
    sum(phila_pop_age_grp_all_fixes$pop[which(phila_pop_age_grp_all_fixes$beginning_zero)]),
  "% of total population" =
    sum(phila_pop_age_grp_all_fixes$pop[which(phila_pop_age_grp_all_fixes$beginning_zero)]) / sum(phila_pop_age_grp_no_low$pop) * 100,
  "End zeros" =
    phila_pop_age_grp_all_fixes %>%
      ungroup() %>%
      pull(beginning_zero) %>%
      sum(na.rm = TRUE),
  "Added population from end zero adjustments" =
    sum(phila_pop_age_grp_all_fixes$pop[which(phila_pop_age_grp_all_fixes$end_zero)]),
  "% of total population" =
    sum(phila_pop_age_grp_all_fixes$pop[which(phila_pop_age_grp_all_fixes$end_zero)]) / sum(phila_pop_age_grp_no_low$pop) * 100,
  "Additional population from adjustments" =
    sum(phila_pop_age_grp_all_fixes$pop) - sum(phila_pop_age_grp_no_low$pop),
  "Population in removed tracts" =
    sum((phila_pop_age_grp_all_fixes %>% filter(GEOID %in% low_pop))$pop),
  sep = "\n"
)
# Total population after adjustments: 9,442,990
# Sandwich zeros: 12
# Added population from sandwich zero adjustments: 396.5931
# % of total population: 0.004200072
# Beginning zeros: 2
# Added population from beginning zero adjustments: 6.093
# % of total population: 0.00006452719
# End zeros: 2
# Added population from end zero adjustments: 55.9311
# % of total population: 0.0005923316
# Additional population from adjustments: 458.6172
# Population in removed tracts: 0


## Single year age groups --------------------------------------------------


# Function for graduating population by age group
phila_pop_age_sex <- function(df) { # keep0
  df %>%
    group_by(GEOID, sex, years) %>%
    group_modify(~ {
      current_sex <- .y$sex
      current_period <- .y$years
      age_grp <- .x$age_grp

      t1 <- graduate(
        Value = .x$pop,
        Age = age_grp,
        method = "pclm",
        OAnew = 120,
        keep0 = TRUE,
        constrain = TRUE
      )

      data.frame(
        age = as.numeric(names(t1)),
        pop = t1,
        years = current_period,
        sex = current_sex
      ) %>%
        group_by(age) %>%
        summarise(
          pop = sum(pop),
          .groups = "drop"
        )
    }) %>%
    ungroup()
}

# Population by single-year age group
phila_pop_age <- phila_pop_age_grp %>%
  phila_pop_age_sex() %>%
  mutate(
    age = case_when(
      age >= age_limit ~ age_limit,
      TRUE ~ age
    )
  ) %>%
  complete(GEOID, sex, years, age, fill = list(pop = 0)) %>%
  filter(!is.na(pop)) %>% #
  group_by(GEOID, sex, years, age) %>%
  summarise(
    pop = sum(pop),
    .groups = "drop"
  )

print_message(
  "Total population (post-graduation single-year age groups)" =
    sum(phila_pop_age$pop),
  "Total population (age groups)" =
    sum(phila_pop_age_grp$pop),
  "Added population from graduation" =
    sum(phila_pop_age$pop) - sum(phila_pop_age_grp$pop),
  sep = "\n"
)
# Total population (post-graduation single-year age groups): 9,495,886
# Total population (age groups): 9,495,886
# Added population from graduation: 0

# Checks (should be TRUE)
nrow(phila_pop_age) == N_tracts * N_ages * N_periods * N_sexes

# Zero out population from UHC-identified tracts
phila_pop_age_no_low <- phila_pop_age %>%
  mutate(
    pop = case_when(
      GEOID %in% low_pop ~ 0,
      TRUE ~ pop
    )
  )

print_message(
  "Total population (post-graduation single-year age groups with tracts removed)" =
    sum(phila_pop_age_no_low$pop),
  "Population removed from removed tracts" =
    sum(phila_pop_age$pop) - sum(phila_pop_age_no_low$pop),
  sep = "\n"
)
# Total population (post-graduation single-year age groups with tracts removed): 9,442,531
# Population removed from removed tracts: 53,354.81

# Checks
nrow(phila_pop_age_no_low) == N_tracts * N_ages * N_periods * N_sexes

# Graduate adjusted age group population to get adjusted population by single-year age group
phila_pop_age_all_fixes <- phila_pop_age_grp_all_fixes %>%
  phila_pop_age_sex() %>%
  mutate(
    age = case_when(
      age >= age_limit ~ age_limit,
      TRUE ~ age
    ),
    pop = case_when(
      GEOID %in% low_pop ~ 0,
      TRUE ~ pop
    )
  ) %>%
  complete(GEOID, sex, years, age, fill = list(pop = 0)) %>%
  filter(!is.na(pop)) %>%
  group_by(GEOID, years, sex, age) %>%
  summarise(
    pop = sum(pop),
    .groups = "drop"
  ) %>%
  arrange(GEOID, sex, age)

print_message(
  "Total population (single-year age groups with all adjustments)" =
    sum(phila_pop_age_all_fixes$pop),
  "Added population from graduation" =
    sum(phila_pop_age_all_fixes$pop) - sum(phila_pop_age_grp_all_fixes$pop),
  sep = "\n"
)
# Total population (single-year age groups with all adjustments): 9,442,990
# Added population from graduation: 0


## Total population --------------------------------------------------------


# Total SEER population by census tract x sex x years after adjustments
phila_pop_tract_sex_total_all_fixes <- phila_pop_age_all_fixes %>%
  group_by(GEOID, years, sex) %>%
  summarise(
    pop = sum(pop),
    .groups = "drop"
  ) %>%
  arrange(years, sex, GEOID)

print_message(
  "Total population" =
    sum(phila_pop_tract_sex_total_all_fixes$pop),
  sep = "\n"
)
# Total population: 9,442,990

# Total SEER population by census tract x years after adjustments
phila_pop_total_all_fixes <- phila_pop_age_grp_all_fixes %>%
  group_by(GEOID, years) %>%
  summarise(
    pop = sum(pop),
    .groups = "drop"
  ) %>%
  inner_join(phila_sf_cross) %>%
  arrange(years, tract)

print_message(
  "Total population" =
    sum(phila_pop_total_all_fixes$pop),
  sep = "\n"
)
# Total population: 9,442,990

phila_pop <- list(
  pop_age_grp = phila_pop_age_grp,
  pop_age_grp_no_low = phila_pop_age_grp_no_low,
  pop_age_grp_all_fixes = phila_pop_age_grp_all_fixes,
  pop_age = phila_pop_age,
  pop_age_no_low = phila_pop_age_no_low,
  pop_age_all_fixes = phila_pop_age_all_fixes,
  pop_tract_sex_total_all_fixes = phila_pop_tract_sex_total_all_fixes,
  pop_total_all_fixes = phila_pop_total_all_fixes
)

save(
  phila_pop,
  file = here(path_onedrive, paste0("phila_pop_", date, ".Rdata"))
)
