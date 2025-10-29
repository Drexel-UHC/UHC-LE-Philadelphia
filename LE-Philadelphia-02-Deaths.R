
# Load Libraries & Data ---------------------------------------------------

rm(list=ls())
library(tidyverse)
library(diagis)

# Create variable to house directory paths
dir_name <- "//files.drexel.edu\\encrypted\\SOPH\\UHC\\SchnakeMahl_PAVitalStat\\Data\\"
results <- "Results/"
working_results <- paste0(results, "Working Results/")
main_text <- paste0(results, "Main Text/")
appendix <- paste0(results, "Appendix/")
path_onedrive <- "Data - Cleansed/"

# Load data created in crosswalks script
load(file = paste0(path_onedrive, "phila_sf.Rdata"))
load(file = paste0(path_onedrive, "low_pop_tracts.Rdata"))
load(file = paste0(path_onedrive, "years_cross.Rdata"))
load(file = paste0(path_onedrive, "age_grp_5.Rdata"))
load(file = paste0(path_onedrive,'phila_tracts_ages_sexes_years.Rdata'))

# All tracts in Philly
phila_tracts <- unique(phila_sf$GEOID)
N_tracts <- length(phila_tracts)

# Age limit for age at death
age_limit <- 85


# Death Data --------------------------------------------------------------

# Columns we need for death files
keep_death <- c("FILENO", "SEX", "DOB", "GEOID10", "geoid10",
                "year", "Age_death", "DOD", "race1215", "RACE1215", 
                "race9011", "HISPANIC")

# Create dataframe of Pennsylvania deaths
deaths <- data.frame()

for(i in 2017:2022) {
  current_data <- read_csv(paste0(dir_name, "deathgeo", i, ".csv")) %>%
    select(one_of(keep_death)) %>%
    mutate(year = i)
  
  # Check the format of DOD and reformat accordingly
  if(is.integer(current_data$DOD)) {
    # For integer-formatted dates (mmddyyyy)
    current_data <- current_data %>%
      mutate(DOD = paste0(substr(as.character(DOD), 1, 2), "/",    # mm/
                          substr(as.character(DOD), 3, 4), "/",    # dd/
                          substr(as.character(DOD), 5, 8))) %>%
      rename("race1215" = "RACE1215")
  }
  
  # Check and rename columns if they exist
  if("GEOID10" %in% colnames(current_data)) {
    current_data <- current_data %>% rename(GEOID = GEOID10)
  }
  else if("geoid10" %in% colnames(current_data)) {
    current_data <- current_data %>% rename(GEOID = geoid10)
  }
  deaths <- bind_rows(deaths, current_data)
}

# Determine what % of all deaths could not be geocoded (2017-2019)
nrow(filter(deaths, year <= 2019, is.na(GEOID) | nchar(as.numeric(GEOID)) < 11)) / nrow(filter(deaths, year <= 2019)) * 100

# Determine what % of all deaths could not be geocoded (2020-2022)
nrow(filter(deaths, year >= 2020, is.na(GEOID) | nchar(as.numeric(GEOID)) < 11)) / nrow(filter(deaths, year >= 2020)) * 100

#Determine what % of all deaths could not be geocoded
deaths_nogeo <- nrow(filter(deaths, is.na(GEOID) | nchar(as.numeric(GEOID)) < 11))
deaths_total <- nrow(deaths)
deaths_nogeo/deaths_total*100

# For demographic information on race-ethnicity
death_demographics <- function(df) {
  x <- df %>%
    mutate(
      sex = case_when(
        SEX == 1 ~ "Male",
        SEX == 2 ~ "Female",
        SEX == 9 ~ "Unknown"
      ),
      RACE = case_when(
        RACE1215 == 01 ~ 1, #White race9011 == 1 (for older years)
        RACE1215 == 02 ~ 2, #Black race9011 == 2 | (for older years)
        RACE1215 == 03 ~ 3, #American Indian Alaska Native race9011 == 3 | (for older years)
        RACE1215 >= 04 & RACE1215 <= 14 ~ 4, #AAPI ((race9011 >= 4 & race9011 <= 8) | race9011 == 0) | (for older years)
        is.na(RACE1215) | RACE1215>= 15 ~ 5) %>% #Other race9011 == 9 | is.na(race9011) | (for older years)
        factor(
          levels = c(1,2,3,4,5),
          labels = c("White", "Black", 
                     "American Indian, Alaska Native", "Asian American Pacific Islander", "Other")),
      HISP = case_when(
        HISPANIC == 1 ~ 1, #Non-Hispanic
        HISPANIC >= 2 & HISPANIC <= 5 ~ 2, #Hispanic
        HISPANIC == 9 ~ 3), #Unknown
      HISP = factor(HISP, labels = c("Non-Hispanic", "Hispanic", "Unknown")),
      RACEHISP = case_when(
        (as.integer(HISP) == 2) ~ 1, #Hispanic
        (as.integer(HISP) == 1) & (as.integer(RACE) == 1) ~ 2, #Non-Hispanic White
        (as.integer(HISP) == 1) & (as.integer(RACE) == 2) ~ 3, #Non-Hispanic Black
        (as.integer(HISP) == 1) & (as.integer(RACE) == 3) ~ 4, #Non-Hispanic American Indian Alaska Native
        (as.integer(HISP) == 1) & (as.integer(RACE) == 4) ~ 5, #Non-Hispanic AAPI
        (as.integer(HISP) == 1) & (as.integer(RACE) == 5) ~ 6, #Non-Hispanic Other
        (as.integer(HISP) == 3) ~ 7) %>% #Unknown
        factor(
          levels = c(1,2,3,4,5,6,7),
          labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", 
                     "Non-Hispanic American Indian, Alaska Native",
                     "Non-Hispanic Asian American Pacific Islander", "Non-Hispanic Other", "Unknown")),
      PHILA = case_when(
        substr(GEOID, 1, 5) == 42101 ~ TRUE,
        TRUE ~ FALSE),
      GEOCODED = case_when(
        is.na(GEOID) | nchar(as.numeric(GEOID)) < 10 ~ "Non-Geocoded",
        TRUE ~ "Geocoded"))
}

# Modified dataframe of PA death data with demographic information
deaths_modified <- death_demographics(deaths)

# Determine what % of deaths were missing sex (2017-2019)
deaths_modified %>% filter(year <= 2019, SEX == 9) %>% nrow() / nrow(deaths_modified) * 100

# Determine what % of deaths were missing age at death (2017-2019)
deaths_modified %>% filter(year <= 2019, is.na(Age_death)) %>% nrow() / nrow(deaths_modified) * 100

# Determine what % of deaths in Philly were not fully geocoded (2017-2019) (GEOID starts with 42101 but missing other info)
deaths_modified %>% 
  filter(year <= 2019, substr(GEOID, 1, 5) == 42101, nchar(as.numeric(GEOID)) < 11) %>% 
  nrow() / nrow(deaths_modified %>% filter(year <= 2019, substr(GEOID, 1, 5) == 42101)) * 100

# Death by years
pa_death_counts_by_years <- deaths_modified %>%
  inner_join(years_cross) %>%
  group_by(years) %>%
  summarise(deaths=n()) %>%
  mutate(total=sum(deaths), pct=round(deaths/total*100,2),
         deaths_pct=paste0(format(deaths, big.mark=",",scientific=FALSE), " (", pct, "%)")) %>%
  pivot_wider(names_from = years, values_from = deaths_pct, values_fill = "") %>%
  select(-c(deaths, total, pct)) %>%
  summarise("2017-2019"=paste(`2017-2019`, collapse = ""), "2020-2022"=paste(`2020-2022`, collapse = ""))

# Calculate number of deaths in PA
pa_death_count_summary <- deaths_modified %>%
  inner_join(years_cross) %>%
  group_by(years, sex) %>%
  summarise(deaths=n()) %>%
  group_by(years) %>%
  mutate(total=sum(deaths), pct=round(deaths/total*100,2),
         deaths_chr=format(deaths, big.mark=",",scientific=FALSE),
         sex=paste0("    ", sex)) %>%
  bind_rows(
    deaths_modified %>%
      inner_join(years_cross) %>%
      group_by(years) %>%
      summarise(deaths=n(), total = sum(deaths)) %>%
      mutate(sex="  Total", pct=round(deaths/total*100,2),
             deaths_chr=format(deaths, big.mark=",",scientific=FALSE))
  ) %>%
  mutate(deaths_pct = paste0(deaths_chr, " (", pct, "%)")) %>%
  pivot_wider(names_from = years, values_from = deaths_pct, values_fill = "") %>%
  select(-c(deaths, pct)) %>%
  group_by(sex) %>%
  summarise("PA - 2017-2019"=paste(`2017-2019`, collapse = ""), "PA - 2020-2022"=paste(`2020-2022`, collapse = "")) %>%
  bind_rows(c(sex = "Number of deaths (%)", "PA - 2017-2019" = "", "PA - 2020-2022" = ""),
            c(sex = "  Sex", "PA - 2017-2019" = "", "PA - 2020-2022" = "")) %>%
  rename("Measure" = sex) %>%
  mutate(Measure = factor(Measure, 
                          levels = c("Number of deaths (%)", "  Total", "  Sex", "    Female", "    Male", "    Unknown"))) %>%
  arrange(Measure)

# Save to CSV to use for Table 1
write.csv(pa_death_count_summary, file = paste0(working_results,"pa_death_count_summary.csv"), row.names = F)

# Philadelphia Deaths -----------------------------------------------------

# Philly deaths by tract x sex x age group
phila_deaths <- deaths %>%
  filter(GEOID %in% phila_tracts) %>%
  mutate(
    years = case_when(
      year >= 2017 & year <= 2019 ~ "2017-2019",
      year >= 2020 & year <= 2022 ~ "2020-2022"),
    sex = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      SEX == 9 ~ "Unknown"),
    age = case_when(
      floor(Age_death) >= age_limit ~ age_limit,
      floor(Age_death) < age_limit ~ floor(Age_death)),
    GEOID = as.numeric(GEOID)) %>%
  inner_join(age_grp_5)

# What % of deaths were from low population/special land use tracts?
nrow(filter(phila_deaths, GEOID %in% low_pop)) / nrow(phila_deaths) * 100

# Philadelphia deaths with low population census tracts removed
phila_deaths_nolow <- phila_deaths %>%
  filter(!(GEOID %in% low_pop))
nrow(phila_deaths_nolow)

# Save data (to encrypted drive)
save(phila_deaths, 
     phila_deaths_nolow, 
     file = paste0(dir_name, "Philly LE\\", "phila_deaths.Rdata"))

# Philadelphia deaths with demographic information
phila_deaths_modified <- deaths_modified %>%
  filter(GEOID %in% phila_tracts) %>%
  mutate(
    years = case_when(
      year >= 2017 & year <= 2019 ~ "2017-2019",
      year >= 2020 & year <= 2022 ~ "2020-2022"),
    age = case_when(
      floor(Age_death) >= age_limit ~ age_limit,
      floor(Age_death) < age_limit ~ floor(Age_death)),
    GEOID = as.numeric(GEOID)) %>%
  inner_join(age_grp_5)

# Philadelphia deaths with demographic information - low population/special land use tracts removed
phila_deaths_nolow_modified <- phila_deaths_modified %>%
  filter(!(GEOID %in% low_pop))

# How many entries were removed?
nrow(phila_deaths) - nrow(phila_deaths_nolow) # 302

# Determine % of final sample missing sex information
nrow(filter(phila_deaths_nolow_modified, SEX == 9)) # 4
nrow(phila_deaths_nolow_modified) # 90,399
nrow(filter(phila_deaths_nolow_modified, SEX == 9)) / nrow(phila_deaths_nolow_modified) * 100 # 0.004424828%

# Determine % of final sample missing age at death information
nrow(filter(phila_deaths_nolow_modified, is.na(Age_death))) # 8
nrow(phila_deaths_nolow_modified) # 90,399
nrow(filter(phila_deaths_nolow_modified, is.na(Age_death))) / nrow(phila_deaths_nolow_modified) * 100 # 0.008849655%

# Determine % of final sample missing age at death information (same as above, but with age variable)
nrow(filter(phila_deaths_nolow_modified, is.na(age))) # 8
nrow(phila_deaths_nolow_modified) # 90,399
nrow(filter(phila_deaths_nolow_modified, is.na(age))) / nrow(phila_deaths_nolow_modified) * 100 # 0.008849655%

# Summary info for age at death
summary(phila_deaths_nolow$Age_death)

# Philly death counts - exclude entries missing information for sex and age
phila_death_counts <- phila_deaths %>%
  filter(SEX != 9, !is.na(age)) %>%
  mutate(sex = as.factor(sex)) %>%
  group_by(GEOID, sex, years, age) %>%
  summarise(deaths = n()) %>%
  ungroup() %>%
  full_join(phila_tracts_ages_sexes_years, by=c("GEOID", "years", "sex", "age")) %>%
  complete(GEOID, sex, years, age, fill = list(deaths = 0)) %>%
  filter(sex %in% c("Male", "Female"), !is.na(years), !is.na(age), GEOID %in% phila_tracts) %>%
  arrange(sex, years, tract, age)
sum(phila_death_counts$deaths) # 90,688

# Checks 
phila_death_counts$GEOID %>% unique %>% length # 384
nrow(phila_death_counts) # 132,096
N_tracts*2*2*86

# What % of data has 0 deaths?
nrow(filter(phila_death_counts, deaths == 0))/nrow(phila_death_counts)*100 # 67.96572

# FINAL SAMPLE - Philly death counts by age with low population/special land use census tracts removed
phila_death_counts_nolow <- phila_death_counts %>% 
  mutate(deaths = case_when(
    GEOID %in% low_pop ~ 0,
    TRUE ~ deaths))

# Checks
phila_death_counts_nolow$GEOID %>% unique %>% length # 384
nrow(phila_death_counts_nolow) # 132,096
N_tracts*2*2*86 # 132,096

sum(phila_death_counts$deaths) # 90,688 before removing deaths from low population tracts
sum(phila_death_counts_nolow$deaths) # 90,387 after removing deaths from low population 
sum(phila_death_counts$deaths) - sum(phila_death_counts_nolow$deaths) # 301 records removed
sum((phila_death_counts_nolow%>%filter(GEOID %in% low_pop))$deaths) # 0 deaths in tracts to be excluded

# Check to make sure 301 removed is correct
nrow(filter(phila_deaths_modified, !is.na(age), SEX != 9 , GEOID %in% low_pop)) # 301

# Death counts by year in final sample
phila_death_counts_nolow %>%
  group_by(years) %>%
  summarise(deaths = sum(deaths))

# Philly death counts by age with low population census tracts removed - includes unknown
phila_death_count_nolow_unknown <- phila_deaths %>%
  filter(!is.na(age)) %>%
  mutate(sex = as.factor(sex)) %>%
  group_by(GEOID, sex, years, age) %>%
  summarise(deaths = n()) %>%
  ungroup() %>%
  full_join(phila_tracts_ages_sexes_years, by=c("GEOID", "years", "sex", "age")) %>%
  complete(GEOID, sex, years, age, fill = list(deaths = 0)) %>%
  filter(!is.na(years), !is.na(age), (GEOID %in% phila_tracts)) %>%
  arrange(sex, years, GEOID, age) %>% 
  mutate(deaths = case_when(
    GEOID %in% low_pop ~ 0,
    TRUE ~ deaths))

sum(phila_death_count_nolow_unknown$deaths) # 90,391 after adding back unknown sex -- 4 additional

# Philly death counts by age group with low population census tracts removed
phila_death_counts_nolow_age_grp <- phila_death_counts_nolow %>% 
  group_by(GEOID, age_grp_5, sex, years) %>% 
  summarise(deaths = sum(deaths))
length(unique(phila_death_counts_nolow_age_grp$GEOID)) # 384
sum((phila_death_counts_nolow_age_grp%>%filter(GEOID %in% low_pop))$deaths) # 0

#Determine what % of philadelphia deaths could not be geocoded
philadelphia_nogeo <- nrow(filter(phila_deaths, is.na(GEOID) | nchar(as.numeric(GEOID)) < 11))
philadelphia_total <- nrow(phila_deaths)
philadelphia_nogeo_pct <- philadelphia_nogeo/philadelphia_total*100
philadelphia_geo_pct <- 100 - philadelphia_nogeo_pct

# Save data (to encrypted drive)
save(phila_death_counts, 
     phila_death_counts_nolow, 
     phila_death_count_nolow_unknown,
     phila_death_counts_nolow_age_grp, 
     file = paste0(dir_name, "Philly LE\\", "phila_death_counts.Rdata"))
