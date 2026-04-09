
# Load Libraries & Data ---------------------------------------------------

rm(list=ls())
library(here)
library(tidyverse)
library(DemoTools)
library(readr)
library(sf)

# Create variable to house directory paths
data <- here("Data")
results <- here("Results")
working_results <- here(results, "Working Results")
main_text <- here(results, "Main Text/")
appendix <- here(results, "Appendix/")
path_onedrive <- here("Data - Cleansed")

# Load relevant crosswalks
load(file = here(path_onedrive, "geoid_crosswalk.Rdata"))
load(file = here(path_onedrive,'phila_tracts_age_grp_5_sexes_years.Rdata'))
load(file = here(path_onedrive,'phila_tracts_ages_sexes_years.Rdata'))
load(file = here(path_onedrive, "phila_sf.Rdata"))
load(file = here(path_onedrive, "phila_sf_cross.Rdata"))
load(file = here(path_onedrive, "low_pop_tracts.Rdata"))

# All tracts in Philly
phila_tracts <- phila_sf$GEOID %>% unique
N_tracts <- length(phila_tracts)

# Age limit for population
age_limit<-85

# All ages to be included in analysis
ages <- seq(from = 0, to = age_limit, by = 1)


# SEER Population --------------------------------------------------------------

# Function for graduating population by age group
phila_pop_age_sex <- function(df) {
  x <- df %>%
    group_by(GEOID, sex, years) %>%
    group_modify(~{
      current_sex <- .y$sex 
      current_period <- .y$years
      #print(.x)
      t1 <- graduate(Value = .x$pop, Age = .x$age_grp_5, method = "pclm", OAnew = 120)
      temp_df <- data.frame(age = as.numeric(names(t1)), pop = t1, years = current_period, sex = current_sex)
      temp_df <- temp_df %>%
        group_by(age) %>%
        summarise(pop = sum(pop))
      temp_df
    })
  return(x)
}

# Load SEER population estimates 
# Replace with your own SEER URL after requesting access for data at https://seer.cancer.gov/censustract-pops/request.html
seer_url <- ""

seer_destfile <- here(data, "seer_tract_pop.txt.gz")

# Only attempt download if a non-empty URL is provided; otherwise rely on an existing local file
if (nzchar(seer_url)) {
  download.file(seer_url, destfile = seer_destfile, mode = "wb")
} else if (!file.exists(seer_destfile)) {
  stop(
    "SEER population file not found at '", seer_destfile,
    "'. Please request access at https://seer.cancer.gov/censustract-pops/request.html, ",
    "set 'seer_url' to the download URL, or manually place the file at this path."
  )
}

# SEER population by sex x tract x age group
phila_pop_age_grp_seer <- read_lines(here(data, "seer_tract_pop.txt.gz")) %>% # Old: us.2006_2022.tract.level.pops.txt
  enframe(name = NULL, value = "data") %>%
  mutate(state = substr(data, 5, 6),
         stateFIPS = substr(data, 7, 8),
         countyFIPS = substr(data, 9, 11),
         tract = substr(data, 12, 17),
         GEOID = as.numeric(paste0(stateFIPS, countyFIPS, tract))) %>%
  filter(GEOID %in% phila_tracts) %>%
  mutate(
    year = as.integer(substr(data, 1, 4)),
    years = case_when(
      year >= 2017 & year <= 2019 ~ "2017-2019",
      year >= 2020 & year <= 2022 ~ "2020-2022"
    ),
    sex = substr(data, 19,19),
    age = as.numeric(substr(data, 20, 21)),
    pop = as.numeric(substr(data, 22, 30))/10000) %>%
  filter(year >= 2017 & year <= 2022) %>%
  select(GEOID, years, sex, age, pop) %>%
  mutate(
    sex = case_when(
      sex == 1 ~ "Men",
      sex == 2 ~ "Women") %>% as.factor,
    age_grp_5 = case_when(
      age == 0 ~ 0,
      age == 1 ~ 0,
      age == 2 ~ 5,
      age == 3 ~ 10,
      age == 4 ~ 15,
      age == 5 ~ 20,
      age == 6 ~ 25,
      age == 7 ~ 30,
      age == 8 ~ 35,
      age == 9 ~ 40,
      age == 10 ~ 45,
      age == 11 ~ 50,
      age == 12 ~ 55,
      age == 13 ~ 60,
      age == 14 ~ 65,
      age == 15 ~ 70,
      age == 16 ~ 75,
      age == 17 ~ 80,
      age == 18 ~ 85
    )) %>%
  select(-age) %>%
  full_join(phila_tracts_age_grp_5_sexes_years) %>%
  complete(GEOID, age_grp_5, years, sex, fill = list(pop = 0)) %>%
  filter(!is.na(pop), !is.na(age_grp_5)) %>%
  group_by(GEOID, age_grp_5, years, sex) %>%
  summarise(pop = sum(pop)); gc()

#Checks
phila_pop_age_grp_seer$GEOID %>% unique %>% length # 384
nrow(phila_pop_age_grp_seer) # 27,648
N_tracts*18*2*2 # 27,648

# Total SEER population by age group
sum(phila_pop_age_grp_seer$pop) # Old: 9,565,508, New: 9,495,886

# Total SEER population in tracts to be excluded
sum((phila_pop_age_grp_seer %>% filter(GEOID %in% low_pop))$pop) # Old: 54,689.67, New: 53,354.81

# Total SEER population by age group in tracts to be included
sum(phila_pop_age_grp_seer$pop) - sum((phila_pop_age_grp_seer %>% filter(GEOID %in% low_pop))$pop) # Old: 9,510,818, New: 9,442,531

# SEER population by single-year age group
phila_pop_age_seer <- phila_pop_age_grp_seer %>%
  phila_pop_age_sex() %>%
  mutate(age = case_when(
    age >= age_limit ~ age_limit,
    T ~ age
  )) %>%
  ungroup() %>%
  full_join(phila_tracts_ages_sexes_years) %>%
  complete(GEOID, age, years, sex, fill = list(pop = 0)) %>%
  filter(!is.na(pop), !is.na(age_grp_5)) %>%
  group_by(GEOID, sex, years, age) %>%
  summarise(pop = sum(pop))

# Total SEER population by single-year age group
sum(phila_pop_age_seer$pop) # Old: 9,565,530, New: 9,495,973

# Checks
phila_pop_age_seer$GEOID %>% unique %>% length # 384
nrow(phila_pop_age_seer) # 132,096
N_tracts*86*2*2 # 132,096

# Difference between total SEER population by age group and single-year age group
sum(phila_pop_age_grp_seer$pop) - sum(phila_pop_age_seer$pop) # Old: -21.88294, New: -86.64896

# Total population by census tract x years
phila_pop_seer_total <- phila_pop_age_grp_seer %>%
  group_by(GEOID, years) %>%
  summarise(pop=sum(pop)) %>%
  inner_join(phila_sf_cross) %>%
  arrange(years, tract)
sum(phila_pop_seer_total$pop) # Old: 9,565,508, New: 9,495,886

# Zero out population from UHC-identified tracts
phila_pop_seer_age_grp_no_low <- phila_pop_age_grp_seer %>%
  mutate(
    pop = case_when(GEOID %in% low_pop ~ 0,
      TRUE ~ pop))

# Total SEER population by age group after zeroing out low-population tracts
sum(phila_pop_seer_age_grp_no_low$pop) # Old: 9,510,818, New: 9,442,531

# How many people have we zeroed out?
sum(phila_pop_age_grp_seer$pop) - sum(phila_pop_seer_age_grp_no_low$pop) # Old: 54,689.67, New: 53,354.81

# SEER population by age group, no population in UHC-identified tracts, 
# non-structural zeros adjusted
phila_pop_age_grp_seer_all_fixes <-  phila_pop_seer_age_grp_no_low %>%
  ungroup() %>%
  group_by(GEOID, years, sex) %>%
  mutate(ID=cur_group_id(),
         lag_pop = lag(pop, n=1), 
         lead_pop = lead(pop),
         sandwich_zero = (pop == 0) & (lag_pop != 0) & (lead_pop != 0),
         beginning_zero = is.na(lag_pop) & pop == 0 & lead_pop != 0,
         end_zero = is.na(lead_pop) & pop == 0 & lag_pop != 0) %>%
  ungroup() %>%
  mutate(
    pop = case_when(
      sandwich_zero ~ (lag_pop+lead_pop)/2,
      !sandwich_zero ~ pop,
      is.na(sandwich_zero) ~ pop)) %>%
  mutate(pop = case_when(
    is.na(lead_pop) & pop == 0 & lag_pop != 0 ~ lag_pop,
    TRUE ~ pop))

# How many ns zeros were there?
phila_pop_seer_age_grp_no_low %>%
  ungroup() %>%
  group_by(GEOID, years, sex) %>%
  mutate(ID=cur_group_id(),
         lag_pop = lag(pop, n=1), 
         lead_pop = lead(pop),
         sandwich_zero = (pop == 0) & (lag_pop != 0) & (lead_pop != 0)) %>%
  pull(sandwich_zero) %>% 
  sum(na.rm = TRUE)

# Total SEER population by age group after adjustments
sum(phila_pop_age_grp_seer_all_fixes$pop) # Old: 9,511,225, New: 9,442,978

# How many additional people have we added?
sum(phila_pop_age_grp_seer_all_fixes$pop) - sum(phila_pop_seer_age_grp_no_low$pop) # Old: 406.5494, New: 446.3977

# Do we have any people in UHC-identified tracts?
sum((phila_pop_age_grp_seer_all_fixes%>%filter(GEOID %in% low_pop))$pop) # Old: 0, New: 0

# Total SEER population by census tract x years after adjustments
phila_pop_seer_total_all_fixes <- phila_pop_age_grp_seer_all_fixes %>%
  group_by(GEOID, years) %>%
  summarise(pop=sum(pop)) %>%
  inner_join(phila_sf_cross) %>%
  arrange(years, tract)
sum(phila_pop_seer_total_all_fixes$pop) # Old: 9,511,225, New: 9,442,978

# Graduate adjusted age group population to get adjusted population by single-year age group
phila_pop_age_seer_all_fixes <- phila_pop_age_grp_seer_all_fixes %>%
  phila_pop_age_sex() %>%
  mutate(
    age = case_when(
      age >= age_limit ~ age_limit,
      T ~ age),
    pop = case_when(GEOID %in% low_pop ~ 0,
      TRUE ~ pop)) %>%
  full_join(phila_tracts_ages_sexes_years, by=c("GEOID", "years", "sex", "age")) %>%
  ungroup() %>%
  complete(GEOID, age, years, sex, fill = list(pop = 0)) %>%
  filter(!is.na(pop), !is.na(age_grp_5)) %>%  
  group_by(GEOID, years, sex, age) %>%
  summarise(pop = sum(pop)) %>%
  arrange(GEOID, sex, age)

# Checks
phila_pop_age_seer_all_fixes$GEOID %>% unique %>% length # 384
nrow(phila_pop_age_seer_all_fixes) # 132,096
N_tracts*2*2*86 # 132,096

# Total single-year age group population after adjustments
sum(phila_pop_age_seer_all_fixes$pop) # Old: 9,511,239, New: 9,443,057

# Total age group population after adjustments
sum(phila_pop_age_grp_seer_all_fixes$pop) # Old: 9,511,225, New: 9,442,978

# How many additional people have we added?
sum(phila_pop_age_seer_all_fixes$pop) - sum(phila_pop_age_grp_seer_all_fixes$pop) # Old: 14.16606, New: 79.15564

# Total SEER population by census tract x sex x years after adjustments
phila_pop_seer_tract_sex_total_all_fixes <- phila_pop_age_seer_all_fixes %>%
  group_by(GEOID, years, sex) %>%
  summarise(pop=sum(pop)) %>%
  arrange(years, sex, GEOID)

# Total SEER population by census tract x years after adjustments
# We can use this to find out which tracts have no population data
phila_pop_seer_total_all_fixes <- phila_pop_age_seer_all_fixes %>%
  group_by(GEOID, years) %>%
  summarise(pop=sum(pop)) %>%
  mutate(no_pop = pop < 1,
         no_pop_type = case_when(
           GEOID %in% low_pop ~ "No Population - Expected",
           no_pop & !(GEOID %in% low_pop) ~ "No Population - Unexpected",
           TRUE ~ "Populated"))

# Map areas with no population
phila_pop_seer_no_pop_maps <- 
  st_as_sf(as.data.frame(phila_sf)) %>% 
  inner_join(phila_pop_seer_total_all_fixes, by = "GEOID") %>%
  ggplot(aes(fill = no_pop_type)) +
  geom_sf() +
  scale_fill_manual(values = c(
    "No Population - Expected" = "orange",
    "No Population - Unexpected" = "red",
    "Populated" = "white"
  )) +
  facet_wrap(~years) +
  theme_void() +
  theme(legend.position = "bottom")
phila_pop_seer_no_pop_maps

# Save map
ggsave(filename = here(working_results, "phila_pop_seer_no_pop_maps.png"), 
       plot = phila_pop_seer_no_pop_maps, 
       device = "png", 
       width = 15, 
       height = 13)

# Save cleaned up population data
save(phila_pop_age_grp_seer, phila_pop_age_seer, 
     phila_pop_seer_total, phila_pop_seer_age_grp_no_low, 
     phila_pop_age_grp_seer_all_fixes, phila_pop_seer_total_all_fixes,
     phila_pop_age_seer_all_fixes, phila_pop_seer_tract_sex_total_all_fixes, 
     phila_pop_seer_total_all_fixes,
     file = here(path_onedrive, "phila_pop_seer.rdata"))
