
# Load Libraries & Data ---------------------------------------------------

rm(list=ls())
library(tidyverse)

# Create variable to house directory paths
results <- "Results/"
working_results <- paste0(results, "Working Results/")
main_text <- paste0(results, "Main Text/")
appendix <- paste0(results, "Appendix/")
path_onedrive <- "C:/Users/ga437/OneDrive - Drexel University/Documents/UHC-LE-Philadelphia/Data - Cleansed/"
dir_name <- "//files.drexel.edu\\encrypted\\SOPH\\UHC\\SchnakeMahl_PAVitalStat\\Data\\"

# Load data created in 01-Crosswalks
load(file = paste0(path_onedrive,'sex_years.Rdata'))
load(file = paste0(path_onedrive, "low_pop_tracts.Rdata"))


# Mortality Rates ---------------------------------------------------------

# Load data data created in 02-Deaths and 03-Population
load(file = paste0(path_onedrive, "phila_pop_seer.rdata"))
load(paste0(dir_name, "Philly LE\\", "phila_death_counts.Rdata")); gc()

# Calculate mortality rates by tract, age, and gender
phila_death_rates_age_tract <- full_join(phila_death_counts_nolow, phila_pop_age_seer_all_fixes) %>%
  filter(!is.na(age)) %>%
  mutate(
    deaths = case_when(
      deaths > pop ~ floor(pop),
      TRUE ~ deaths),
    rate=deaths/pop, log_rate = log(rate)) %>%
  inner_join(sex_years) %>%
  arrange(GEOID, sex_years, age)

# Checks
phila_death_rates_age_tract %>% pull(GEOID) %>% unique %>% length # 384
sum((phila_death_rates_age_tract%>%filter(GEOID %in% low_pop))$deaths) # 0
sum((phila_death_rates_age_tract%>%filter(GEOID %in% low_pop))$pop) # 0
sum(phila_death_rates_age_tract$deaths) # 90,354
sum(phila_death_rates_age_tract$pop) # 9,511,239

# Philly mortality rate by age x sex x years with low population census tracts removed and adjusted for unexpected zeros
phila_death_rates_age <- phila_death_rates_age_tract %>%
  group_by(age, sex, years, sex_years) %>%
  summarise(pop=sum(pop, na.rm = T), deaths=sum(deaths, na.rm = T)) %>%
  mutate(rate=deaths/pop, log_rate=log(rate)) %>%
  inner_join(sex_years) %>%
  arrange(sex_years, age)

# Add smoothed log rates (log lambda star/theta star)
theta_star_male_2017_2019 <- data.frame(
  theta_star = loess(log_rate ~ age, data=filter(phila_death_rates_age, sex_years == 1), degree = 2, span = 0.25)$fitted,
  age = 0:85, sex_years = 1)

theta_star_male_2020_2022 <- data.frame(
  theta_star = loess(log_rate ~ age, data=filter(phila_death_rates_age, sex_years == 2), degree = 2, span = 0.25)$fitted,
  age = 0:85, sex_years = 2)

theta_star_female_2017_2019 <- data.frame(
  theta_star = loess(log_rate ~ age, data=filter(phila_death_rates_age, sex_years == 3), degree = 2, span = 0.25)$fitted,
  age = 0:85, sex_years = 3)

theta_star_female_2020_2022 <- data.frame(
  theta_star = loess(log_rate ~ age, data=filter(phila_death_rates_age, sex_years == 4), degree = 2, span = 0.25)$fitted,
  age = 0:85, sex_years = 4)

theta_star_df <- bind_rows(theta_star_male_2017_2019, theta_star_male_2020_2022, 
                           theta_star_female_2017_2019, theta_star_female_2020_2022)

phila_death_rates_age <- inner_join(phila_death_rates_age, theta_star_df)

sum(phila_death_rates_age$pop) # 9,511,239

# Save data (to encrypted drive)
save(phila_death_rates_age_tract, phila_death_rates_age, 
     file = paste0(dir_name, "Philly LE\\", "phila_death_rates.Rdata"))
