
# Load Libraries & Data ---------------------------------------------------

rm(list=ls())
library(tidyverse)

# Onedirve Paths (replace with your own folder)
# PATH FOR GIANNI'S COMPUTER:
path_onedrive <- "C:/Users/ga437/OneDrive - Drexel University/Documents/UHC-LE-Philadelphia/Data - Cleansed/"

# Create variable to house the directory path to create ease during import process
dir_name <- "//files.drexel.edu\\encrypted\\SOPH\\UHC\\SchnakeMahl_PAVitalStat\\Data\\"

load(file = paste0(path_onedrive,'sex_years.Rdata'))
load(file = paste0(path_onedrive, "low_pop_tracts.Rdata"))

# Mortality Rates ---------------------------------------------------------

load(file = paste0(path_onedrive, "phila_pop_seer.rdata"))
load(paste0(dir_name, "Philly LE\\", "phila_death_counts.Rdata")); gc()

# Philly mortality rates by census tract x age group x sex x years with adjustments
# phila_death_rate_age_grp_seer_all_fixes <- full_join(phila_death_counts_nolow_age_grp, phila_pop_age_grp_seer_all_fixes) %>%
#   inner_join(phila_sf_cross) %>%
#   mutate(    
#     deaths = case_when(
#       GEOID == 42101008801 & deaths > 0 & pop == 1 ~ 0,
#       TRUE ~ deaths),
#     rate=deaths/pop, log_rate = log(rate)) %>%
#   rowwise() %>%
#   ungroup() %>%
#   arrange(sex, years, GEOID, age_grp_5)

# phila_death_rate_age_grp_seer_all_fixes <- full_join(phila_death_counts_nolow_age_grp, phila_pop_age_grp_acs_all_fixes) %>%
#   inner_join(phila_sf_cross) %>%
#   mutate(    
#     # deaths = case_when(
#     #   GEOID == 42101008801 & deaths > 0 & pop == 1 ~ 0,
#     #   TRUE ~ deaths),
#     rate=deaths/pop, log_rate = log(rate)) %>%
#   rowwise() %>%
#   ungroup() %>%
#   arrange(sex, years, GEOID, age_grp_5)
# 
# length(unique(filter(phila_death_rate_age_grp_seer_all_fixes, years == '2017-2019')$GEOID))
# length(unique(filter(phila_death_rate_age_grp_seer_all_fixes, years == '2020-2022')$GEOID))
# sum((phila_death_rate_age_grp_seer_all_fixes%>%filter(GEOID %in% low_pop_2010))$deaths)
# sum((phila_death_rate_age_grp_seer_all_fixes%>%filter(GEOID %in% low_pop_2020))$deaths)
# sum((phila_death_rate_age_grp_seer_all_fixes%>%filter(GEOID %in% low_pop_2010))$pop)
# sum((phila_death_rate_age_grp_seer_all_fixes%>%filter(GEOID %in% low_pop_2020))$pop)
# sum(phila_death_rate_age_grp_seer_all_fixes$deaths)
# sum(phila_death_rate_age_grp_seer_all_fixes$pop)

# Philly mortality rate by census tract x age x sex x years with adjustments
# phila_death_rate_seer_all_fixes <- full_join(phila_death_counts_nolow, phila_pop_age_acs_seer_all_fixes) %>%
#   filter(!is.na(age)) %>%
#   mutate(
#     deaths = case_when(
#       GEOID == 42101008801 & deaths > 0 & pop < 1 ~ 0,
#       TRUE ~ deaths),
#     rate=deaths/pop, log_rate = log(rate)) %>%
#   inner_join(sex_years) %>%
#   arrange(GEOID, sex_years, age)

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

save(phila_death_rates_age_tract, phila_death_rates_age, 
     file = paste0(dir_name, "Philly LE\\", "phila_death_rates.Rdata"))

