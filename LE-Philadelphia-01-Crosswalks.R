
# Load Libraries ----------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(sf)
library(data.table)


# Shapefile/Paths/Crosswalks ----------------------------------------------

# Create variable to house the directory path to create ease during import process
dir_name <- "//files.drexel.edu\\encrypted\\SOPH\\UHC\\SchnakeMahl_PAVitalStat\\Data\\"

# Results path
results <- "C:/Users/ga437/OneDrive - Drexel University/Documents/UCH-LE-Philadelphia/Results/"

# Onedirve Paths (replace with your own folder)
# PATH FOR GIANNI'S COMPUTER:
path_onedrive <- "C:/Users/ga437/OneDrive - Drexel University/Documents/UCH-LE-Philadelphia/Data - Cleansed/"

# Philly shapefile - 2010 boundaries
phila_sf <- read_sf(dsn = "Data/ct10_prcm20230419/CT10_PRCM20230419.shp") %>%
  filter(STCOFIPS == 42101) %>%
  rename(GEOID = GEOID10) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  arrange(GEOID)

# Save shapefile for later use
save(phila_sf, file = paste0(path_onedrive, "phila_sf.Rdata"))

# Philly shapefile - 2020 boundaries
phila_sf_2020 <- read_sf(dsn = "Data/CT20_PRC/CT20_PRC.shp") %>%
  filter(COUNTYFP == 101) %>%
  rename(GEOID = GEOID20) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  arrange(GEOID)

# Save shapefile for later use
save(phila_sf_2020, file = paste0(path_onedrive, "phila_sf_2020.Rdata"))

# All tracts in Philly
phila_tracts <- unique(phila_sf$GEOID)
phila_tracts_2020 <- unique(phila_sf_2020$GEOID)

# Load GEOID20-GEOID10 Crosswalk
geoid_crosswalk <- fread("./Data/ltdb_ct20toct10_xwalk_files/ct20_ct10_xwalk.csv") %>%
  rename(GEOID20 = "TRTID2020", GEOID10 = "TRTID2010") %>%
  filter(GEOID20 %in% phila_tracts_2020 & GEOID10 %in% phila_tracts) %>%
  rename(WEIGHT = "TR20_TO_TR10_WEIGHT") %>%
  mutate(GEOID20 = as.numeric(GEOID20), GEOID10 = as.numeric(GEOID10))

# How many tracts were there in 2020 and 2010?
geoid_crosswalk$GEOID20 %>% unique %>% length # 408
geoid_crosswalk$GEOID10 %>% unique %>% length # 384

save(geoid_crosswalk, file = paste0(path_onedrive, "geoid_crosswalk.Rdata"))

# Low population/special land use tracts
low_pop <- phila_sf %>%
  as.data.frame %>%
  filter(LowPop_Spe == 1) %>%
  pull(GEOID)

# Save for later use
save(low_pop, file = paste0(path_onedrive, "low_pop_tracts.Rdata"))

# Create data frame with tract #'s and associated GEOIDs
tract_ids <- phila_sf %>%
  select(GEOID) %>%
  arrange(GEOID) %>%
  unique() %>%
  mutate(tract = 1:n()) %>%
  select(tract, GEOID)

# Save for later use
save(tract_ids, file = paste0(path_onedrive,'tract_ids.Rdata'))

# Crosswalk GEOID and OBJECTID
phila_sf_cross <- tract_ids %>% 
  as.data.frame %>% 
  select(tract, GEOID)

# phila_sf_cross_2020 <- tract_ids_2020 %>% 
#   as.data.frame %>% 
#   select(tract, GEOID) %>%
#   mutate(years = "2020-2022")

# Save for later use
save(phila_sf_cross, file = paste0(path_onedrive, "phila_sf_cross.Rdata"))

# Number of tracts in Philly
N_tracts <- length(phila_tracts)
# N_tracts_2020 <- length(phila_tracts_2020)

# Crosswalk of all analysis years and their year groups
years_cross <- data.frame(matrix(nrow=6, ncol=2)) %>% rename("year"=X1, "years"=X2) %>%
  mutate(year=c(2017:2022),
         years=case_when(year>=2017 & year<=2019 ~ "2017-2019",
                         year>=2020 & year<=2022 ~ "2020-2022"))

# Save for later use
save(years_cross, file = paste0(path_onedrive, "years_cross.Rdata"))

# Age limit for population
age_limit<-85

# Single-year age groups to be included in analysis
ages <- seq(from = 0, to = age_limit, by = 1)

# Crosswalk of all single-year age groups to 5-year age groups
age_grp_5 <- data.frame(matrix(nrow=87, ncol=2)) %>% rename("age"=X1, "age_grp_5"=X2) %>%
  mutate(age=c(0:85,NA), 
         age_grp_5 = case_when(
           age >= 0 & age < 5 ~ 0,
           age >= 5 & age < 10 ~ 5,
           age >= 10 & age < 15 ~ 10,
           age >= 15 & age < 20 ~ 15,
           age >= 20 & age < 25 ~ 20,
           age >= 25 & age < 30 ~ 25,
           age >= 30 & age < 35 ~ 30,
           age >= 35 & age < 40 ~ 35,
           age >= 40 & age < 45 ~ 40,
           age >= 45 & age < 50 ~ 45, 
           age >= 50 & age < 55 ~ 50,
           age >= 55 & age < 60 ~ 55,
           age >= 60 & age < 65 ~ 60,
           age >= 65 & age < 70 ~ 65,
           age >= 70 & age < 75 ~ 70,
           age >= 75 & age < 80 ~ 75,
           age >= 80 & age < 85 ~ 80,
           age >= 85 ~ 85))

# Save for later use
save(age_grp_5, file = paste0(path_onedrive,"age_grp_5.Rdata"))

# vector of age groups
age_grp_5_vec <- age_grp_5$age_grp_5 %>% unique

# All tract/age combinations
phila_tracts_ages <- expand.grid(phila_tracts, ages) %>%
  rename(GEOID = Var1, age = Var2) %>%
  inner_join(age_grp_5) %>%
  inner_join(phila_sf_cross) %>%
  arrange(tract, age) %>%
  mutate(age = as.numeric(age))

# Save for later use
save(phila_tracts_ages, file = paste0(path_onedrive,"phila_tracts_ages.Rdata"))

# Sexes and years for crosswalks
sexes <- c("Male", "Female")
years <- years_cross$years %>% unique

# Crosswalk of all ages x tracts x sexes x years
phila_tracts_ages_sexes_years <- expand.grid(phila_tracts, ages, sexes, years) %>%
  rename(GEOID = Var1, age = Var2, sex = Var3, years = Var4) %>%
  inner_join(age_grp_5) %>%
  inner_join(phila_sf_cross) %>%
  arrange(years, sex, tract, age) %>%
  mutate(age = as.numeric(age))

# Save for later use
save(phila_tracts_ages_sexes_years, file = paste0(path_onedrive,'phila_tracts_ages_sexes_years.Rdata'))

# Crosswalk of all age groups x tracts x sexes x years
phila_tracts_age_grp_5_sexes_years <- expand.grid(phila_tracts, age_grp_5_vec, sexes, years) %>%
  rename(GEOID = Var1, age_grp_5 = Var2, sex = Var3, years = Var4) %>%
  inner_join(phila_sf_cross) %>%
  arrange(years, sex, tract, age_grp_5) %>%
  mutate(age_grp_5 = as.numeric(age_grp_5))

save(phila_tracts_age_grp_5_sexes_years, file = paste0(path_onedrive,'phila_tracts_age_grp_5_sexes_years.Rdata'))

# phila_tracts_age_grp_5_sexes_years_2020 <- expand.grid(phila_tracts_2020, age_grp_5_vec, sexes, years) %>%
#   rename(GEOID = Var1, age_grp_5 = Var2, sex = Var3, years = Var4) %>%
#   inner_join(phila_sf_cross_2020) %>%
#   arrange(years, sex, tract, age_grp_5) %>%
#   mutate(age_grp_5 = as.numeric(age_grp_5))
# 
# save(phila_tracts_age_grp_5_sexes_years_2020, file = paste0(path_onedrive,'phila_tracts_age_grp_5_sexes_years_2020.Rdata'))
# 
# phila_tracts_age_grp_5_sexes_years <- rbind(phila_tracts_age_grp_5_sexes_years_2010, phila_tracts_age_grp_5_sexes_years_2020)

# Crosswalk of sexes x years
sex_years <- phila_tracts_ages_sexes_years %>%
  group_by(sex, years) %>%
  summarise(sex_years = cur_group_id()) %>%
  mutate(sex_years_lab = paste0(sex, " ", years) %>%
           factor(levels = c("Male 2006-2010", "Male 2011-2015", "Male 2016-2020", "Female 2006-2010", "Female 2011-2015", "Female 2016-2020")))

# sex_years <- phila_tracts_ages_sexes_years %>%
#   group_by(sex, years) %>%
#   summarise(sex_years = cur_group_id()) %>%
#   mutate(sex_years_lab = paste0(sex, " ", years) %>%
#            factor(levels = c("Male 2017-2019", "Male 2020-2022", "Female 2017-2019", "Female 2020-2022")),
#          gender)

save(sex_years, file = paste0(path_onedrive,'sex_years.Rdata'))
