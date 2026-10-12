# Load Libraries ----------------------------------------------------------


rm(list = ls())
library(here)
library(tidyverse)
library(sf)

# Create variable to house directory paths
dir_name <- here("//files.drexel.edu/encrypted/SOPH/UHC/SchnakeMahl_PAVitalStat/Data")
path_onedrive <- here("Data - Cleansed")


# Create Crosswalks -------------------------------------------------------


# Philly shapefile - 2010 boundaries
phila_sf <- read_sf(dsn = here("Data", "ct10_prcm20230419", "CT10_PRCM20230419.shp")) %>%
  filter(STCOFIPS == 42101) %>%
  rename(GEOID = GEOID10) %>%
  arrange(GEOID)

save(
  phila_sf,
  file = here(path_onedrive, "phila_sf.Rdata")
)

# Philly shapefile - 2020 boundaries
phila_sf_2020 <- read_sf(dsn = here("Data/CT20_PRC/CT20_PRC.shp")) %>%
  filter(COUNTYFP == 101) %>%
  rename(GEOID = GEOID20) %>%
  arrange(GEOID)

save(
  phila_sf_2020,
  file = here(path_onedrive, "phila_sf_2020.Rdata")
)

# All tracts in Philly
phila_tracts <- unique(phila_sf$GEOID)
phila_tracts_2020 <- unique(phila_sf_2020$GEOID)

# Load GEOID20-GEOID10 Crosswalk
geoid_crosswalk <- read_csv(here("Data", "ltdb_ct20toct10_xwalk_files", "ct20_ct10_xwalk.csv")) %>%
  rename(GEOID20 = "TRTID2020", GEOID10 = "TRTID2010") %>%
  filter(GEOID20 %in% phila_tracts_2020 & GEOID10 %in% phila_tracts) %>%
  rename(WEIGHT = "TR20_TO_TR10_WEIGHT")

save(
  geoid_crosswalk,
  file = here(path_onedrive, "geoid_crosswalk.Rdata")
)

# How many tracts were there in 2020 and 2010?
geoid_crosswalk$GEOID20 %>%
  unique() %>%
  length() # 408
geoid_crosswalk$GEOID10 %>%
  unique() %>%
  length() # 384

# Low population/special land use tracts
low_pop <- phila_sf %>%
  as.data.frame() %>%
  filter(LowPop_Spe == 1) %>%
  pull(GEOID)

save(
  low_pop,
  file = here(path_onedrive, "low_pop_tracts.Rdata")
)

# Create data frame with tract #'s and associated GEOIDs
tract_ids <- phila_sf %>%
  select(GEOID) %>%
  arrange(GEOID) %>%
  unique() %>%
  mutate(tract = 1:n()) %>%
  select(tract, GEOID)

save(
  tract_ids,
  file = here(path_onedrive, "tract_ids.Rdata")
)

# Crosswalk GEOID and OBJECTID
phila_sf_cross <- tract_ids %>%
  as.data.frame() %>%
  select(tract, GEOID)

save(
  phila_sf_cross,
  file = here(path_onedrive, "phila_sf_cross.Rdata")
)

# Number of tracts in Philly
N_tracts <- length(phila_tracts)

# Crosswalk of all analysis years and their year groups
years_cross <- data.frame(matrix(nrow = 6, ncol = 2)) %>%
  rename("year" = X1, "years" = X2) %>%
  mutate(
    year = c(2017:2022),
    years = case_when(
      year >= 2017 & year <= 2019 ~ "2017-2019",
      year >= 2020 & year <= 2022 ~ "2020-2022"
    )
  )

save(
  years_cross,
  file = here(path_onedrive, "years_cross.Rdata")
)

# Age limit for population
age_limit <- 85

# Single-year age groups to be included in analysis
ages <- seq(from = 0, to = age_limit, by = 1)

# Crosswalk of all single-year age groups to 5-year age groups (other than 0-1 and 1-4)
age_grp <- data.frame(age = 0:85) %>%
  mutate(
    age_grp = case_when(
      age == 0 ~ 0,
      age < 5 ~ 1,
      TRUE ~ 5 * floor(age / 5)
    )
  )

save(
  age_grp,
  file = here(path_onedrive, "age_grp.Rdata")
)

# Age groups
age_grps <- age_grp$age_grp %>% unique()

# All tract/age combinations
phila_tracts_ages <- expand.grid(phila_tracts, ages) %>%
  rename(GEOID = Var1, age = Var2) %>%
  inner_join(age_grp, by = "age") %>%
  inner_join(phila_sf_cross, by = "GEOID") %>%
  arrange(tract, age) %>%
  mutate(age = as.numeric(age))

# Sexes and years for crosswalks
sexes <- c("Men", "Women")
years <- years_cross$years %>% unique()

# Crosswalk of all ages x tracts x sexes x years
phila_tracts_ages_sexes_years <- expand.grid(phila_tracts, ages, sexes, years) %>%
  rename(GEOID = Var1, age = Var2, sex = Var3, years = Var4) %>%
  inner_join(age_grp, by = "age") %>%
  inner_join(phila_sf_cross, by = "GEOID") %>%
  arrange(years, sex, tract, age) %>%
  mutate(age = as.numeric(age))

save(
  phila_tracts_ages_sexes_years,
  file = here(path_onedrive, "phila_tracts_ages_sexes_years.Rdata")
)

# Crosswalk of all age groups x tracts x sexes x years
phila_tracts_age_grp_sexes_years <- expand.grid(phila_tracts, age_grps, sexes, years) %>%
  rename(GEOID = Var1, age_grp = Var2, sex = Var3, years = Var4) %>%
  inner_join(phila_sf_cross, by = "GEOID") %>%
  arrange(years, sex, tract, age_grp) %>%
  mutate(age_grp = as.numeric(age_grp))

save(
  phila_tracts_age_grp_sexes_years,
  file = here(path_onedrive, "phila_tracts_age_grp_sexes_years.Rdata")
)

# Crosswalk of sexes x years
sex_years <- phila_tracts_ages_sexes_years %>%
  group_by(sex, years) %>%
  summarise(
    sex_years = cur_group_id(),
    .groups = "drop"
  ) %>%
  mutate(
    sex_years_lab = paste0(sex, " ", years) %>%
      factor(
        levels = c(
          "Men 2017-2019", "Men 2020-2022",
          "Women 2017-2019", "Women 2020-2022"
        )
      )
  )

save(
  sex_years,
  file = here(path_onedrive, "sex_years.Rdata")
)
