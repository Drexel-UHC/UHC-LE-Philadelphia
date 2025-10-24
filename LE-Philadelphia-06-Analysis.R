
# Load Libraries & Data ---------------------------------------------------

rm(list=ls())
library(tidyverse)
library(classInt)
library(tidytext)
library(tidycensus)
library(reldist)
library(Hmisc)
library(diagis)
library(dineq)
library(plotly)
library(leaflet)
library(sf)
library(bit64)
library(broom)
library(GGally)
library(naniar)
library(tmap)
library(RColorBrewer)
library(ggpubr)

# PATH FOR GIANNI'S COMPUTER (DREXEL)
path_onedrive <- "C:/Users/ga437/OneDrive - Drexel University/Documents/LE-Philadelphia/Data - Cleansed/"
dir_name <- "//files.drexel.edu\\encrypted\\SOPH\\UHC\\SchnakeMahl_PAVitalStat\\Data\\"

# Load Data
load(file=paste0(path_onedrive, "le_df_nimble_20250117.Rdata"))
load(file=paste0(path_onedrive,'sex_years.RData'))
load(file=paste0(path_onedrive,'phila_sf.RData'))
load(file = paste0(path_onedrive, "phila_sf_2020.Rdata"))
load(file = paste0(path_onedrive, "low_pop_tracts.Rdata"))
load(file=paste0(path_onedrive,'tract_ids.RData'))
load(file = paste0(path_onedrive,'phila_tracts_ages_sexes_years.Rdata'))
load(file=paste0(path_onedrive,'phila_pop_acs.RData'))
load(file=paste0(path_onedrive,'phila_pop_seer.RData'))

# All tracts in Philly
phila_tracts <- unique(phila_sf$GEOID)
phila_tracts_2020 <- unique(phila_sf_2020$GEOID)


# Summary -----------------------------------------------------------------

load(file = "ShinyApp/Data/le_birth.Rdata"); gc()

# summarise by averaging across tracts for sex/time
le_summary <- le_birth %>%
  group_by(sex, years, sex_years, sex_years_lab) %>%
  summarise(le_med = round(median(med, na.rm = TRUE),2),
            le_lci = round(quantile(med, 0.025, na.rm=T),2),
            le_uci = round(quantile(med, 0.975, na.rm=T),2))

# summarise change in LE
le_change_summary <- le_birth %>%
  pivot_wider(names_from = years, values_from = med, values_fill = 0) %>%
  group_by(GEOID, sex) %>%
  summarise(t1 = sum(`2017-2019`), 
            t2 = sum(`2020-2022`),
            .groups = "drop") %>%
  mutate(le_change = t2 - t1) %>%
  group_by(sex) %>%
  summarise(change_med = round(median(le_change, na.rm = T),2),
            change_lci = round(quantile(le_change, 0.025, na.rm = T),2),
            change_uci = round(quantile(le_change, 0.975, na.rm = T),2))

# % of Tracts that had a decrease in LE
le_decrease <- le_birth_change %>%
  mutate(decr = med < 0) %>%
  group_by(sex) %>%
  summarise(pct_decr = mean(decr, na.rm = T)) %>%
  mutate(pct_decr = round(pct_decr*100,2))


# Dataframes for Maps -----------------------------------------------------


# Dataframe for maps of life expectancy at birth by census tract x sex x years
le_birth <- filter(le_df_nimble, Age == 0) %>%
  left_join(sex_years) %>%
  full_join(phila_sf) %>%
  select(GEOID, Age, lci, med, uci, geometry, sex, years, sex_years, sex_years_lab) %>%
  ungroup() %>%
  mutate(
    med = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ med),
    lci = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ lci),
    uci = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ uci),
    jenks=cut(med, breaks=classIntervals(med,n=6,style="jenks")$brks,include.lowest=T))%>%
  inner_join(tract_ids) %>%
  arrange(sex_years, tract)

save(le_birth, file = "ShinyApp/Data/le_birth.Rdata")


# MHI ---------------------------------------------------------------------

# Load 2020-2010 GEOID Crosswalk
load(file = paste0(path_onedrive, "geoid_crosswalk.Rdata"))

# Load ACS variables
vars <- load_variables(2015, "acs5")
vars_2017_2019 <- load_variables(2019, "acs5")
vars_2020_2022 <- load_variables(2023, "acs5")
vars_2020 <- load_variables(2020, "pl")

#Pull mhi by tract
mhi_1 <-  map_dfr(c(2019:2023), function(year_mhi){
  df <- get_acs(geography = "tract", survey = "acs5", state = 42, year = year_mhi,
               variables = c(mhi = "B19013_001")) %>% # pop_male = "B01001_002", pop_female = "B01001_026", pop_total = "B01001_001",
    mutate(GEOID = as.numeric(GEOID),
           year = year_mhi) %>%
    pivot_wider(names_from = variable, values_from = estimate, values_fill = 0) %>%
    select(-moe) %>%
    group_by(GEOID, year) %>%
    summarise_at(c("mhi"), sum, na.rm = T) %>%
    arrange(GEOID)
  
  return(df)
}) %>%
  pivot_wider(names_from = year, values_from = mhi, values_fill = 0) %>%
  group_by(GEOID) %>%
  summarise_all(sum, na.rm = T) %>%
  mutate(`2023` = case_when(
    `2019`!=0 & `2023`==0 & `2022`!=0   ~ `2022`,
    `2019`!=0 & `2023`==0 & `2022`==0 & `2021` !=0 ~ `2021`,
    `2019`!=0 & `2023`==0 & `2022`==0 & `2021` ==0 & `2020` != 0 ~ `2020`,
    TRUE ~ `2023`)) %>%
  select(GEOID, `2019`, `2023`) %>%
  pivot_longer(cols = c(`2019`, `2023`), names_to = "years", values_to = "mhi") %>%
  mutate(
    years = case_when(
      years == "2019" ~ "2017-2019",
      years == "2023" ~ "2020-2022"
    )
  ) %>%
  filter((years == '2017-2019' & GEOID %in% phila_tracts) | (years == '2020-2022' & GEOID %in% phila_tracts_2020))

# Compare tracts that did not change
mhi_comp_unchanged_tracts <- mhi_1 %>%
  pivot_wider(names_from = years, values_from = mhi, values_fill = 0) %>%
  group_by(GEOID) %>%
  summarise_all(sum) %>%
  # mutate(
  #   `2017-2019` = case_when(
  #     GEOID %in% phila_tracts ~ `2017-2019`,
  #     TRUE ~ NA_real_),
  #   `2020-2022` = case_when(
  #     GEOID %in% phila_tracts_2020 ~ `2020-2022`,
  #     TRUE ~ NA_real_)) %>%
  mutate(
    `2019` = case_when(
      GEOID %in% phila_tracts ~ `2019`,
      TRUE ~ NA_real_),
    `2020` = case_when(
      GEOID %in% phila_tracts_2020 ~ `2020`,
      TRUE ~ NA_real_),
    `2021` = case_when(
      GEOID %in% phila_tracts_2020 ~ `2021`,
      TRUE ~ NA_real_),
    `2022` = case_when(
      GEOID %in% phila_tracts_2020 ~ `2022`,
      TRUE ~ NA_real_),
    `2023` = case_when(
      GEOID %in% phila_tracts_2020 ~ `2023`,
      TRUE ~ NA_real_)) %>%
  # filter(!is.na(`2017-2019`) & !is.na(`2020-2022`),
  #        !(GEOID %in% low_pop)) %>%
  filter(!is.na(`2019`) & !is.na(`2020`),
         !is.na(`2021`) & !is.na(`2022`),
         !is.na(`2023`) & !(GEOID %in% low_pop)) %>%
  # mutate(diff = `2020-2022` - `2017-2019`, pct = diff/`2017-2019`*100)
  mutate(diff_2020 = `2020` - `2019`, pct_2020 = diff_2020/`2019`*100,
         diff_2021 = `2021` - `2019`, pct_2021 = diff_2021/`2019`*100,
         diff_2022 = `2022` - `2019`, pct_2022 = diff_2022/`2019`*100,
         diff_2023 = `2023` - `2019`, pct_2023 = diff_2023/`2019`*100)


# Filter for just 2020-2022 and map to 2010 census tracts
mhi_2020_2022 <- mhi_1 %>%
  filter(years == '2020-2022') %>%
  inner_join(geoid_crosswalk, by = c("GEOID" = "GEOID20")) %>%
  mutate(mhi_weighted = mhi * WEIGHT) %>%
  group_by(GEOID10, years) %>%
  summarise(mhi = sum(mhi*WEIGHT)/sum(WEIGHT)) %>% # pop_male = sum(pop_male*WEIGHT), pop_female = sum(pop_female*WEIGHT), pop_total = sum(pop_total*WEIGHT), 
  rename(GEOID = GEOID10)

# MHI for main analysis - use 'baseline' (2017-2019) MHI for both periods
mhi <- mhi_1 %>%
  filter(years == '2017-2019') %>%
  select(GEOID, mhi) %>%
  rename(mhi_2017_2019 = mhi) %>%
  inner_join(mhi_1 %>% filter(years == '2017-2019') %>% select(GEOID, mhi) %>% rename(mhi_2020_2022 = mhi), by = "GEOID") %>%
  ungroup() %>%
  pivot_longer(cols = c(mhi_2017_2019, mhi_2020_2022), names_to = "years", values_to = "mhi") %>%
  mutate(
    years = case_when(
      years == "mhi_2017_2019" ~ "2017-2019",
      years == "mhi_2020_2022" ~ "2020-2022"),
    mhi = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ mhi))

# Save as R data file
save(mhi, file = paste0(path_onedrive, "mhi.Rdata"))

# MHI for sensitivity analysis - time varying MHI
mhi_sensitivity <- mhi_1 %>%
  filter(years == '2017-2019') %>%
  select(GEOID, mhi) %>%
  rename(mhi_2017_2019 = mhi) %>%
  inner_join(mhi_2020_2022 %>% select(GEOID, mhi) %>% rename(mhi_2020_2022 = mhi), by = "GEOID") %>%
  ungroup() %>%
  pivot_longer(cols = c(mhi_2017_2019, mhi_2020_2022), names_to = "years", values_to = "mhi") %>%
  mutate(
    years = case_when(
      years == "mhi_2017_2019" ~ "2017-2019",
      years == "mhi_2020_2022" ~ "2020-2022"),
    mhi = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ mhi))

# Save as R data file
save(mhi_sensitivity, file = paste0(path_onedrive, "mhi_sensitivity.Rdata"))

# Compare 2017-2019 and 2020-2022 MHI
mhi_comparison <- mhi_sensitivity %>%
  filter(years == '2017-2019') %>%
  select(GEOID, mhi) %>%
  rename(mhi_2017_2019 = mhi) %>%
  inner_join(mhi_2020_2022 %>% select(GEOID, mhi) %>% rename(mhi_2020_2022 = mhi), by = "GEOID") %>%
  ungroup() %>%
  mutate(diff = mhi_2020_2022 - mhi_2017_2019, pct = diff/mhi_2017_2019*100) %>%
  filter(!(GEOID %in% low_pop))
         
# Map MHI % Change
mhi_change_map <- mhi_comparison %>%
  inner_join(phila_sf) %>%
  ggplot(aes(fill = pct)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = "bottom")
mhi_change_map

# Create tmap with jenks
mhi_change_map_tmap <- mhi_comparison %>%
  inner_join(phila_sf) %>%
  st_as_sf() %>%
  tm_shape() +
  tm_fill(col = "pct", style = "jenks", palette = "viridis") +
  tm_borders() +
  tm_layout(legend.position = c("right", "bottom"))
mhi_change_map_tmap

#Get average and sd of mhi for Philadelphia
mhi_phl_demographics <- mhi %>% # mhi_sensitivity
  group_by(years) %>%
  summarise(mean = round(mean(mhi, na.rm = TRUE), 2), # , wtd.mean = round(wtd.mean(mhi, w=pop_total),2)
            sd = round(sd(mhi, na.rm = TRUE), 2))     # , wtd.sd = round(sqrt(weighted_var(mhi, w=pop_total, na.rm = T)),2)

# MHI deciles - Main Analysis
mhi_deciles <- mhi %>% 
  group_by(years)%>%
  mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1), na.rm = T), include.lowest = T)), 
         decile_income1=(decile_income-1)/9)%>%
  inner_join(phila_sf)%>%
  select(GEOID, years, mhi, decile_income, decile_income1, geometry) # pop_male, pop_female, pop_total,

# Save for later use
save(mhi_deciles, file=paste0(path_onedrive, "mhi_deciles.Rdata"))

# MHI deciles - Sensitivity Analysis
mhi_deciles_sensitivity <-  mhi_sensitivity %>% 
  group_by(years)%>%
  mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1), na.rm = T), include.lowest = T)), 
         decile_income1=(decile_income-1)/9)%>%
  inner_join(phila_sf)%>%
  select(GEOID, years, mhi, decile_income, decile_income1, geometry)

# Save for later use
save(mhi_deciles_sensitivity, file=paste0(path_onedrive, "mhi_deciles_sensitivity.Rdata"))


# LE by Iteration with Income Decile --------------------------------------

#Load LE & MHI data
load(paste0(path_onedrive,'le_iter_nimble_20250117.Rdata'))
load(file=paste0(path_onedrive, "mhi_deciles.Rdata"))
load(file=paste0(path_onedrive, "mhi_deciles_sensitivity.Rdata"))

# LE by iteration with MHI deciles - Main Analysis
le_iter <- le_iter_nimble %>%
  inner_join(mhi_deciles) %>%
  inner_join(sex_years) %>%
  mutate(
    le = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ le))

# Save for later use
save(le_iter, file=paste0(path_onedrive, "le_iter.R"))

# LE by iteration with MHI deciles - Sensitivity Analysis
le_iter_sensitivity <- le_iter_nimble %>%
  inner_join(mhi_deciles_sensitivity) %>%
  inner_join(sex_years) %>%
  mutate(
    le = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ le))

# Save for later use
save(le_iter_sensitivity, file=paste0(path_onedrive, "le_iter_sensitivity.R"))

# Change in LE by iteration - Main Analysis
le_iter_change <- le_iter_nimble %>%
  mutate(
    le = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ le)) %>%
  pivot_wider(names_from = years, values_from = le, values_fill = 0) %>%
  group_by(GEOID, iteration, Age, sex) %>%
  summarise_at(c("2017-2019", "2020-2022"), sum, na.rm = T) %>%
  rename(t1 = '2017-2019', t2 = '2020-2022') %>%
  ungroup %>%
  mutate(change = t2-t1)

# Change in LE (medians with credible intervals) - main analysis
le_birth_change <- le_iter_change %>%
  filter(Age == 0) %>%
  group_by(GEOID, Age, sex) %>%
  summarise(lci = quantile(change, probs = c(.025), na.rm = TRUE),
            med = quantile(change, probs = c(.5), na.rm = TRUE),
            uci = quantile(change, probs = c(.975), na.rm = TRUE)) %>%
  full_join(phila_sf) %>%
  select(GEOID, Age, lci, med, uci, geometry, sex) %>%
  ungroup() %>%
  mutate(
    med = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ med),
    lci = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ lci),
    uci = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ uci),
    sig = case_when(
      GEOID %in% low_pop ~ NA,
      lci <= 0 & uci >= 0 ~ "Insignificant Change",
      TRUE ~ "Significant Change") %>% as.factor,
    width = uci - lci)%>%
  inner_join(tract_ids) %>%
  arrange(sex, tract)

# Save for later use and for Shiny app
save(le_birth_change, file = "ShinyApp/Data/le_birth_change.Rdata")

# What % of estimates had negative point estimates?
le_birth_change_neg <- le_birth_change %>%
  mutate(neg = med < 0) %>%
  group_by(sex) %>%
  summarise(neg_pct = mean(neg, na.rm = T)) %>%
  mutate(neg_pct = round(neg_pct*100,2))

le_birth_change_neg_sig <- le_birth_change %>%
  mutate(neg_uci = uci < 0) %>%
  group_by(sex) %>%
  summarise(neg_uci_pct = mean(neg_uci, na.rm = T))

# Posterior probability of decrease in LE
le_birth_post_prob_dec <- le_iter_change %>%
  mutate(decr = change < 0) %>%
  group_by(GEOID, sex) %>%
  summarise(prob_decr = mean(decr, na.rm = TRUE))  %>%
  mutate(prob_decr = round(prob_decr*100, 2)) %>%
  full_join(phila_sf) %>%
  select(GEOID, sex, prob_decr, geometry) %>%
  ungroup() %>%
  mutate(
    prob_decr = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ prob_decr))

# Save for later use
save(le_birth_post_prob_dec, file=paste0(path_onedrive, "le_birth_post_prob_dec.R"))

# Take average over tracts of posterior probabilities 
le_birth_post_prob_dec_summary <- le_birth_post_prob_dec %>%
  group_by(sex) %>%
  summarise(prob_decr = mean(prob_decr, na.rm = T))

# Change in LE with MHI deciles - Main Analysis
le_iter_change_deciles <-   bind_rows(
  le_iter_change %>% 
    mutate(years='2017-2019'),
  le_iter_change %>% 
    mutate(years='2020-2022')
  ) %>%
  inner_join(mhi_deciles) %>%
  inner_join(sex_years) %>%
  mutate(
    change = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ change))

# Save for later use
save(le_iter_change_deciles, file = paste0(path_onedrive, "le_iter_change_deciles.rdata"))

# Change in LE with MHI deciles - Sensitivity Analysis
le_iter_change_deciles_sensitivity <-   bind_rows(
  le_iter_change %>% 
    mutate(years='2017-2019'),
  le_iter_change %>% 
    mutate(years='2020-2022')
) %>%
  inner_join(mhi_deciles_sensitivity) %>%
  inner_join(sex_years) %>%
  mutate(
    change = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ change))

# Save for later use
save(le_iter_change_deciles_sensitivity, file = paste0(path_onedrive, "le_iter_change_deciles_sensitivity.rdata"))

# Total Disparities -------------------------------------------------------

# Absolute disparities
abs_ineq_long <- le_iter %>%
  filter(!is.na(le)) %>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes) %>%
  group_by(iteration, Age, sex, years, sex_years, sex_years_lab) %>%
  group_modify(~{
    #.x<-le_male_iter %>% filter(Age==0, iteration==1, GEOID==42101000100)
    wtd.mean <- weighted.mean(x = .x$le, w = .x$pop)
    mean <- mean(.x$le, na.rm = TRUE)
    quants <- reldist::wtd.quantile(x = .x$le, q = c(.1,.9), weight = .x$pop)
    wtd.sd <- sqrt(x = Hmisc::wtd.var(.x$le, weights = .x$pop))
    gini<-gini(x = .x$le, weights =.x$pop)*100
    data.frame(mean = mean, 
               wtd.mean = wtd.mean,
               dif=quants[2]-quants[1],
               ratio=quants[2]/quants[1],
               cv=wtd.sd/wtd.mean*100,
               gini=gini,
               mld.wt=mld.wtd(x = .x$le, weights =.x$pop)
    ) %>% as_tibble
  })

# Find median, 97.5, 2.5 percentile
abs_ineq_summary <- abs_ineq_long %>%
  group_by(Age, sex, years, sex_years, sex_years_lab)%>%
  summarise(med_dif=median(dif), 
            uci_dif=as.numeric(quantile(dif,probs=0.975)),
            lci_dif=as.numeric(quantile(dif,probs=0.025)),
            med_ratio=median(ratio),
            uci_ratio=as.numeric(quantile(ratio, probs=0.975)),
            lci_ratio=as.numeric(quantile(ratio, probs=0.025)),
            med_cv=median(cv), 
            uci_cv=as.numeric(quantile(cv,probs=0.975)),
            lci_cv=as.numeric(quantile(cv,probs=0.025)),
            med_gini=median(gini),
            uci_gini=as.numeric(quantile(gini,probs=0.975)),
            lci_gini=as.numeric(quantile(gini,probs=0.025)),
            med_mld.wt=median(mld.wt), 
            uci_mld.wt=as.numeric(quantile(mld.wt,probs=0.975)),
            lci_mld.wt=as.numeric(quantile(mld.wt,probs=0.025)))

# Calculate disparities between time periods
abs_ineq_time_disparities <- abs_ineq_long %>%
  group_by(iteration, Age, sex) %>%
  mutate(ID = cur_group_id(), dif_2017_2019 = dif, dif_2020_2022 = lead(dif), ratio_2017_2019 = ratio, ratio_2020_2022 = lead(ratio)) %>%
  group_by(iteration, Age, sex, years, sex_years, sex_years_lab) %>%
  summarise(dif_dif = dif_2020_2022 - dif_2017_2019, ratio_ratio = ratio_2020_2022/ratio_2017_2019)%>%
  filter(!is.na(dif_dif) & !is.na(ratio_ratio)) %>%
  select(-c("years", "sex_years", "sex_years_lab")) %>%
  group_by(Age, sex) %>%
  summarise(
    med_dif_dif=median(dif_dif),
    uci_dif_dif=as.numeric(quantile(dif_dif,probs=0.975)),
    lci_dif_dif=as.numeric(quantile(dif_dif,probs=0.025)),
    med_ratio_ratio=median(ratio_ratio),
    uci_ratio_ratio=as.numeric(quantile(ratio_ratio,probs=0.975)),
    lci_ratio_ratio=as.numeric(quantile(ratio_ratio,probs=0.025)))

options(scipen=999)

# Create table with CIs
summary_table <- abs_ineq_summary %>%
    mutate_at(.vars = vars(med_dif,uci_dif,lci_dif, med_ratio, uci_ratio, lci_ratio,
                           med_cv, uci_cv, lci_cv, med_gini, uci_gini, lci_gini), round, digits = 2) %>%
    mutate_at(.vars = vars(med_mld.wt, uci_mld.wt,lci_mld.wt), round, digits = 4) %>%
    ungroup() %>%
    transmute(
      Age = Age,
      Years = years,
      Sex = sex,
      AbsDif = paste0(med_dif, " (", lci_dif, ", ", uci_dif, ")"),
      RelDif = paste0(med_ratio, " (", lci_ratio, ", ", uci_ratio, ")"),
      CV = paste0(med_cv, " (", lci_cv, ", ", uci_cv, ")"),
      Gini = paste0(med_gini, " (", lci_gini, ", ", uci_gini, ")"),
      MLD = paste0(med_mld.wt, " (", lci_mld.wt, ", ", uci_mld.wt, ")"))

# Save as R object
save(summary_table, file = "ShinyApp/Data/summary_table.Rdata")

# Write table to CSV
write.csv(summary_table, "./Results/summary_table_20250117.csv", row.names = FALSE)

# Create table with CIs
summary_time_disparities_table <- abs_ineq_time_disparities %>%
    mutate_at(.vars = vars(med_dif_dif,uci_dif_dif,lci_dif_dif,
                           med_ratio_ratio, uci_ratio_ratio, lci_ratio_ratio), round, digits = 2) %>%
    transmute(
      Age = Age,
      Sex = sex,
      AbsDif = paste0(med_dif_dif, " (", lci_dif_dif, ", ", uci_dif_dif, ")"),
      RelDif = paste0(med_ratio_ratio, " (", lci_ratio_ratio, ", ", uci_ratio_ratio, ")"))

# Write table to CSV
write.csv(summary_time_disparities_table, "./Results/summary_time_disparities_table_20250117.csv", row.names = FALSE)

# Income-Based Disparities ------------------------------------------------

load(file = paste0(path_onedrive, "le_iter_change_deciles.rdata"))

# Find the top/bottom ratio and top/bottom difference for LE using MHI deciles - Main Analysis
decile_le <- le_iter %>%
  filter(!is.na(le)) %>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes) %>%
  group_by(Age, years, sex, sex_years, sex_years_lab, iteration, decile_income1) %>% 
  summarise(le=stats::weighted.mean(x = le, w = pop)) %>% 
  filter(decile_income1%in%c(0, 1))%>%
  pivot_wider(names_from=decile_income1, names_prefix="le_decile", values_from="le")%>%
  mutate(dif=le_decile1-le_decile0, 
         ratio=le_decile1/le_decile0)

# Find the top/bottom ratio and top/bottom difference for change in LE using MHI deciles - Main Analysis
decile_le_change <- le_iter_change_deciles %>%
  filter(!is.na(change)) %>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes %>% filter(years == "2017-2019")) %>%
  group_by(Age, years, sex, iteration, decile_income1) %>% 
  summarise(le=stats::weighted.mean(x = change, w = pop)) %>% 
  filter(decile_income1%in%c(0, 1))%>%
  pivot_wider(names_from=decile_income1, names_prefix="le_decile", values_from="le")%>%
  mutate(dif=le_decile1-le_decile0, 
         ratio=le_decile1/le_decile0)

# Find the top/bottom ratio and top/bottom difference using MHI deciles - Sensitivity Analysis
decile_le_sensitivity <- le_iter_sensitivity %>%
  filter(!is.na(le))%>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes) %>%
  group_by(Age, years, sex, sex_years, sex_years_lab, iteration, decile_income1) %>% 
  summarise(le=stats::weighted.mean(le, w=pop))%>% 
  filter(decile_income1%in%c(0, 1))%>%
  pivot_wider(names_from=decile_income1, names_prefix="le_decile", values_from="le")%>%
  mutate(dif=le_decile1-le_decile0, 
         ratio=le_decile1/le_decile0)

# Find median, 97.5, and 2.5 percentile for the ratio/difference measures for LE - Main Analysis
summary_income <- decile_le %>%
  group_by(Age, years, sex, sex_years, sex_years_lab)%>%
  summarise(med_dif=median(dif), 
            uci_dif=as.numeric(quantile(dif,probs=0.975)),
            lci_dif=as.numeric(quantile(dif,probs=0.025)),
            med_ratio=median(ratio),
            uci_ratio=as.numeric(quantile(ratio, probs=0.975)),
            lci_ratio=as.numeric(quantile(ratio, probs=0.025))) %>%
  arrange(Age, sex_years)

# Find median, 97.5, and 2.5 percentile for the ratio/difference measures for change in LE - Main Analysis
summary_income_change <- decile_le_change %>%
  group_by(Age, years, sex)%>%
  summarise(med_dif=median(dif), 
            uci_dif=as.numeric(quantile(dif,probs=0.975)),
            lci_dif=as.numeric(quantile(dif,probs=0.025)),
            med_ratio=median(ratio),
            uci_ratio=as.numeric(quantile(ratio, probs=0.975)),
            lci_ratio=as.numeric(quantile(ratio, probs=0.025))) %>%
  arrange(Age, sex)

# Find median, 97.5, and 2.5 percentile for the ratio/difference measures for LE - Sensitivity Analysis
summary_income_sensitivity <- decile_le_sensitivity %>%
  group_by(Age, years, sex, sex_years, sex_years_lab)%>%
  summarise(med_dif=median(dif), 
            uci_dif=as.numeric(quantile(dif,probs=0.975)),
            lci_dif=as.numeric(quantile(dif,probs=0.025)),
            med_ratio=median(ratio),
            uci_ratio=as.numeric(quantile(ratio, probs=0.975)),
            lci_ratio=as.numeric(quantile(ratio, probs=0.025))) %>%
  arrange(Age, sex_years)

# Find SII and RII for income
# SII - Slope Index of Inequality - diff in avg health status b/w a person in the highest 
# socioeconomic group and a person in the lowest socioeconomic group
# RII - Relative Index of Inequality - SII rel. to avg. health in the population

# SII/RII - Main Analysis
income_ineq_v2 <- le_iter %>%
  filter(!is.na(le))%>%
  group_by(Age, years, sex, sex_years, sex_years_lab, iteration) %>% 
  group_modify(~{
    #   if (.y$iteration==1) print(.y$cbsa)
    model_sii<-lm(le~decile_income1, data=.x) %>% tidy
    model_rii<-lm(log(le)~decile_income1, data=.x) %>% tidy
    bind_rows(model_sii %>% filter(term=="decile_income1") %>% select(estimate) %>% mutate(type="sii"),
              model_rii %>% filter(term=="decile_income1") %>% select(estimate) %>% mutate(type="rii"))
  }) %>%
  pivot_wider(names_from = "type", values_from = "estimate")

# SII/RII for change in LE - Main Analysis
income_ineq_v2_change <- le_iter_change_deciles %>%
  filter(!is.na(change), years == "2017-2019") %>%
  group_by(Age, sex, iteration) %>% 
  group_modify(~{
    #   if (.y$iteration==1) print(.y$cbsa)
    model_sii<-lm(change~decile_income1, data=.x) %>% tidy
    model_rii<-lm(log(change)~decile_income1, data=.x) %>% tidy
    bind_rows(model_sii %>% filter(term=="decile_income1") %>% select(estimate) %>% mutate(type="sii"),
              model_rii %>% filter(term=="decile_income1") %>% select(estimate) %>% mutate(type="rii"))
  }) %>%
  pivot_wider(names_from = "type", values_from = "estimate")

# SII/RII - Sensitivity Analysis
income_ineq_v2_sensitivity <- le_iter_sensitivity %>%
  filter(!is.na(le))%>%
  group_by(Age, years, sex, sex_years, sex_years_lab, iteration) %>% 
  group_modify(~{
    #   if (.y$iteration==1) print(.y$cbsa)
    model_sii<-lm(le~decile_income1, data=.x) %>% tidy
    model_rii<-lm(log(le)~decile_income1, data=.x) %>% tidy
    bind_rows(model_sii %>% filter(term=="decile_income1") %>% select(estimate) %>% mutate(type="sii"),
              model_rii %>% filter(term=="decile_income1") %>% select(estimate) %>% mutate(type="rii"))
  }) %>%
  pivot_wider(names_from = "type", values_from = "estimate")

# Find median, 97.5, and 2.5 percentile for the SII and RII - Main Analysis
income_ineq_v3 <- income_ineq_v2 %>%
  group_by(Age, years, sex, sex_years, sex_years_lab) %>% 
  summarise(med_sii=median(sii), 
            uci_sii=as.numeric(quantile(sii,probs=0.975)), 
            lci_sii=as.numeric(quantile(sii,probs=0.025)),
            med_rii=exp(median(rii)), 
            uci_rii=exp(as.numeric(quantile(rii,probs=0.975))), 
            lci_rii=exp(as.numeric(quantile(rii,probs=0.025)))) %>% 
  mutate(Sex = sex) %>%
  arrange(Age, sex_years)

# Find median, 97.5, and 2.5 percentile for the SII and RII for change in LE - Main Analysis
income_ineq_v3_change <- income_ineq_v2_change %>%
  group_by(Age, sex) %>% 
  summarise(med_sii=median(sii), 
            uci_sii=as.numeric(quantile(sii,probs=0.975)), 
            lci_sii=as.numeric(quantile(sii,probs=0.025)),
            med_rii=exp(median(rii)), 
            uci_rii=exp(as.numeric(quantile(rii,probs=0.975))), 
            lci_rii=exp(as.numeric(quantile(rii,probs=0.025)))) %>% 
  mutate(Sex = sex) %>%
  arrange(Age, Sex)

# Find median, 97.5, and 2.5 percentile for the SII and RII - Sensitivity Analysis
income_ineq_v3_sensitivity <- income_ineq_v2_sensitivity %>%
  group_by(Age, years, sex, sex_years, sex_years_lab) %>% 
  summarise(med_sii=median(sii), 
            uci_sii=as.numeric(quantile(sii,probs=0.975)), 
            lci_sii=as.numeric(quantile(sii,probs=0.025)),
            med_rii=exp(median(rii)), 
            uci_rii=exp(as.numeric(quantile(rii,probs=0.975))), 
            lci_rii=exp(as.numeric(quantile(rii,probs=0.025)))) %>% 
  mutate(Sex = sex) %>%
  arrange(Age, sex_years)

# Merge the SII and RII w/ the top/bottom difference and ratio
income_ineq <- summary_income %>% left_join(income_ineq_v3) %>% select(-c(Sex))
income_ineq_change <- summary_income_change %>% left_join(income_ineq_v3_change) %>% select(-c(Sex))
income_ineq_sensitivity <- summary_income_sensitivity %>% left_join(income_ineq_v3_sensitivity) %>% select(-c(Sex))

# Create table with CIs
income_ineq_table <- income_ineq %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  transmute(
    Age = Age,
    Years = years,
    Sex = sex,
    AbsDif = paste0(med_dif, " (", lci_dif, ", ", uci_dif, ")"),
    RelDif = paste0(med_ratio, " (", lci_ratio, ", ", uci_ratio, ")"),
    SII = paste0(med_sii, " (", lci_sii, ", ", uci_sii, ")"),
    RII = paste0(med_rii, " (", lci_rii, ", ", uci_rii, ")")) %>%
  ungroup %>%
  select(-c(years, sex, sex_years))

# Save as R object
save(income_ineq_table, file = "ShinyApp/Data/income_ineq_table.Rdata")

# Write table to CSV
write.csv(income_ineq_table, "./Results/income_ineq_table_20250117.csv", row.names = FALSE)

# Create table for change in LE with CIs
income_ineq_table_change <- income_ineq_change %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  transmute(
    Age = Age,
    Years = years,
    Sex = sex,
    AbsDif = paste0(med_dif, " (", lci_dif, ", ", uci_dif, ")"),
    RelDif = paste0(med_ratio, " (", lci_ratio, ", ", uci_ratio, ")"),
    SII = paste0(med_sii, " (", lci_sii, ", ", uci_sii, ")"),
    RII = paste0(med_rii, " (", lci_rii, ", ", uci_rii, ")")) %>%
  ungroup

# sensitivity
income_ineq_table_sensitivity <- income_ineq_sensitivity %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  transmute(
    Age = Age,
    Years = years,
    Sex = sex,
    AbsDif = paste0(med_dif, " (", lci_dif, ", ", uci_dif, ")"),
    RelDif = paste0(med_ratio, " (", lci_ratio, ", ", uci_ratio, ")"),
    SII = paste0(med_sii, " (", lci_sii, ", ", uci_sii, ")"),
    RII = paste0(med_rii, " (", lci_rii, ", ", uci_rii, ")")) %>%
  ungroup %>%
  select(-c(years, sex, sex_years))

# Calculate disparities between time periods
income_time_disparities <- income_ineq_v2 %>%
  group_by(iteration, Age, sex) %>%
  mutate(ID = cur_group_id(), sii_2017_2019 = sii, sii_2020_2022 = lead(sii), rii_2017_2019 = rii, rii_2020_2022 = lead(rii)) %>%
  group_by(iteration, Age, sex, years, sex_years, sex_years_lab) %>%
  summarise(dif_sii = sii_2020_2022 - sii_2017_2019, ratio_rii = rii_2020_2022/rii_2017_2019)%>%
  filter(!is.na(dif_sii) & !is.na(ratio_rii)) %>%
  select(-c("years", "sex_years", "sex_years_lab")) %>%
  group_by(Age, sex) %>%
  summarise(
    med_dif_sii=median(dif_sii),
    uci_dif_sii=as.numeric(quantile(dif_sii,probs=0.975)),
    lci_dif_sii=as.numeric(quantile(dif_sii,probs=0.025)),
    med_ratio_rii=median(ratio_rii),
    uci_ratio_rii=as.numeric(quantile(ratio_rii,probs=0.975)),
    lci_ratio_rii=as.numeric(quantile(ratio_rii,probs=0.025)))

# Create table with CIs
income_time_disparities_table <- income_time_disparities %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  transmute(
    Age = Age,
    Sex = sex,
    AbsDif_SII = paste0(med_dif_sii, " (", lci_dif_sii, ", ", uci_dif_sii, ")"),
    RelDif_RII = paste0(med_ratio_rii, " (", lci_ratio_rii, ", ", uci_ratio_rii, ")"))

# Write table to CSV
write.csv(income_time_disparities_table, "./Results/income_time_disparities_table_20250117.csv", row.names = FALSE)

# Save as R object
save(income_time_disparities_table, file = "ShinyApp/Data/income_time_disparities_table_20250117.Rdata")


# Data for LE by MHI Decile Plots -----------------------------------------

# Load data
load(file=paste0(path_onedrive, "le_iter.R"))
load(file=paste0(path_onedrive, "le_iter_sensitivity.R"))
load(file = paste0(path_onedrive, "le_iter_change_deciles.rdata"))
load(file = paste0(path_onedrive, "le_iter_change_deciles_sensitivity.rdata"))

# LE by Decile
le_by_decile <- le_iter %>%
  filter(!is.na(le))%>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes) %>%
  filter(!is.na(le), !is.na(pop), pop > 0) %>%
  group_by(Age, years, sex, iteration, decile_income) %>%
  summarise(wtdle=wtd.mean(le, weights=pop),
            wtdsd=sqrt(wtd.var(le, weights=pop))) %>%
  ungroup() %>%
  group_by(Age, years, sex, decile_income) %>%
  summarise(le = median(wtdle),
            le_lci = quantile(wtdle, probs = c(.025), na.rm = TRUE),
            le_uci = quantile(wtdle, probs = c(0.975), na.rm = TRUE),
            sd = median(wtdsd),
            sd_lci = quantile(wtdsd, probs = c(0.025), na.rm = TRUE),
            sd_uci = quantile(wtdsd, probs = c(0.975), na.rm = TRUE)) %>%
  rename(Sex = sex)

#Save for later use
save(le_by_decile, file = "ShinyApp/Data/le_by_decile.Rdata")

# LE by Decile for sensitivity analysis
le_by_decile_sensitivity <- le_iter_sensitivity %>%
  filter(!is.na(le))%>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes) %>%
  group_by(Age, years, sex, iteration, decile_income) %>%
  summarise(le=weighted.mean(le, w=pop)) %>%
  ungroup() %>%
  group_by(Age, years, sex, decile_income) %>%
  summarise(le = median(le)) %>%
  rename(Sex = sex)

#Save for later use
save(le_by_decile_sensitivity, file = paste0(path_onedrive, "le_by_decile_sensitivity.Rdata"))

# Change in LE by MHI decile - Main Analysis
le_change_by_decile <- le_iter_change_deciles %>%
  filter(!is.na(change))%>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes) %>%
  group_by(Age, years, sex, iteration, decile_income) %>%
  summarise(change=weighted.mean(change, w=pop)) %>%
  ungroup() %>%
  group_by(Age, years, sex, decile_income) %>%
  summarise(change = median(change)) %>%
  rename(Sex = sex) %>%
  mutate(`MHI Years` = years)

# Save for later use
save(le_change_by_decile, file = paste0(path_onedrive, "le_change_by_decile.Rdata"))

# Change in LE by MHI decile - Sensitivity Analysis
le_change_by_decile_sensitivity <- le_iter_change_deciles_sensitivity %>%
  filter(!is.na(change))%>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes) %>%
  group_by(Age, years, sex, iteration, decile_income) %>%
  summarise(change=weighted.mean(change, w=pop)) %>%
  ungroup() %>%
  group_by(Age, years, sex, decile_income) %>%
  summarise(change = median(change)) %>%
  rename(Sex = sex) %>%
  mutate(`MHI Years` = years)

# Save for later use
save(le_change_by_decile_sensitivity, file = paste0(path_onedrive, "le_change_by_decile_sensitivity.Rdata"))

# Mortality Rate Plots ---------------------------------------------------

load(file = paste0(path_onedrive,'lambdas_nimble_20250117.Rdata')); gc()
load(file = paste0(dir_name, "Philly LE\\", "phila_death_rates.Rdata")); gc()

# How many 0 deaths?
sum(phila_death_rate_seer_all_fixes$deaths == 0)/nrow(phila_death_rate_seer_all_fixes)
mean(phila_death_rate_seer_all_fixes$deaths)
sd(phila_death_rate_seer_all_fixes$deaths)^2
range(phila_death_rate_seer_all_fixes$deaths)

nozero <- phila_death_rate_seer_all_fixes %>% filter(deaths > 0)
mean(nozero$deaths)
sd(nozero$deaths)^2
range(nozero$deaths)

lambda_star <- loess(log_rate ~ age, 
                     data = phila_death_rate_age_seer_all_fixes %>% filter(sex_years == 1), 
                     degree = 2, span = 0.25)$fitted %>%
  append(loess(log_rate ~ age, 
               data = phila_death_rate_age_seer_all_fixes %>% filter(sex_years == 2), 
               degree = 2, span = 0.25)$fitted) %>%
  append(loess(log_rate ~ age, 
               data = phila_death_rate_age_seer_all_fixes %>% filter(sex_years == 3), 
               degree = 2, span = 0.25)$fitted) %>%
  append(loess(log_rate ~ age, 
               data = phila_death_rate_age_seer_all_fixes %>% filter(sex_years == 4), 
               degree = 2, span = 0.25)$fitted)

phila_death_rate_age_seer_all_fixes <- phila_death_rate_age_seer_all_fixes %>%
  cbind(log_lambda_star = lambda_star)

# Plot lambda star
lambda_star_plot <- phila_death_rate_age_seer_all_fixes %>%
  ggplot(aes(x = age, y = log_lambda_star)) +
  geom_line() +
  facet_wrap(~sex_years_lab) +
  labs(x = "Age", y = "Log Mortality Rate") +
  theme_bw() +
  theme(axis.text=element_text(color="black", size=24),
        axis.title =element_text(color="black", size=24),
        strip.text=element_text(color="black", size=24))
lambda_star_plot

# Save as png
ggsave(filename = "C:/Users/ga437/OneDrive - Drexel University/General - SAE Philly/0 - Papers (Life Expectancy)/JSM 2024/Figures/theta_star.png", 
       plot = lambda_star_plot, device = "png", width = 7, height = 7)

rates_plot_ribbon <- thetas_med_ci %>% 
  group_by(GEOID) %>% 
  group_map(~{
    p<-ggplot(.x, aes(x=age)) +
      geom_point(aes(x = knot, y = offset))+
      geom_rug(aes(x = rug))+
      geom_line(aes(y = offset))+
      geom_line(aes(y = med))+
      geom_ribbon(aes(ymin=lci, ymax=uci), alpha=0.3)+
      geom_ribbon(aes(ymin=offset_lci, ymax=offset_uci), alpha=0.3)+
      geom_line(aes(y=theta_star), color="red")+
      geom_hline(yintercept = 0)+
      geom_point(pch=21, fill=NA, color="black", aes(y=crude))+
      facet_wrap(~sex_years_lab)+
      labs(title=.y$GEOID, y = "Log Mortality Rate")+
      theme_bw()
    p$GEOID<-.y$GEOID
    p
  })

pdf(file = "./Results/rates_plot_ribbon_20250117.pdf")
rates_plot_ribbon
dev.off()

# Plot for census tract 42101001102
rates_42101001102 <- lambdas_med_ci %>% 
  filter(GEOID == 42101001102) %>% 
  ggplot(aes(x=age)) +
  geom_point(aes(x = knot, y = offset), color = "blue")+
  geom_rug(aes(x = rug))+
  geom_line(aes(y = offset), linetype = "dashed", color = "blue", )+
  geom_line(aes(y = med))+
  geom_ribbon(aes(ymin=lci, ymax=uci), alpha=0.3)+
  geom_ribbon(aes(ymin=offset_lci, ymax=offset_uci), fill = "blue", alpha=0.3)+
  geom_line(aes(y=theta_star), color="red")+
  geom_hline(yintercept = 0)+
  geom_point(pch=21, fill=NA, color="black", aes(y=crude))+
  geom_label(data = filter(thetas_med_ci, GEOID == 42101001102, age == 25, sex_years == 1),
            aes(y = offset + 1, label = "offset"), color = "blue", vjust = -0.5, size = 6)+
  geom_label(data = filter(lambdas_med_ci, GEOID == 42101001102, age == 25, sex_years == 1),
            aes(y = theta_star - 1, label = "standard \n schedule"), color = "red", vjust = -0.5, size = 6)+
  geom_label(data = filter(lambdas_med_ci, GEOID == 42101001102, age == 25, sex_years == 1),
            aes(y = med + 0.5, label = "log MR"), color = "black", vjust = -0.5, size = 6)+
  facet_wrap(~sex_years_lab)+
  labs(x = "Age", y = "Log Mortality Rate")+
  theme_bw()+
  theme(strip.text = element_text(size=24),
        axis.title = element_text(size=24),
        axis.text = element_text(size=24))
rates_42101001102

pdf(file = "./Results/rates_42101001102.pdf", width = 9, height = 9)
rates_42101001102
dev.off()

png(file = "./Results/rates_42101001102.png")
rates_42101001102
dev.off()

ggsave(filename = "C:/Users/ga437/OneDrive - Drexel University/General - SAE Philly/0 - Papers (Life Expectancy)/JSM 2024/Figures/rates_42101001102.png", 
       plot = rates_42101001102, device = "png", width = 7, height = 7)


# Racial-Ethnic Distribution of Deciles -----------------------------------

decile_by_racehisp <- inner_join(pop_hisp, pop_nhb, by = c("GEOID", "years", "Total", "decile_income")) %>%
  select(GEOID, years, Hispanic, NHB, Total, decile_income) %>%
  group_by(decile_income, years) %>%
  summarise(hisp = sum(Hispanic), nhb = sum(NHB), tot = sum(Total)) %>%
  mutate(pct_hisp = hisp/tot*100, pct_nhb = nhb/tot*100) %>%
  arrange(years, decile_income)

#Write table to CSV
write.csv(decile_by_racehisp, "./Results/decile_by_racehisp_20240206.csv", row.names = FALSE)

decile_by_racehisp_long <- decile_by_racehisp %>%
  pivot_longer(cols = c("pct_hisp", "pct_nhb"), names_to = "racehisp", values_to = "pct") %>%
  mutate(racehisp = case_when(
    racehisp == "pct_hisp" ~ "Hispanic",
    racehisp == "pct_nhb" ~ "Non-Hispanic Black"
  ))

decile_by_racehisp_plot <- decile_by_racehisp_long %>%
  ggplot(aes(x=decile_income, y=pct)) + 
  geom_line(aes(group=racehisp, color=racehisp))+
  geom_point(aes(color=racehisp))+
  labs(x="Decile of Median Household Income",
       y="% of Decile Population")+
  #scale_y_continuous(limits=c(65, 85.8), breaks=seq(65, 85, by=5))+
  scale_x_continuous(limits=c(0, 10), breaks=seq(0, 10 , by=2))+
  theme_bw() +
    theme(axis.text=element_text(color="black", size=15),
        axis.title.x =element_text(color="black", face="bold", size=20),
        axis.title.y =element_text(color="black", face="bold", size=20),
        strip.text=element_text(color="black", face="bold", size=20),
        strip.background = element_blank(), 
        legend.position = "bottom", 
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color="black", size = 20)) + 
  guides(color=guide_legend(title="Racial-Ethnic Group")) + 
  facet_wrap(~years)
decile_by_racehisp_plot

ggsave(filename = "./Results/decile_by_racehisp_plot_20240206.pdf", plot = decile_by_racehisp_plot, 
       device = "pdf", width = 14, height = 10)

ggsave(filename = "./Results/decile_by_racehisp_plot_20240206.png", plot = decile_by_racehisp_plot, 
       device = "png", width = 15, height = 10)


# Boxplots ----------------------------------------------------------------

boxplots_le <- ggplot(le_birth) +
  geom_boxplot(aes(x = sex, y = med, color = sex)) +   
  theme_bw() +
  labs(x = "Sex", y = "Life Expectancy at Birth") +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title=element_text(face="bold", size=20),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank()) +
  facet_wrap(~years) +
  guides(color=guide_legend(title="Sex"))
boxplots_le

ggsave(filename = "./Results/boxplots_le_20250117.png", plot = boxplots_le, 
       device = "png", width = 15, height = 10)


# MHI Plots ---------------------------------------------------------------

# Density plots comparing MHI by time period
mhi_density_plot <- mhi %>%
  ggplot(aes(x = mhi, fill = years)) +
  geom_density(alpha = 0.5) +
  labs(x = "Decile of Median Household Income", y = "Density") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title=element_text(face="bold", size=20),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank()) +
  guides(fill=guide_legend(title="Time Period"))
mhi_density_plot

#scatterplot comparing MHI by time period
mhi_scatter_plot <- mhi %>%
  pivot_wider(names_from = years, values_from = mhi, values_fill = 0) %>%
  select(-c("pop_female", "pop_male", "pop_total")) %>%
  group_by(GEOID) %>%
  summarise(across(everything(), sum))
  ggplot(aes(x = mhi, y = le, color = years)) +
  geom_point() +
  labs(x = "Decile of Median Household Income", y = "Life Expectancy at Birth") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title=element_text(face="bold", size=20),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank()) +
  guides(color=guide_legend(title="Time Period"))
mhi_scatter_plot


# Population Demographic Maps ---------------------------------------------

# Load ACS variables
vars<-load_variables(2015, "acs5", cache = TRUE) %>% rename(variable = name)

# Pull data on % of each tract that is Hispanic
pop_hisp <- get_acs(geography = "tract", year = 2015, state = 42, 
                    variables = c("Hispanic" = "B03002_012", "Total" = "B03002_001")) %>%
  mutate(years = '2011-2015') %>%
  rbind({
    get_acs(geography = "tract", year = 2010, state = 42, 
            variables = c("Hispanic" = "B03002_012", "Total" = "B03002_001")) %>%
      mutate(years = '2006-2010')
  }) %>%
  filter(substr(GEOID,1,5)==42101)%>%
  pivot_wider(names_from = variable, values_from = estimate, values_fill = 0) %>%
  group_by(GEOID, years)%>%
  summarise_at(c("Hispanic", "Total"), sum) %>%
  ungroup() %>%
  mutate(pct = Hispanic/Total*100, GEOID=as.numeric(GEOID), 
         jenks=cut(pct, breaks=classIntervals(pct,n=6,style="jenks")$brks,include.lowest=T),
         racehisp = "Hispanic")%>%
  inner_join(phila_sf) %>%
  inner_join(mhi_deciles, by = c("GEOID", "years", "geometry"))

# Pull data on % of each tract that is Non-Hispanic Black
pop_nhb<-get_acs(geography = "tract", 
                 year = 2015, state = 42, 
                 variables = c("NHB" = "B03002_004", "Total" = "B03002_001")) %>%
  mutate(years = '2011-2015') %>%
  rbind({
    get_acs(geography = "tract", 
            year = 2010, state = 42, 
            variables = c("NHB" = "B03002_004", "Total" = "B03002_001")) %>%
      mutate(years = '2006-2010')
  }) %>%
  filter(substr(GEOID,1,5)==42101)%>%
  pivot_wider(names_from = variable, values_from = estimate, values_fill = 0) %>%
  group_by(GEOID, years) %>%
  summarise_at(c("NHB", "Total"), sum) %>%
  ungroup() %>%
  mutate(pct = NHB/Total*100, GEOID=as.numeric(GEOID), 
         jenks=cut(pct, breaks=classIntervals(pct,n=6,style="jenks")$brks,include.lowest=T),
         racehisp = "Non-Hispanic Black")%>%
  inner_join(phila_sf)%>%
  inner_join(mhi_deciles, by = c("GEOID", "years", "geometry"))

# Create maps of % of each tract that is Hispanic
pop_hisp_maps <- ggplot(pop_hisp) +
  geom_sf(aes(geometry = geometry, fill = jenks), lwd = .1) + 
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "bottom", strip.text = element_text(size = 20), 
        legend.key.width = unit(4,"cm"), legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20)) + 
  facet_wrap(~ years, ncol = 2) +
  #scale_fill_manual(values = colorRampPalette(colors = c("red", "white", "green"))(6))+
  scale_fill_brewer() +
  labs(fill = "Hispanic % of Population")
pop_hisp_maps

# Create maps of % of each tract that is Non-Hispanic Black
pop_nhb_maps <- ggplot(pop_nhb) +
  geom_sf(aes(geometry = geometry, fill = jenks), lwd = .1) + 
  theme_void() +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "bottom", strip.text = element_text(size = 20), 
        legend.key.width = unit(4,"cm"), legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20)) + 
  facet_wrap(~ years, ncol = 2) +
  #scale_fill_manual(values = colorRampPalette(colors = c("red", "white", "green"))(6))+
  scale_fill_brewer() +
  labs(fill = "Non-Hispanic Black % of Population")
pop_nhb_maps


# Table 1 - Descriptive Statistics/Disparities ----------------------------

# Combine total and income-based disparities for Table 1
load(file = "ShinyApp/Data/summary_table.Rdata")
load(file = "ShinyApp/Data/income_ineq_table.Rdata")
load(file = "ShinyApp/Data/le_birth.Rdata")

table1_averageLE <- le_birth %>%
  group_by(sex, years) %>%
  summarise(
    mean_med = mean(med, na.rm = TRUE),
    # lci = mean(lci, na.rm = TRUE),
    # uci = mean(uci, na.rm = TRUE),
    sd = sd(med, na.rm = TRUE)
  ) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(Type = '',
         Measure = 'Average LE (SD)',
         mean_sd = paste0(mean_med, " (", sd,")")) %>%
  pivot_wider(names_from = c(sex, years), values_from = mean_sd, values_fill = "") %>%
  select(-c(mean_med,sd)) %>%
  summarise(
    Type = first(Type),
    Measure = first(Measure),
    across(where(is.character) & !all_of(c("Type", "Measure")), ~ .[. != ""][1])
  )

table1_total <- summary_table %>%
  filter(Age == 0) %>%
  select(Years, Sex, AbsDif, RelDif) %>%
  mutate(Type = 'Total') %>%
  pivot_longer(values_to = "Value", cols = c(AbsDif, RelDif), names_to = "Measure") %>%
  pivot_wider(names_from = c(Sex, Years), values_from = Value)

table1_income <- income_ineq_table %>%
  filter(Age == 0) %>%
  select(Years, Sex, AbsDif, RelDif) %>%
  mutate(Type = 'Income-Based') %>%
  pivot_longer(values_to = "Value", cols = c(AbsDif, RelDif), names_to = "Measure") %>%
  pivot_wider(names_from = c(Sex, Years), values_from = Value)

table1 <- rbind(table1_averageLE, 
                table1_total, 
                table1_income)

write.csv(table1, "./Results/Main Text/table1.csv", row.names = FALSE)

table1_income_sensitivity <- income_ineq_table_sensitivity %>%
  filter(Age == 0) %>%
  select(Years, Sex, AbsDif, RelDif) %>%
  mutate(Type = 'Income-Based') %>%
  pivot_longer(values_to = "Value", cols = c(AbsDif, RelDif), names_to = "Measure") %>%
  pivot_wider(names_from = c(Sex, Years), values_from = Value)

table1_sensitivity <- rbind(table1_averageLE, 
                            table1_total, 
                            table1_income_sensitivity)


# Figure 1 - LE w/ Credible Intervals Plots -------------------------------

load(file = "ShinyApp/Data/le_birth.Rdata")

#Credible intervals by tract

# Calculate ordering based on baseline years
t1_order <- le_birth %>%
  mutate(gen = 
           case_when(
             sex=='Male' ~ 'Men',
             sex=='Female' ~ 'Women'
           ) %>%
           as.factor) %>%
  filter(years == '2017-2019') %>%
  group_by(gen) %>%
  mutate(order = rank(med, ties.method = "first")) %>%
  select(GEOID, gen, order)

# Same plot with both periods in same facet
le_plots_t1_order <- le_birth %>%
  mutate(gen = 
           case_when(
             sex=='Male' ~ 'Men',
             sex=='Female' ~ 'Women'
           ) %>%
           as.factor) %>%
  full_join(t1_order, by = join_by(GEOID, gen)) %>%
  ggplot() +
  geom_point(aes(y = reorder(med, order), x = med, col = years)) +
  geom_segment(aes(y = reorder(med, order), 
                   yend = reorder(med, order), 
                   x = lci, xend = uci, col = years)) +
  theme_bw() +
  theme(
    axis.text=element_text(color="black", size=25),
    axis.title.x =element_text(color="black", size=25),
    axis.title.y =element_blank(),
    strip.text=element_text(color="black", size=25),
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25),
    strip.placement = "outside") +
  facet_wrap(~gen, ncol = 2, scales="free_y") +
  scale_x_continuous(sec.axis = dup_axis(name = NULL),
                     breaks = round(seq(25, 100, by = 5))) +
  labs(x= "LE at Birth (Years)", col = "Years")
le_plots_t1_order

# Load LE change data
load(file = "ShinyApp/Data/le_birth_change.Rdata")

# Calculate ordering based on change in LE
change_order <- le_birth_change %>%
  mutate(gen = 
           case_when(
             sex=='Male' ~ 'Men',
             sex=='Female' ~ 'Women'
           ) %>%
           as.factor) %>%
  group_by(gen) %>%
  mutate(order = rank(med, ties.method = "first")) %>%
  select(GEOID, gen, order)

# Plot change in LE with credible intervals
le_change_plots <- le_birth_change %>%
  mutate(gen = 
           case_when(
             sex=='Male' ~ 'Men',
             sex=='Female' ~ 'Women'
           ) %>%
           as.factor) %>%
  full_join(change_order, by = join_by(GEOID, gen)) %>%
  ggplot() +
  geom_point(aes(y = reorder(med, order), x = med)) +
  geom_segment(aes(y = reorder(med, order), 
                   yend = reorder(med, order), 
                   x = lci, xend = uci)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(
    axis.text=element_text(color="black", size=25),
    axis.title.x =element_text(color="black", size=25),
    axis.title.y =element_blank(),
    strip.text=element_text(color="black", size=25),
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    strip.placement = "outside") +
  facet_wrap(~gen, ncol = 2, scales="free_y") +
  scale_x_continuous(sec.axis = dup_axis(name = NULL),
                     breaks = round(seq(floor(min(le_birth_change$lci, na.rm = T)/5)*5, 
                                        ceiling(max(le_birth_change$uci, na.rm = T)/5)*5, by = 5))) +
  labs(x= "Change in LE at Birth (Years)")
le_change_plots

library(ggpubr)
le_plots_combined <- ggarrange(le_plots_t1_order, le_change_plots, ncol = 1)

ggsave(filename = "./Results/Main Text/figure1.pdf", plot = le_plots_combined, 
       device = "pdf", width = 16, height = 20)
ggsave(filename = "./Results/Main Text/figure1.png", plot = le_plots_combined, 
       device = "png", width = 16, height = 18, dpi = 600)

# Figure 2 - Maps of LE at Birth ------------------------------------------

# Load data
load(file = "ShinyApp/Data/le_birth.Rdata")

# LE at birth maps
# Convert to sf object
le_sf <- le_birth %>%
  mutate(gen = 
           case_when(
             sex=='Male' ~ 'Men',
             sex=='Female' ~ 'Women'
           ) %>%
           as.factor,
         gen_years_lab = paste0(gen, " ", years) %>% as.factor) %>%
  st_as_sf()

# Initialize list to store maps
le_maps_list <- list()

# Create maps for all sex x period combinations
for (i in unique(le_sf$gen_years_lab)) {
  
  # Filter data for a specific `sex_years`
  data_i <- le_sf %>% filter(gen_years_lab == i)
  
  # Create map
  le_maps_list[[i]] <- tm_shape(data_i) +
    tm_borders(lwd = 0.8) +
    tm_polygons(
      fill = "med", 
      fill.scale = tm_scale_intervals(style = "jenks", 
                                      values = brewer.pal(n = 5, name = "Blues"),
                                      label.na = "NA"), 
      fill.legend = tm_legend(title = "LE at Birth (Years)"),
      id = "GEOID") +
    tm_title(i) +
    tm_layout(
      title.position = c('center', 'top'),
      title.size = 2,
      inner.margins = c(0.02, 0.02, 0.02, 0.02),
      legend.position = c('right', 'bottom'),
      legend.title.size = 1.4, 
      legend.text.size = 1.4)
}

le_maps <- tmap_arrange(le_maps_list, ncol = 2, outer.margins = 0.02, widths = c(0.5, 0.5))
le_maps

# Save as pdf
tmap_save(le_maps, filename = "./Results/Main Text/figure2.pdf", 
          width = 14, height = 14, dpi = 600)
tmap_save(le_maps, filename = "./Results/Main Text/figure2.png",
          width = 14, height = 14, units = "in", dpi = 600)


# Figure 3 - Maps of Change in LE/Posterior Probability -------------------

# Load data
load(file = "ShinyApp/Data/le_birth_change.Rdata")
load(file=paste0(path_onedrive, "le_birth_post_prob_dec.R"))

# Change in LE at birth maps
le_change_sf <- le_birth_change %>%
  mutate(gen = 
           case_when(
             sex=='Male' ~ 'Men',
             sex=='Female' ~ 'Women'
           ) %>%
           as.factor) %>%
  st_as_sf()

# Posterior probability maps
post_prob_sf <- le_birth_post_prob_dec %>%
  mutate(gen = 
           case_when(
             sex=='Male' ~ 'Men',
             sex=='Female' ~ 'Women'
           ) %>%
           as.factor) %>%
  st_as_sf()

# Save for Shiny App
save(post_prob_sf, file = "ShinyApp/Data/post_prob_sf.Rdata")

# Initialize lists to store maps
le_change_maps_list <- list()
post_prob_maps_list <- list()

# Create maps for all sex x period combinations
for (i in unique(le_change_sf$gen)) {
  
  # Filter le change data
  data_i <- le_change_sf %>% filter(gen == i)
  
  # Map numerical value of change
  le_change_maps_list[[i]] <- tm_shape(data_i) + 
    tm_borders(lwd = 0.5) +
    tm_polygons(
      fill = "med", 
      fill.scale = tm_scale_intervals(style = "jenks", 
                                      values = colorRampPalette(c("red", "white", "green"))(5),
                                      label.na = "NA"), 
      fill.legend = tm_legend(title = "Change in LE \nat Birth (Years)"),
      id = "GEOID") +
    tm_title(i) +
    tm_layout(
      title.position = c('center', 'top'),
      title.size = 2,
      inner.margins = c(0.02, 0.02, 0.02, 0.25),
      legend.position = c('right', 'bottom'), 
      legend.title.size = 1.4, 
      legend.text.size = 1.4)
  
  # Filter posterior probability data 
  post_prob_i <- post_prob_sf %>% filter(gen == i)
  
  # Maps of posterior probability of a decrease in LE in each census tract
  post_prob_maps_list[[i]] <- tm_shape(post_prob_i) + 
    tm_borders(lwd = 0.5) +
    tm_polygons(
      fill = "prob_decr", 
      fill.scale = tm_scale_intervals(style = "fixed", 
                                      breaks = c(0, 5, 10, 25, 75, 90, 95, 100),
                                      values = brewer.pal(7, "RdBu")[7:1], 
                                      # label.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")),
                                      label.na = "NA"), 
      fill.legend = tm_legend(title = "Posterior Probability \nof Decrease in LE (%)"),
      id = "GEOID") +
    tm_title(i) +
    tm_layout(
      title.position = c('center', 'top'),
      title.size = 2,
      inner.margins = c(0.02, 0.02, 0.02, 0.25),
      legend.position = c('right', 'bottom'), 
      legend.title.size = 1.4, 
      legend.text.size = 1.4)
  
}

le_change_post_prob_maps_list <- c(le_change_maps_list, post_prob_maps_list)[c(1,3,2,4)]
le_change_post_prob_maps <- tmap_arrange(le_change_post_prob_maps_list, 
                                         ncol = 2, nrow = 2, outer.margins = 0.02)
le_change_post_prob_maps
tmap_save(le_change_post_prob_maps, filename = "./Results/Main Text/figure3.pdf", 
          width = 14, height = 14, dpi = 600)
tmap_save(le_change_post_prob_maps, filename = "./Results/Main Text/figure3.png",
          width = 14, height = 14, units = "in", dpi = 600)


# Figure 4 - LE/Change in LE by MHI Decile (Main Analysis) ----------------

# Load data
load(file = "ShinyApp/Data/le_by_decile.Rdata")
load(file = paste0(path_onedrive, "le_by_decile_sensitivity.Rdata"))
load(file = paste0(path_onedrive, "le_change_by_decile.Rdata"))
load(file = paste0(path_onedrive, "le_change_by_decile_sensitivity.Rdata"))

# Plots of LE by MHI decile with SII - Main Analysis
le_by_decile_plot_sii <- le_by_decile %>% 
  mutate(gen = 
           case_when(
             Sex=='Male' ~ 'Men',
             Sex=='Female' ~ 'Women'
           ) %>%
           as.factor) %>%
  filter(Age == 0) %>%
  ggplot(aes(x=decile_income, y=le)) + 
  geom_line(aes(group=years, color=years))+
  geom_point(aes(color=years))+
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", aes(group=years, color=years))+
  labs(x="Decile of Median Household Income",
       y="LE at Birth (Years)",
       col = "Years")+
  scale_y_continuous(limits=c(60,85), breaks=seq(60, 85, by=5))+
  scale_x_continuous(limits=c(-0.2, 11), breaks=seq(0, 10 , by=2))+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=15),
        axis.title =element_text(color="black", face="bold", size=20),
        strip.text=element_text(color="black", face="bold", size=20),
        strip.background = element_blank(), 
        legend.position = "bottom",
        legend.text = element_text(size=18), 
        legend.title = element_text(size=18)) + 
  facet_wrap(~gen)
le_by_decile_plot_sii

# Plots of LE by MHI decile with SII - Sensitivity Analysis
le_by_decile_plot_sii_sensitivity <- le_by_decile_sensitivity %>% 
  mutate(gen = 
           case_when(
             Sex=='Male' ~ 'Men',
             Sex=='Female' ~ 'Women'
           ) %>%
           as.factor) %>%
  filter(Age == 0) %>%
  ggplot(aes(x=decile_income, y=le)) + 
  geom_line(aes(group=years, color=years))+
  geom_point(aes(color=years))+
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", aes(group=years, color=years))+
  labs(x="Decile of Median Household Income",
       y="Life Expectancy at Birth")+
  scale_y_continuous(limits=c(60,85), breaks=seq(60, 85, by=5))+
  scale_x_continuous(limits=c(-0.2, 11), breaks=seq(0, 10 , by=2))+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=15),
        axis.title =element_text(color="black", face="bold", size=20),
        strip.text=element_text(color="black", face="bold", size=20),
        strip.background = element_blank(), legend.position = "bottom",
        legend.text = element_text(size=18), legend.title = element_text(size=18)) + 
  facet_wrap(~gen)
le_by_decile_plot_sii_sensitivity

# Plots of change in LE by MHI decile - Main Analysis
le_change_by_decile_plot <- le_change_by_decile %>% 
  mutate(gen = 
           case_when(
             Sex=='Male' ~ 'Men',
             Sex=='Female' ~ 'Women'
           ) %>%
           as.factor) %>%
  filter(Age == 0, `MHI Years` == '2017-2019') %>%
  ggplot(aes(x=decile_income, y=change)) + 
  geom_line(aes(group=years))+
  geom_point()+
  labs(x="Decile of Median Household Income",
       y="Change in LE at Birth (Years)")+
  #scale_y_continuous(limits=c(60,85), breaks=seq(60, 85, by=5))+
  scale_x_continuous(limits=c(-0.2, 11), breaks=seq(0, 10 , by=2))+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=15),
        axis.title =element_text(color="black", face="bold", size=20),
        strip.text=element_text(color="black", face="bold", size=20),
        strip.background = element_blank(), legend.position = "bottom",
        legend.text = element_text(size=18), legend.title = element_text(size=18)) + 
  facet_wrap(~gen)
le_change_by_decile_plot

# Stack plots of LE by MHI decile and chage in LE by MHI decile - Main Analysis
le_by_decile_plot_stacked <- ggarrange(le_by_decile_plot_sii, le_change_by_decile_plot, ncol = 1)

# Save plots
ggsave(filename = "./Results/Main Text/figure4.pdf", plot = le_by_decile_plot_stacked, 
       device = "pdf", width = 10, height = 12, dpi = 600)
ggsave(filename = "./Results/Main Text/figure4.png", plot = le_by_decile_plot_stacked, 
       device = "png", width = 10, height = 11, dpi = 600)


# Appendix S5 - MHI Maps --------------------------------------------------

# Load Data
load(file=paste0(path_onedrive, "mhi_deciles.Rdata"))

# Convert to sf
mhi_sf <- st_as_sf(mhi_deciles)

# Initialize list to store maps
mhi_maps_list <- list()

# Create map for each period
for (i in unique(mhi_sf$years)) {
  
  # Filter data for a specific `sex_years`
  data_i <- mhi_sf %>% filter(years == i)
  
  mhi_maps_list[[i]] <- tm_shape(data_i) +
    tm_borders(lwd = 0.8) +
    tm_polygons(
      fill = "mhi", 
      fill.scale = tm_scale_intervals(style = "jenks", 
                                      values = brewer.pal(n = 5, name = "Greens"),
                                      label.na = "NA"), 
      fill.legend = tm_legend(title = "MHI ($)"),
      id = "GEOID") +
    tm_title(i) +
    tmap_options(component.autoscale = TRUE) +
    tm_layout(
      title.position = c('center', 'top'),
      title.size = 2,
      inner.margins = c(0.02, 0.02, 0.02, 0.02),
      legend.position = c('right', 'bottom'),
      legend.title.size = 1.5, 
      legend.text.size = 1.5)
}

mhi_maps <- tmap_arrange(mhi_maps_list, ncol = 2, outer.margins = 0.02, widths = c(0.5, 0.5))
mhi_maps

# Save as pdf and png
tmap_save(mhi_maps, filename = "./Results/Appendix/S6.pdf", width = 14, height = 8, dpi = 600)
tmap_save(mhi_maps, filename = "./Results/Appendix/S6.png", width = 14, height = 8, dpi = 600)


# Appendix S6 - LE/Change in LE by MHI Decile (Sensitivity) ---------------

# Plots of change in LE by MHI decile - Sensitivity Analysis
le_change_by_decile_plot_sensitivity <- le_change_by_decile_sensitivity %>% 
  mutate(gen = 
           case_when(
             Sex=='Male' ~ 'Men',
             Sex=='Female' ~ 'Women'
           ) %>%
           as.factor) %>%
  filter(Age == 0) %>%
  ggplot(aes(x=decile_income, y=change)) + 
  geom_line(aes(group=years, color=`MHI Years`))+
  geom_point()+
  labs(x="Decile of Median Household Income",
       y="Change in LE at Birth (Years)")+
  scale_x_continuous(limits=c(-0.2, 11), breaks=seq(0, 10 , by=2))+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=15),
        axis.title =element_text(color="black", face="bold", size=20),
        strip.text=element_text(color="black", face="bold", size=20),
        strip.background = element_blank(), legend.position = "bottom",
        legend.text = element_text(size=18), legend.title = element_text(size=18)) + 
  facet_wrap(~gen)
le_change_by_decile_plot_sensitivity

# Stack plots of LE by MHI decile and chage in LE by MHI decile - Sensitivity Analysis
le_by_decile_plot_stacked_sensitivity <- ggarrange(le_by_decile_plot_sii_sensitivity, le_change_by_decile_plot_sensitivity, ncol = 1)

# Save plots
ggsave(filename = "./Results/Appendix/S6.pdf", plot = le_by_decile_plot_stacked_sensitivity, 
       device = "pdf", width = 10, height = 12, dpi = 600)
ggsave(filename = "./Results/Appendix/S6.png", plot = le_by_decile_plot_stacked_sensitivity, 
       device = "png", width = 10, height = 12, dpi = 600)

