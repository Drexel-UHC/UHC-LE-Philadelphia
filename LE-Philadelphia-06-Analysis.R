
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

# Create variable to house directory paths
results <- "Results/"
working_results <- paste0(results, "Working Results/")
main_text <- paste0(results, "Main Text/")
appendix <- paste0(results, "Appendix/")
path_onedrive <- "Data - Cleansed/"
dir_name <- "//files.drexel.edu\\encrypted\\SOPH\\UHC\\SchnakeMahl_PAVitalStat\\Data\\"

# Load data created in 01-Crosswalks 
load(file=paste0(path_onedrive,'sex_years.RData'))
load(file=paste0(path_onedrive,'phila_sf.RData'))
load(file = paste0(path_onedrive, "phila_sf_2020.Rdata"))
load(file = paste0(path_onedrive, "low_pop_tracts.Rdata"))
load(file=paste0(path_onedrive,'tract_ids.RData'))
load(file = paste0(path_onedrive,'phila_tracts_ages_sexes_years.Rdata'))

# All tracts in Philly
phila_tracts <- unique(phila_sf$GEOID)
phila_tracts_2020 <- unique(phila_sf_2020$GEOID)


# Dataframes for Maps -----------------------------------------------------

# Load LE data calculated in 05-Models
load(file=paste0(path_onedrive, "le_df_nimble_20250117.Rdata"))

# Dataframe life expectancy at birth by census tract x sex x years (with spatial structure for maps)
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
      TRUE ~ uci))%>%
  inner_join(tract_ids) %>%
  arrange(sex_years, tract)

# Save for Quarto doc
save(le_birth, file = "Quarto/Data/le_birth.Rdata")


# Summary -----------------------------------------------------------------

# Load LE at birth data created above
load(file = "Quarto/Data/le_birth.Rdata"); gc()

# Median/10th and 90th percentile of life expectancy across tracts
le_summary <- le_birth %>%
  group_by(sex, years, sex_years, sex_years_lab) %>%
  summarise(le_med = round(median(med, na.rm = TRUE),2),
            le_P10 = round(quantile(med, 0.1, na.rm=T),2),
            le_P90 = round(quantile(med, 0.9, na.rm=T),2))

# Median/10th and 90th percentile of life expectancy change across tracts
le_change_summary <- le_birth %>%
  pivot_wider(names_from = years, values_from = med, values_fill = 0) %>%
  group_by(GEOID, sex) %>%
  summarise(t1 = sum(`2017-2019`), 
            t2 = sum(`2020-2022`),
            .groups = "drop") %>%
  mutate(le_change = t2 - t1) %>%
  group_by(sex) %>%
  summarise(change_med = round(median(le_change, na.rm = T),2),
            change_P10 = round(quantile(le_change, 0.1, na.rm = T),2),
            change_P90 = round(quantile(le_change, 0.9, na.rm = T),2))


# MHI ---------------------------------------------------------------------

# Load 2020-2010 GEOID Crosswalk created in 01-Crosswalks
load(file = paste0(path_onedrive, "geoid_crosswalk.Rdata"))

#Pull MHI by tract
mhi_1 <-  map_dfr(c(2019:2023), function(year_mhi){
  df <- get_acs(geography = "tract", survey = "acs5", state = 42, year = year_mhi,
               variables = c(mhi = "B19013_001")) %>% # pop_men = "B01001_002", pop_women = "B01001_026", pop_total = "B01001_001",
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

# MHI for main analysis - use 'baseline' (2017-2019) MHI for both periods
mhi <- mhi_1 %>%
  filter(years == '2017-2019') %>%
  select(GEOID, mhi) %>%
  rename(mhi_2017_2019 = mhi) %>%
  inner_join(
    mhi_1 %>% 
      filter(years == '2017-2019') %>% 
      select(GEOID, mhi) %>% 
      rename(mhi_2020_2022 = mhi), 
    by = "GEOID") %>%
  ungroup() %>%
  pivot_longer(cols = c(mhi_2017_2019, mhi_2020_2022), names_to = "years", values_to = "mhi") %>%
  mutate(
    years = case_when(
      years == "mhi_2017_2019" ~ "2017-2019",
      years == "mhi_2020_2022" ~ "2020-2022"),
    mhi = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ mhi))

# Save for later use
save(mhi, file = paste0(path_onedrive, "mhi.Rdata"))

# Filter for just 2020-2022 and map to 2010 census tracts
mhi_2020_2022 <- mhi_1 %>%
  filter(years == '2020-2022') %>%
  inner_join(geoid_crosswalk, by = c("GEOID" = "GEOID20")) %>%
  mutate(mhi_weighted = mhi * WEIGHT) %>%
  group_by(GEOID10, years) %>%
  summarise(mhi = sum(mhi*WEIGHT)/sum(WEIGHT)) %>% # pop_men = sum(pop_men*WEIGHT), pop_women = sum(pop_women*WEIGHT), pop_total = sum(pop_total*WEIGHT), 
  rename(GEOID = GEOID10)

# MHI for sensitivity analysis - time varying MHI
mhi_sensitivity <- mhi_1 %>%
  filter(years == '2017-2019') %>%
  select(GEOID, mhi) %>%
  rename(mhi_2017_2019 = mhi) %>%
  inner_join(
    mhi_2020_2022 %>% 
      select(GEOID, mhi) %>% 
      rename(mhi_2020_2022 = mhi), 
    by = "GEOID") %>%
  ungroup() %>%
  pivot_longer(cols = c(mhi_2017_2019, mhi_2020_2022), names_to = "years", values_to = "mhi") %>%
  mutate(
    years = case_when(
      years == "mhi_2017_2019" ~ "2017-2019",
      years == "mhi_2020_2022" ~ "2020-2022"),
    mhi = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ mhi))

# Save for later use
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

# Create tmap with jenks
mhi_comparison %>%
  full_join(phila_sf) %>%
  st_as_sf() %>%
  tm_shape() +
  tm_polygons(
    fill = "pct",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = colorRampPalette(c("red", "green"))(5),
      midpoint = NA,
      label.na = "NA"),
    fill.legend = tm_legend(title = "MHI % Change")
  ) + 
  tm_borders() +
  tm_layout(legend.position = c("right", "bottom"))

#Get average and sd of mhi for Philadelphia
mhi_phl_demographics <- mhi %>% # mhi_sensitivity
  group_by(years) %>%
  summarise(mean = round(mean(mhi, na.rm = TRUE), 2), # , wtd.mean = round(wtd.mean(mhi, w=pop_total),2)
            sd = round(sd(mhi, na.rm = TRUE), 2))     # , wtd.sd = round(sqrt(weighted_var(mhi, w=pop_total, na.rm = T)),2)

# MHI deciles - main analysis
mhi_deciles <- mhi %>% 
  group_by(years)%>%
  mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1), na.rm = T), include.lowest = T)), 
         decile_income1=(decile_income-1)/9)%>%
  inner_join(phila_sf)%>%
  select(GEOID, years, mhi, decile_income, decile_income1, geometry) # pop_men, pop_women, pop_total,

# Save for later use
save(mhi_deciles, file=paste0(path_onedrive, "mhi_deciles.Rdata"))

# MHI deciles - sensitivity analysis
mhi_deciles_sensitivity <-  mhi_sensitivity %>% 
  group_by(years)%>%
  mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1), na.rm = T), include.lowest = T)), 
         decile_income1=(decile_income-1)/9)%>%
  inner_join(phila_sf)%>%
  select(GEOID, years, mhi, decile_income, decile_income1, geometry)

# Save for later use
save(mhi_deciles_sensitivity, file=paste0(path_onedrive, "mhi_deciles_sensitivity.Rdata"))


# LE by Iteration with Income Decile --------------------------------------

# Load LE data created in 05-Models & MHI data created above
load(paste0(path_onedrive,'le_iter_nimble_20250117.Rdata'))
load(file=paste0(path_onedrive, "mhi_deciles.Rdata"))
load(file=paste0(path_onedrive, "mhi_deciles_sensitivity.Rdata"))

# LE by iteration with MHI deciles - main analysis
le_iter <- le_iter_nimble %>%
  inner_join(mhi_deciles) %>%
  inner_join(sex_years) %>%
  mutate(
    le = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ le))

# Save for later use
save(le_iter, file=paste0(path_onedrive, "le_iter.R"))

# LE by iteration with MHI deciles - sensitivity analysis
le_iter_sensitivity <- le_iter_nimble %>%
  inner_join(mhi_deciles_sensitivity) %>%
  inner_join(sex_years) %>%
  mutate(
    le = case_when(
      GEOID %in% low_pop ~ NA_real_,
      TRUE ~ le))

# Save for later use
save(le_iter_sensitivity, file=paste0(path_onedrive, "le_iter_sensitivity.R"))

# Change in LE by iteration - main analysis
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

# Change in LE (medians with 95% credible intervals) - main analysis
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

# Save for later use and for Quarto doc
save(le_birth_change, file = "Quarto/Data/le_birth_change.Rdata")

# % of Tracts that had a decrease in LE
le_decrease <- le_birth_change %>%
  mutate(decr = med < 0) %>%
  group_by(sex) %>%
  summarise(pct_decr = mean(decr, na.rm = T)) %>%
  mutate(pct_decr = round(pct_decr*100,2))

# What % of estimates had negative point estimates?
le_birth_change_neg <- le_birth_change %>%
  mutate(neg = med < 0) %>%
  group_by(sex) %>%
  summarise(neg_pct = mean(neg, na.rm = T)) %>%
  mutate(neg_pct = round(neg_pct*100,2))

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

# Change in LE with MHI deciles - main analysis
le_iter_change_deciles <- bind_rows(
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

# Change in LE with MHI deciles - sensitivity analysis
le_iter_change_deciles_sensitivity <- bind_rows(
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

#Load LE data created in 03-Population and 05-Models
load(file = paste0(path_onedrive, "phila_pop_seer.rdata"))
load(file=paste0(path_onedrive, "le_iter.R"))

# Absolute disparities
abs_ineq_long <- le_iter %>%
  filter(!is.na(le)) %>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes) %>%
  group_by(iteration, Age, sex, years, sex_years, sex_years_lab) %>%
  group_modify(~{
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

# Find medians and 95% credible intervals
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

# Remove scientific notation
options(scipen=999)

# Create table with credible intervals
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

# Save data for Quarto doc
save(summary_table, file = "Quarto/Data/summary_table.Rdata")

# Write table to CSV
write.csv(summary_table, paste0(working_results,"summary_table_20250117.csv"), row.names = FALSE)

# Create table with credible intervals
summary_time_disparities_table <- abs_ineq_time_disparities %>%
    mutate_at(.vars = vars(med_dif_dif,uci_dif_dif,lci_dif_dif,
                           med_ratio_ratio, uci_ratio_ratio, lci_ratio_ratio), round, digits = 2) %>%
    transmute(
      Age = Age,
      Sex = sex,
      AbsDif = paste0(med_dif_dif, " (", lci_dif_dif, ", ", uci_dif_dif, ")"),
      RelDif = paste0(med_ratio_ratio, " (", lci_ratio_ratio, ", ", uci_ratio_ratio, ")"))

# Write table to CSV
write.csv(summary_time_disparities_table, paste0(working_results,"summary_time_disparities_table_20250117.csv"), row.names = FALSE)


# Income-Based Disparities ------------------------------------------------

# Load population data created in 03-Population
load(file = paste0(path_onedrive, "phila_pop_seer.rdata"))

# Load iteration-level LE and changes in LE by deciles created above
load(file=paste0(path_onedrive, "le_iter.R")); gc()

# Find the top/bottom ratio and top/bottom difference for LE using MHI deciles - main analysis
decile_le <- le_iter %>%
  filter(!is.na(le)) %>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes) %>%
  group_by(Age, years, sex, sex_years, sex_years_lab, iteration, decile_income1) %>% 
  summarise(le=stats::weighted.mean(x = le, w = pop)) %>% 
  filter(decile_income1%in%c(0, 1))%>%
  pivot_wider(names_from=decile_income1, names_prefix="le_decile", values_from="le")%>%
  mutate(dif=le_decile1-le_decile0, 
         ratio=le_decile1/le_decile0)

# Unload LE by iteration and clear memory
rm(le_iter); gc()

# Load iteration-level changes in LE data created above
load(file = paste0(path_onedrive, "le_iter_change_deciles.rdata")); gc()

# Find the top/bottom ratio and top/bottom difference for change in LE using MHI deciles - main analysis
decile_le_change <- le_iter_change_deciles %>%
  filter(!is.na(change)) %>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes %>% filter(years == "2017-2019")) %>%
  group_by(Age, years, sex, iteration, decile_income1) %>% 
  summarise(le=stats::weighted.mean(x = change, w = pop)) %>% 
  filter(decile_income1%in%c(0, 1))%>%
  pivot_wider(names_from=decile_income1, names_prefix="le_decile", values_from="le")%>%
  mutate(dif=le_decile1-le_decile0, 
         ratio=le_decile1/le_decile0)

# Unload change in LE by iteration and clear memory
rm(le_iter_change_deciles); gc()

# Load iteration-level LE for sensitivity analysis created above
load(file=paste0(path_onedrive, "le_iter_sensitivity.R")); gc()

# Find the top/bottom ratio and top/bottom difference using MHI deciles - sensitivity analysis
decile_le_sensitivity <- le_iter_sensitivity %>%
  filter(!is.na(le))%>%
  inner_join(phila_pop_seer_tract_sex_total_all_fixes) %>%
  group_by(Age, years, sex, sex_years, sex_years_lab, iteration, decile_income1) %>% 
  summarise(le=stats::weighted.mean(le, w=pop))%>% 
  filter(decile_income1%in%c(0, 1))%>%
  pivot_wider(names_from=decile_income1, names_prefix="le_decile", values_from="le")%>%
  mutate(dif=le_decile1-le_decile0, 
         ratio=le_decile1/le_decile0)

# Unload LE by iteration (sensitivity) and clear memory
rm(le_iter_sensitivity); gc()

# Find median, 97.5, and 2.5 percentile for the ratio/difference measures for LE - main analysis
summary_income <- decile_le %>%
  group_by(Age, years, sex, sex_years, sex_years_lab)%>%
  summarise(med_dif=median(dif), 
            uci_dif=as.numeric(quantile(dif,probs=0.975)),
            lci_dif=as.numeric(quantile(dif,probs=0.025)),
            med_ratio=median(ratio),
            uci_ratio=as.numeric(quantile(ratio, probs=0.975)),
            lci_ratio=as.numeric(quantile(ratio, probs=0.025))) %>%
  arrange(Age, sex_years)

# Find median, 97.5, and 2.5 percentile for the ratio/difference measures for change in LE - main analysis
summary_income_change <- decile_le_change %>%
  group_by(Age, years, sex)%>%
  summarise(med_dif=median(dif), 
            uci_dif=as.numeric(quantile(dif,probs=0.975)),
            lci_dif=as.numeric(quantile(dif,probs=0.025)),
            med_ratio=median(ratio),
            uci_ratio=as.numeric(quantile(ratio, probs=0.975)),
            lci_ratio=as.numeric(quantile(ratio, probs=0.025))) %>%
  arrange(Age, sex)

# Find median, 97.5, and 2.5 percentile for the ratio/difference measures for LE - sensitivity analysis
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

# Load iteration-level LE and changes in LE by deciles created above
load(file=paste0(path_onedrive, "le_iter.R")); gc()

# SII/RII - main analysis
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

# Unload LE by iteration and clear memory
rm(le_iter); gc()

# Load iteration-level changes in LE data created above
load(file = paste0(path_onedrive, "le_iter_change_deciles.rdata")); gc()

# SII/RII for change in LE - main analysis
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

# Unload change in LE by iteration and clear memory
rm(le_iter_change_deciles); gc()

# Load iteration-level LE for sensitivity analysis created above
load(file=paste0(path_onedrive, "le_iter_sensitivity.R")); gc()

# SII/RII - sensitivity analysis
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

# Unload LE by iteration (sensitivity) and clear memory
rm(le_iter_sensitivity); gc()

# Find median, 97.5, and 2.5 percentile for the SII and RII - main analysis
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

# Find median, 97.5, and 2.5 percentile for the SII and RII for change in LE - main analysis
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

# Find median, 97.5, and 2.5 percentile for the SII and RII - sensitivity analysis
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

# Save data for Quarto doc
save(income_ineq_table, file = "Quarto/Data/income_ineq_table.Rdata")

# Write table to CSV
write.csv(income_ineq_table, paste0(working_results,"income_ineq_table_20250117.csv"), row.names = FALSE)

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

# Save data for Quarto doc
save(income_ineq_table_sensitivity, file = paste0(path_onedrive, "income_ineq_table_sensitivity.Rdata"))


# Data for LE by MHI Decile Plots -----------------------------------------

# Load iteration-level LE and changes in LE by deciles created above
load(file=paste0(path_onedrive, "le_iter.Rdata")); gc()
load(file=paste0(path_onedrive, "phila_pop_seer.Rdata")); gc()

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

# Unload LE by iteration and clear memory
rm(le_iter); gc()

#Save data for later use and for Quarto doc
save(le_by_decile, file = "Quarto/Data/le_by_decile.Rdata")

# Load iteration-level LE for sensitivity analysis created above
load(file=paste0(path_onedrive, "le_iter_sensitivity.R")); gc()

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

# Unload LE by iteration (sensitivity) and clear memory
rm(le_iter_sensitivity); gc()

#Save data for later use
save(le_by_decile_sensitivity, file = paste0(path_onedrive, "le_by_decile_sensitivity.Rdata"))

# Load iteration-level changes in LE data created above
load(file = paste0(path_onedrive, "le_iter_change_deciles.rdata")); gc()

# Change in LE by MHI decile - main analysis
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

# Unload change in LE by iteration and clear memory
rm(le_iter_change_deciles); gc()

# Save data for later use
save(le_change_by_decile, file = paste0(path_onedrive, "le_change_by_decile.Rdata"))

# Load iteration-level LE changes (sensitivity) created above
load(file = paste0(path_onedrive, "le_iter_change_deciles_sensitivity.rdata"))

# Change in LE by MHI decile - sensitivity analysis
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

# Unload change in LE by iteration (sensitivity) and clear memory
rm(le_iter_change_deciles_sensitivity); gc()

# Save data for later use
save(le_change_by_decile_sensitivity, file = paste0(path_onedrive, "le_change_by_decile_sensitivity.Rdata"))


# Mortality Rate Plots ---------------------------------------------------

# Load mortality rate data created in 03-Mortality-Rates and 05-Models
load(file = paste0(path_onedrive,'lambdas_nimble_20250117.Rdata')); gc()
load(file = paste0(dir_name, "Philly LE\\", "phila_death_rates.Rdata")); gc()
load(file = paste0(path_onedrive,'thetas_med_ci_20250117.Rdata'));

# Plot estimated rates with credible intervals
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

# Save as PDF
pdf(file = paste0(working_results,"rates_plot_ribbon_20250117.pdf"))
rates_plot_ribbon
dev.off()


# Table 1 - Descriptive Statistics/Disparities ----------------------------

# Load data created above
load(file = "Quarto/Data/summary_table.Rdata")
load(file = "Quarto/Data/income_ineq_table.Rdata")
load(file = "Quarto/Data/le_birth.Rdata")

# Combine total and income-based disparities for Table 1
# Median LE by sex and time period
table1_med_LE <- le_birth %>%
  group_by(sex, years) %>%
  summarise(
    median = round(median(med, na.rm = TRUE),2),
    P10 = round(quantile(med, 0.10, na.rm = T),2),
    P90 = round(quantile(med, 0.90, na.rm = T),2),
  ) %>%
  mutate(Type = '',
         Measure = 'Median LE [P10 - P90]',
         med_10_90 = paste0(median, " [", P10," - ", P90,"]")) %>%
  pivot_wider(names_from = c(sex, years), values_from = med_10_90, values_fill = "") %>%
  select(-c(median,P10,P90)) %>%
  summarise(
    Type = first(Type),
    Measure = first(Measure),
    across(where(is.character) & !all_of(c("Type", "Measure")), ~ .[. != ""][1])
  )

# Total disparities
table1_total <- summary_table %>%
  filter(Age == 0) %>%
  select(Years, Sex, AbsDif, RelDif) %>%
  mutate(Type = 'Total') %>%
  pivot_longer(values_to = "Value", cols = c(AbsDif, RelDif), names_to = "Measure") %>%
  pivot_wider(names_from = c(Sex, Years), values_from = Value)

# Income-based disparities
table1_income <- income_ineq_table %>%
  filter(Age == 0) %>%
  select(Years, Sex, AbsDif, RelDif) %>%
  mutate(Type = 'Income-Based') %>%
  pivot_longer(values_to = "Value", cols = c(AbsDif, RelDif), names_to = "Measure") %>%
  pivot_wider(names_from = c(Sex, Years), values_from = Value)

# Table 1
table1 <- rbind(table1_med_LE, 
                table1_total, 
                table1_income)

# Write table to CSV
write.csv(table1, paste0(main_text,"table1.csv"), row.names = FALSE)

# Income-based disparities for sensitivity analysis
table1_income_sensitivity <- income_ineq_table_sensitivity %>%
  filter(Age == 0) %>%
  select(Years, Sex, AbsDif, RelDif) %>%
  mutate(Type = 'Income-Based') %>%
  pivot_longer(values_to = "Value", cols = c(AbsDif, RelDif), names_to = "Measure") %>%
  pivot_wider(names_from = c(Sex, Years), values_from = Value)

# Table 1 (sensitivity)
table1_sensitivity <- rbind(table1_averageLE, 
                            table1_total, 
                            table1_income_sensitivity)


# Figure 1 - LE w/ Credible Intervals Plots -------------------------------

# Load data created above
load(file = "Quarto/Data/le_birth.Rdata")

# Calculate ordering based on baseline years
t1_order <- le_birth %>%
  filter(years == '2017-2019') %>%
  group_by(sex) %>%
  mutate(order = rank(med, ties.method = "first")) %>%
  select(GEOID, sex, order)

# Same plot with both periods in same facet
le_plots_t1_order <- le_birth %>%
  full_join(t1_order, by = join_by(GEOID, sex)) %>%
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
  facet_wrap(~sex, ncol = 2, scales="free_y") +
  scale_x_continuous(sec.axis = dup_axis(name = NULL),
                     breaks = round(seq(25, 100, by = 5))) +
  labs(x= "LE at Birth (Years)", col = "Years")
le_plots_t1_order

# Load LE change data created above
load(file = "Quarto/Data/le_birth_change.Rdata")

# Calculate ordering based on change in LE
change_order <- le_birth_change %>%
  group_by(sex) %>%
  mutate(order = rank(med, ties.method = "first")) %>%
  select(GEOID, sex, order)

# Plot change in LE with credible intervals
le_change_plots <- le_birth_change %>%
  full_join(change_order, by = join_by(GEOID, sex)) %>%
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
  facet_wrap(~sex, ncol = 2, scales="free_y") +
  scale_x_continuous(sec.axis = dup_axis(name = NULL),
                     breaks = round(seq(floor(min(le_birth_change$lci, na.rm = T)/5)*5, 
                                        ceiling(max(le_birth_change$uci, na.rm = T)/5)*5, by = 5))) +
  labs(x= "Change in LE at Birth (Years)")
le_change_plots

# Combine plots
le_plots_combined <- ggarrange(le_plots_t1_order, le_change_plots, ncol = 1)

# Save as PDF and PNG
ggsave(filename = paste0(main_text,"figure1.pdf"), plot = le_plots_combined, 
       device = "pdf", width = 16, height = 20)
ggsave(filename = paste0(main_text,"figure1.png"), plot = le_plots_combined, 
       device = "png", width = 16, height = 18, dpi = 600)


# Figure 2 - Maps of LE at Birth ------------------------------------------

# Load data created above
load(file = "Quarto/Data/le_birth.Rdata")

# LE at birth maps
# Convert to sf object
le_sf <- le_birth %>%
  st_as_sf()

# Number of breaks for legend
n_breaks_le <- 5

# Initialize list to store maps
le_maps_list <- list()

# Create maps for all sex x period combinations
for (i in unique(le_sf$sex_years_lab)) {
  
  # Filter data for a specific `sex_years`
  data_i <- le_sf %>% filter(sex_years_lab == i)
  
  # Breaks for legend
  breaks_le <- classIntervals(data_i$med, n = n_breaks_le, style = "jenks")$brks
  
  # Create map
  le_maps_list[[i]] <- tm_shape(data_i) +
    tm_borders(lwd = 0.8) +
    tm_polygons(
      fill = "med", 
      fill.scale = tm_scale_intervals(style = "fixed", 
                                      breaks = breaks_le,
                                      label.format = list(text.separator = "to"),
                                      values = brewer.pal(n = n_breaks_le, name = "Blues"),
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

# Put maps together
le_maps <- tmap_arrange(le_maps_list, ncol = 2, outer.margins = 0.02, widths = c(0.5, 0.5))
le_maps

# Save as PDF and PNG
tmap_save(le_maps, filename = paste0(main_text,"figure2.pdf"), 
          width = 14, height = 14, dpi = 600)
tmap_save(le_maps, filename = paste0(main_text,"figure2.png"),
          width = 14, height = 14, units = "in", dpi = 600)


# Figure 3 - Maps of Change in LE/Posterior Probability -------------------

# Load data created above
load(file = "Quarto/Data/le_birth_change.Rdata")
load(file=paste0(path_onedrive, "le_birth_post_prob_dec.R"))

# Change in LE at birth maps
le_change_sf <- le_birth_change %>%
  mutate(med = round(med,2)) %>%
  st_as_sf()

# Posterior probability maps
post_prob_sf <- le_birth_post_prob_dec %>%
  st_as_sf()

# Save for Quarto doc
save(le_change_sf, file = "Quarto/Data/le_change_sf.Rdata")
save(post_prob_sf, file = "Quarto/Data/post_prob_sf.Rdata")

# Initialize lists to store maps
le_change_maps_list <- list()
post_prob_maps_list <- list()

# Number of breaks for legend
n_breaks_le_change <- 5

for (i in unique(le_change_sf$sex)) {
  
  # Filter LE change data
  data_i <- le_change_sf %>% filter(sex == i)
  
  # Breaks for legend
  breaks_le_change <- classIntervals(data_i$med, n = n_breaks_le_change, style = "jenks")$brks
  
  # LE change map
  le_change_maps_list[[i]] <- tm_shape(data_i) + 
    tm_borders(lwd = 0.5) +
    tm_polygons(
      fill = "med", 
      fill.scale = tm_scale_intervals(
        style = "fixed",
        values = colorRampPalette(c("red", "white", "green"))(n_breaks_le_change),
        breaks = breaks_le_change, 
        label.format = list(text.separator = "to"),
        label.na = "NA"),
      fill.legend = tm_legend(title = "Change in LE \nat Birth (Years)"),
      id = "GEOID"
    ) +
    tm_title(i) +
    tm_layout(
      title.position = c('center', 'top'),
      title.size = 2,
      inner.margins = c(0.02, 0.02, 0.02, 0.25),
      legend.position = c('right', 'bottom'), 
      legend.title.size = 1.4, 
      legend.text.size = 1.4
    )
  
  # Posterior probability data
  post_prob_i <- post_prob_sf %>% filter(sex == i)
  
  # Posterior probability map
  post_prob_maps_list[[i]] <- tm_shape(post_prob_i) + 
    tm_borders(lwd = 0.5) +
    tm_polygons(
      fill = "prob_decr", 
      fill.scale = tm_scale_intervals(
        style = "fixed",
        breaks = c(0, 5, 10, 25, 75, 90, 95, 100),
        values = brewer.pal(7, "RdBu")[7:1], 
        label.format = list(text.separator = "to"),
        label.na = "NA"),
      fill.legend = tm_legend(title = "Posterior Probability \nof Decrease in LE (%)"),
      id = "GEOID"
    ) +
    tm_title(i) +
    tm_layout(
      title.position = c('center', 'top'),
      title.size = 2,
      inner.margins = c(0.02, 0.02, 0.02, 0.25),
      legend.position = c('right', 'bottom'), 
      legend.title.size = 1.4, 
      legend.text.size = 1.4
    )
}


# Create maps for all sex x period combinations
# for (i in unique(le_change_sf$sex)) {
#   
#   # Filter le change data
#   data_i <- le_change_sf %>% filter(sex == i)
#   
#   # Map numerical value of change
#   le_change_maps_list[[i]] <- tm_shape(data_i) + 
#     tm_borders(lwd = 0.5) +
#     tm_polygons(
#       fill = "med", 
#       fill.scale = tm_scale_intervals(style = "jenks", 
#                                       values = colorRampPalette(c("red", "white", "green"))(5),
#                                       label.format = list(
#                                         text.separator = "to"),
#                                       label.na = "NA"), 
#       fill.legend = tm_legend(title = "Change in LE \nat Birth (Years)"),
#       id = "GEOID") +
#     tm_title(i) +
#     tm_layout(
#       title.position = c('center', 'top'),
#       title.size = 2,
#       inner.margins = c(0.02, 0.02, 0.02, 0.25),
#       legend.position = c('right', 'bottom'), 
#       legend.title.size = 1.4, 
#       legend.text.size = 1.4)
#   
#   # Filter posterior probability data 
#   post_prob_i <- post_prob_sf %>% filter(sex == i)
#   
#   # Maps of posterior probability of a decrease in LE in each census tract
#   post_prob_maps_list[[i]] <- tm_shape(post_prob_i) + 
#     tm_borders(lwd = 0.5) +
#     tm_polygons(
#       fill = "prob_decr", 
#       fill.scale = tm_scale_intervals(style = "fixed", 
#                                       breaks = c(0, 5, 10, 25, 75, 90, 95, 100),
#                                       values = brewer.pal(7, "RdBu")[7:1], 
#                                       label.format = list(text.separator = "to"),
#                                       label.na = "NA"), 
#       fill.legend = tm_legend(title = "Posterior Probability \nof Decrease in LE (%)"),
#       id = "GEOID") +
#     tm_title(i) +
#     tm_layout(
#       title.position = c('center', 'top'),
#       title.size = 2,
#       inner.margins = c(0.02, 0.02, 0.02, 0.25),
#       legend.position = c('right', 'bottom'), 
#       legend.title.size = 1.4, 
#       legend.text.size = 1.4)
#   
# }

# Put maps together
le_change_post_prob_maps_list <- c(le_change_maps_list, post_prob_maps_list)[c(1,3,2,4)]
le_change_post_prob_maps <- tmap_arrange(le_change_post_prob_maps_list, 
                                         ncol = 2, nrow = 2, outer.margins = 0.02)
le_change_post_prob_maps

# Save as PDF and PNG
tmap_save(le_change_post_prob_maps, filename = paste0(main_text,"figure3.pdf"), 
          width = 14, height = 14, dpi = 600)
tmap_save(le_change_post_prob_maps, filename = paste0(main_text,"figure3.png"),
          width = 14, height = 14, units = "in", dpi = 600)


# Figure 4 - LE/Change in LE by MHI Decile (main analysis) ----------------

# Load data
load(file = "Quarto/Data/le_by_decile.Rdata")
load(file = paste0(path_onedrive, "le_by_decile_sensitivity.Rdata"))
load(file = paste0(path_onedrive, "le_change_by_decile.Rdata"))
load(file = paste0(path_onedrive, "le_change_by_decile_sensitivity.Rdata"))

# Plots of LE by MHI decile with SII - main analysis
le_by_decile_plot_sii <- le_by_decile %>% 
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
  facet_wrap(~Sex)
le_by_decile_plot_sii

# Plots of LE by MHI decile with SII - sensitivity analysis
le_by_decile_plot_sii_sensitivity <- le_by_decile_sensitivity %>% 
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
  facet_wrap(~Sex)
le_by_decile_plot_sii_sensitivity

# Plots of change in LE by MHI decile - main analysis
le_change_by_decile_plot <- le_change_by_decile %>% 
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
  facet_wrap(~Sex)
le_change_by_decile_plot

# Stack plots of LE by MHI decile and chage in LE by MHI decile - main analysis
le_by_decile_plot_stacked <- ggarrange(le_by_decile_plot_sii, le_change_by_decile_plot, ncol = 1)

# Save plots as PDF and PNG
ggsave(filename = paste0(main_text,"figure4.pdf"), plot = le_by_decile_plot_stacked, 
       device = "pdf", width = 10, height = 12, dpi = 600)
ggsave(filename = paste0(main_text,"figure4.png"), plot = le_by_decile_plot_stacked, 
       device = "png", width = 10, height = 11, dpi = 600)


# Appendix S4 - Demographics of Vital Statistics and ACS Data -------------

# Load mortality data created in 02-Deaths (from encrypted drive)
load(file = paste0(dir_name, "Philly LE\\", "phila_deaths.Rdata"))
load(file = paste0(dir_name, "Philly LE\\", "phila_death_counts.Rdata"))

# Summary table for age at death in Philadelphia for Appendix S4
phila_age_at_death_summary <- phila_deaths_nolow %>% 
  group_by(years) %>%
  summarise(mean = round(mean(Age_death, na.rm = TRUE), 2), 
            sd = round(sd(Age_death, na.rm = TRUE), 2)) %>%
  mutate(sex = "  Total") %>%
  bind_rows(
    phila_deaths_nolow %>% 
      group_by(years, sex) %>%
      summarise(mean = round(mean(Age_death, na.rm = TRUE), 2), 
                sd = round(sd(Age_death, na.rm = TRUE), 2)) %>%
      mutate(sex=paste0("    ", sex))
  ) %>%
  mutate(mean_sd = paste0(mean, " (", sd, ")")) %>%
  pivot_wider(names_from = years, values_from = mean_sd, values_fill = "") %>%
  select(-c(mean, sd)) %>%
  group_by(sex) %>%
  summarise("2017-2019"=paste(`2017-2019`, collapse = ""), "2020-2022"=paste(`2020-2022`, collapse = "")) %>%
  bind_rows(c(sex = "Average age at death (SD)", "2017-2019" = "", "2020-2022" = ""),
            c(sex = "  Sex", "2017-2019" = "", "2020-2022" = "")) %>%
  rename("Measure" = sex) %>%
  mutate(Measure = factor(Measure, 
                          levels = c("Average age at death (SD)", 
                                     "  Total", 
                                     "  Sex", 
                                     "    Men",
                                     "    Women", 
                                     "    Unknown"))) %>%
  arrange(Measure)

# Summary table of deaths in Philly by year and sex
phila_death_count_summary_sex <- phila_death_count_nolow_unknown %>%
  group_by(years, sex) %>%
  summarise(deaths=sum(deaths)) %>%
  group_by(years) %>%
  mutate(total=sum(deaths), pct=round(deaths/total*100,2),
         deaths_chr=format(deaths, big.mark=",",scientific=FALSE),
         sex=paste0("    ", sex)) %>%
  bind_rows(
    phila_death_count_nolow_unknown %>%
      group_by(years) %>%
      summarise(deaths=sum(deaths), total = sum(deaths)) %>%
      mutate(sex="  Total", pct=round(deaths/total*100,2),
             deaths_chr=format(deaths, big.mark=",",scientific=FALSE))
  ) %>%
  mutate(deaths_pct = paste0(deaths_chr, " (", pct, "%)")) %>%
  pivot_wider(names_from = years, values_from = deaths_pct, values_fill = "") %>%
  select(-c(deaths, pct)) %>%
  group_by(sex) %>%
  summarise("2017-2019"=paste(`2017-2019`, collapse = ""), "2020-2022"=paste(`2020-2022`, collapse = "")) %>%
  bind_rows(c(sex = "Number of deaths (%)", "2017-2019" = "", "2020-2022" = ""),
            c(sex = "  Sex", "2017-2019" = "", "2020-2022" = "")) %>%
  rename("Measure" = sex) %>%
  mutate(Measure = factor(Measure, 
                          levels = c("Number of deaths (%)", 
                                     "  Total", 
                                     "  Sex", 
                                     "    Men", 
                                     "    Women", 
                                     "    Unknown"))) %>%
  arrange(Measure)

# Summary table of race-ethnicity of Philly deaths for Table 1
phila_death_count_summary_racehisp <- phila_deaths_nolow_modified %>%
  filter(!is.na(age)) %>% 
  group_by(years, RACEHISP) %>%
  summarise(deaths=n()) %>%
  group_by(years) %>%
  mutate(total=sum(deaths),
         deaths_chr=format(deaths, big.mark=",",scientific=FALSE), 
         pct = round(deaths/total*100,2),
         deaths_pct = paste0(deaths_chr, " (", pct, "%)"),
         RACEHISP = paste0("    ", RACEHISP)) %>%
  pivot_wider(names_from = years, values_from = deaths_pct, values_fill = "") %>%
  select(-c(deaths, pct)) %>%
  group_by(RACEHISP) %>%
  summarise("2017-2019"=paste(`2017-2019`, collapse = ""), "2020-2022"=paste(`2020-2022`, collapse = "")) %>%
  rename("Measure" = RACEHISP) %>%
  bind_rows(c(Measure = "  Racial-ethnic group", "2017-2019" = "", "2020-2022" = "")) %>%
  mutate(Measure = factor(Measure,
                          levels = c("  Racial-ethnic group", "    Hispanic",  "    Non-Hispanic American Indian, Alaska Native",
                                     "    Non-Hispanic Asian American Pacific Islander", "    Non-Hispanic Black",
                                     "    Non-Hispanic White", "    Non-Hispanic Other", "    Unknown"))) %>%
  arrange(Measure)

# Population weighted MHI and SD - pulling from 03-Population and 06-Analysis files
load(file = paste0(path_onedrive, "phila_pop_seer.rdata"))
load(file = paste0(path_onedrive, "mhi.Rdata"))

#Get average and sd of mhi for Philadelphia
weighted_mhi_phila <- mhi_sensitivity %>%
  inner_join(phila_pop_seer_total_all_fixes) %>%
  group_by(years) %>%
  summarise(mean = round(mean(mhi, na.rm = TRUE), 2), wtd.mean = round(weighted_mean(mhi, w=pop, na.rm = T),2),
            sd = round(sd(mhi, na.rm = TRUE), 2), wtd.sd = round(sqrt(weighted_var(mhi, w=pop, na.rm = T)),2)) %>%
  mutate(wtd_mean_chr = format(wtd.mean, big.mark=",",scientific=FALSE), 
         wtd_sd_chr = format(wtd.sd, big.mark=",",scientific=FALSE),
         wtd_mean_sd = paste0("$", wtd_mean_chr, " (", wtd_sd_chr, ")")) %>%
  pivot_wider(names_from = years, values_from = wtd_mean_sd, values_fill = "") %>%
  select(-c(mean, wtd.mean, sd, wtd.sd, wtd_mean_chr, wtd_sd_chr)) %>%
  summarise_all(~paste(., collapse = "")) %>%
  mutate("Measure" = "Population-weighted MHI (SD)") %>%
  relocate(Measure)

# Appendix S4
appendix_mort_summary <- bind_rows(
  phila_age_at_death_summary,
  phila_death_count_summary_sex,
  phila_death_count_summary_racehisp,
  weighted_mhi_phila
)

# Save to use for Appendix S4
write.csv(appendix_mort_summary, file = paste0(appendix,"S4.csv"), row.names = F)


# Appendix S5 - MHI Maps --------------------------------------------------

# Load Data
load(file=paste0(path_onedrive, "mhi_deciles_sensitivity.Rdata"))

# Convert to sf
mhi_sf <- st_as_sf(mhi_deciles_sensitivity)

# Initialize list to store maps
mhi_maps_list <- list()

# Number of breaks for legend
n_breaks_mhi <- 5

# Create map for each period
for (i in unique(mhi_sf$years)) {
  
  # Filter data for a specific `sex_years`
  data_i <- mhi_sf %>% filter(years == i)
  
  # Breaks for legend
  breaks_mhi <- classIntervals(data_i$mhi, n = n_breaks_mhi, style = "jenks")$brks
  
  mhi_maps_list[[i]] <- tm_shape(data_i) +
    tm_borders(lwd = 0.8) +
    tm_polygons(
      fill = "mhi", 
      fill.scale = tm_scale_intervals(style = "fixed",
                                      breaks = breaks_mhi,
                                      values = brewer.pal(n = n_breaks_mhi, name = "Greens"),
                                      label.format = list(text.separator = "to"),
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
tmap_save(mhi_maps, filename = paste0(appendix,"S5.pdf"), width = 14, height = 8, dpi = 600)
tmap_save(mhi_maps, filename = paste0(appendix,"S5.png"), width = 14, height = 8, dpi = 600)


# Appendix S6 - LE/Change in LE by MHI Decile (Sensitivity) ---------------

# Plots of change in LE by MHI decile - sensitivity analysis
le_change_by_decile_plot_sensitivity <- le_change_by_decile_sensitivity %>% 
  filter(Age == 0) %>%
  ggplot(aes(x=decile_income, y=change)) + 
  geom_line(aes(group=years, color=`MHI Years`))+
  geom_point(aes(color = years))+
  labs(x="Decile of Median Household Income",
       y="Change in LE at Birth (Years)")+
  scale_x_continuous(limits=c(-0.2, 11), breaks=seq(0, 10 , by=2))+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=15),
        axis.title =element_text(color="black", face="bold", size=20),
        strip.text=element_text(color="black", face="bold", size=20),
        strip.background = element_blank(), legend.position = "bottom",
        legend.text = element_text(size=18), legend.title = element_text(size=18)) + 
  facet_wrap(~Sex)
le_change_by_decile_plot_sensitivity

# Stack plots of LE by MHI decile and chage in LE by MHI decile - sensitivity analysis
le_by_decile_plot_stacked_sensitivity <- ggarrange(le_by_decile_plot_sii_sensitivity, le_change_by_decile_plot_sensitivity, ncol = 1)

# Save plots
ggsave(filename = paste0(appendix,"S6.pdf"), plot = le_by_decile_plot_stacked_sensitivity, 
       device = "pdf", width = 10, height = 12, dpi = 600)
ggsave(filename = paste0(appendix,"S6.png"), plot = le_by_decile_plot_stacked_sensitivity, 
       device = "png", width = 10, height = 12, dpi = 600)

