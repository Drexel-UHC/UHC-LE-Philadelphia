# Load Libraries & Data ---------------------------------------------------


rm(list = ls())
library(here)
library(tidyverse)
library(classInt)
library(tidycensus)
library(diagis)
library(sf)
library(tmap)
library(RColorBrewer)
library(ggpubr)
library(priceR)
library(matrixStats)
library(reldist)
library(Hmisc)
library(dineq)
library(broom)
library(data.table)
library(future)
library(furrr)
library(DemoTools)

# Create variable to house directory paths
results <- here("Results")
working_results <- here(results, "Working Results")
main_text <- here(results, "Main Text")
appendix <- here(results, "Appendix")
path_onedrive <- here("Data - Cleansed")
dir_name <- here("//files.drexel.edu/encrypted/SOPH/UHC/SchnakeMahl_PAVitalStat/Data")
quarto <- here("Quarto")

# Load data created in 01-Crosswalks
load(file = here(path_onedrive, "sex_years.RData"))
load(file = here(path_onedrive, "phila_sf.RData"))
load(file = here(path_onedrive, "phila_sf_2020.Rdata"))
load(file = here(path_onedrive, "low_pop_tracts.Rdata"))
load(file = here(path_onedrive, "tract_ids.RData"))
load(file = here(path_onedrive, "phila_tracts_ages_sexes_years.Rdata"))

# All tracts in Philly
phila_tracts <- unique(phila_sf$GEOID)
phila_tracts_2020 <- unique(phila_sf_2020$GEOID)

# Date for file saving/loading
date <- "20260716"

# Load helper functions
source(here("Helpers", "compute_le_change.R"))
source(here("Helpers", "total_disparities.R"))
source(here("Helpers", "income_disparities.R"))
source(here("Helpers", "mhi_helpers.R"))
source(here("Helpers", "lt_helpers.R"))
source(here("Helpers", "print_message.R"))


# Other Helpers -----------------------------------------------------------


# Load data and free unused memory
load_gc <- function(path) {
  load(path, envir = .GlobalEnv)
  invisible(gc())
}

# Set values in low population census tracts to NA
set_lowpop_na <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ if_else(GEOID %in% low_pop, NA_real_, .)))
}

plot_theme <- function(base_size = 15) {
  theme_bw() +
    theme(
      axis.text = element_text(color = "black", size = base_size),
      axis.title = element_text(color = "black", face = "bold", size = base_size),
      strip.text = element_text(color = "black", face = "bold", size = base_size),
      legend.position = "bottom",
      legend.text = element_text(size = base_size),
      legend.title = element_text(size = base_size)
    )
}


# Dataframes for Maps -----------------------------------------------------


# Load LE data calculated in 05-Models
load_gc(here(path_onedrive, paste0("le_df_", date, ".Rdata")))

# Dataframe life expectancy at birth by census tract x sex x years (with spatial structure for maps)
le_birth <- le_df %>%
  filter(Age == 0) %>%
  left_join(sex_years) %>%
  full_join(phila_sf) %>%
  select(GEOID, Age, lci, med, uci, geometry, sex, years, sex_years, sex_years_lab) %>%
  ungroup() %>%
  set_lowpop_na(c("med", "lci", "uci")) %>%
  inner_join(tract_ids) %>%
  arrange(sex_years, tract)

save(le_birth,
  file = here("Quarto", "Data", paste0("le_birth_", date, ".Rdata"))
)


# Summary -----------------------------------------------------------------


# Load LE at birth data created above
load_gc(here("Quarto", "Data", paste0("le_birth_", date, ".Rdata")))

# Median/10th and 90th percentile of life expectancy across tracts
le_summary <- le_birth %>%
  inner_join(phila_pop$pop_tract_sex_total_all_fixes) %>%
  group_by(sex, years, sex_years, sex_years_lab) %>%
  summarise(
    le_med = round(reldist::wtd.quantile(med, q = 0.5, weight = pop, na.rm = TRUE), 2),
    le_P10 = round(reldist::wtd.quantile(med, q = 0.1, weight = pop, na.rm = TRUE), 2),
    le_P90 = round(reldist::wtd.quantile(med, q = 0.9, weight = pop, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Median/10th and 90th percentile of change in life expectancy across tracts
le_change_summary <- le_birth %>%
  rename(le = med) %>%
  compute_le_change(
    value = "le",
    iteration = FALSE
  ) %>%
  inner_join(phila_pop$pop_tract_sex_total_all_fixes %>% filter(years == "2017-2019")) %>%
  group_by(sex) %>%
  summarise(
    change_min = round(min(change, na.rm = TRUE), 2),
    change_med = round(reldist::wtd.quantile(change, q = 0.5, weight = pop, na.rm = TRUE), 2),
    change_P10 = round(reldist::wtd.quantile(change, q = 0.1, weight = pop, na.rm = TRUE), 2),
    change_P90 = round(reldist::wtd.quantile(change, q = 0.9, weight = pop, na.rm = TRUE), 2),
    change_max = round(max(change, na.rm = TRUE), 2),
    .groups = "drop"
  )

# % of Tracts that had a decrease in LE
le_decrease <- le_birth %>%
  rename(le = med) %>%
  compute_le_change(
    value = "le",
    iteration = FALSE
  ) %>%
  inner_join(phila_pop$pop_tract_sex_total_all_fixes %>% filter(years == "2017-2019")) %>%
  mutate(decr = as.numeric(change < 0)) %>%
  group_by(sex) %>%
  summarise(
    pct_decr = weighted_mean(decr, w = pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_decr = round(pct_decr * 100, 2))


# MHI ---------------------------------------------------------------------


# Load 2020-2010 GEOID Crosswalk created in 01-Crosswalks
load_gc(here(path_onedrive, "geoid_crosswalk.Rdata"))

# Load population data created in 03-Population
load_gc(here(path_onedrive, paste0("phila_pop_", date, ".Rdata")))

# Pull MHI from ACS
mhi_raw <- map_dfr(c(2019, 2022), ~ {
  get_acs(
    geography = "tract",
    survey = "acs5",
    state = 42,
    year = .x,
    variables = "B19013_001"
  ) %>%
    transmute(
      GEOID = GEOID,
      year = .x,
      mhi = estimate
    )
}) %>%
  filter((year == 2019 & GEOID %in% phila_tracts) |
    (year == 2022 & GEOID %in% phila_tracts_2020))

# Split raw MHI by year
mhi_2019 <- mhi_raw %>%
  filter(year == 2019)

mhi_2022 <- mhi_raw %>%
  filter(year == 2022)

# Convert 2022 MHI to 2010 boundaries
mhi_2022_to_2010 <- mhi_2022 %>%
  inner_join(geoid_crosswalk, by = c("GEOID" = "GEOID20")) %>%
  # mutate(mhi_weighted = mhi * WEIGHT) %>%
  # mutate(mhi_weighted = weighted.mean(mhi, WEIGHT, na.rm = TRUE)) %>%
  group_by(GEOID10) %>%
  summarise(
    # mhi = sum(mhi_weighted, na.rm = TRUE) / sum(WEIGHT, na.rm = TRUE),
    mhi = weighted.mean(mhi, WEIGHT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(GEOID = GEOID10) %>%
  filter(GEOID %in% phila_tracts)

print_message(
  "Tracts with 0 mhi" =
    sum(mhi_2022_to_2010$mhi == 0, na.rm = TRUE)
)
# Tracts with 0 mhi: 0

# Baseline MHI (use 2019 for both periods)
mhi <- bind_rows(
  mhi_2019 %>%
    mutate(years = "2017-2019"),
  mhi_2019 %>%
    mutate(years = "2020-2022")
) %>%
  select(GEOID, years, mhi) %>%
  set_lowpop_na("mhi")

print_message(
  "Tracts with 0 mhi" = sum(mhi$mhi == 0, na.rm = TRUE),
  "Tracts with NA mhi" = sum(is.na(mhi$mhi))
)

# Tracts with 0 mhi: 0
# Tracts with NA mhi: 24

save(
  mhi,
  file = here(path_onedrive, paste0("mhi_", date, ".Rdata"))
)

# Sensitivity MHI (time varying)
mhi_sensitivity <- bind_rows(
  mhi_2019 %>%
    mutate(years = "2017-2019"),
  mhi_2022_to_2010 %>%
    mutate(years = "2020-2022")
) %>%
  select(GEOID, years, mhi) %>%
  set_lowpop_na("mhi")

print_message(
  "Tracts with 0 mhi" = sum(mhi_sensitivity$mhi == 0, na.rm = TRUE),
  "Tracts with NA mhi" = sum(is.na(mhi_sensitivity$mhi))
)

# Tracts with 0 mhi: 0
# Tracts with NA mhi: 27

save(
  mhi_sensitivity,
  file = here(path_onedrive, paste0("mhi_sensitivity_", date, ".Rdata"))
)

print_message("2019 -> 2022 inflation factor" = adjust_for_inflation(
  1,
  from_date = 2019, to_date = 2022, country = "US"
))
# 2019 -> 2022 inflation factor: 1.144715

inflation_factor <- 1.144715

# Adjust MHI for inflation (to 2022 dollars)
adjust_mhi <- function(df) {
  df %>%
    mutate(
      mhi = if_else(years == "2017-2019", mhi * inflation_factor, mhi)
    )
}

mhi_inflation <- rbind(
  mhi %>%
    filter(years == "2017-2019") %>%
    adjust_mhi(),
  mhi %>%
    filter(years == "2017-2019") %>%
    adjust_mhi() %>%
    mutate(years = "2020-2022")
)

save(
  mhi_inflation,
  file = here(path_onedrive, paste0("mhi_inflation_", date, ".Rdata"))
)

mhi_sensitivity_inflation <- adjust_mhi(mhi_sensitivity)

save(
  mhi_sensitivity_inflation,
  file = here(path_onedrive, paste0("mhi_sensitivity_inflation_", date, ".Rdata"))
)

# Split MHI into deciles
mhi_deciles <- add_income_groups(
  mhi_df = mhi_inflation,
  pop_df = phila_pop$pop_total_all_fixes,
  type = "decile"
)

save(
  mhi_deciles,
  file = here(path_onedrive, paste0("mhi_deciles_", date, ".Rdata"))
)

mhi_deciles_sensitivity <- add_income_groups(
  mhi_df = mhi_sensitivity_inflation,
  pop_df = phila_pop$pop_total_all_fixes,
  type = "decile"
)

save(
  mhi_deciles_sensitivity,
  file = here(path_onedrive, paste0("mhi_deciles_sensitivity_", date, ".Rdata"))
)

# Split MHI into ventiles
mhi_ventiles <- add_income_groups(
  mhi_df = mhi_inflation,
  pop_df = phila_pop$pop_total_all_fixes,
  type = "ventile"
)

save(
  mhi_ventiles,
  file = here(path_onedrive, paste0("mhi_ventiles_", date, ".Rdata"))
)


# LE by Iteration with MHI ------------------------------------------------


# Load LE data calculated in 05-Models
load_gc(here(path_onedrive, paste0("le_iter_df_", date, ".Rdata")))

# Load LE data calculated in 06-Analysis
load_gc(here(path_onedrive, paste0("mhi_deciles_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("mhi_deciles_sensitivity_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("mhi_ventiles_", date, ".Rdata")))

# Deciles
le_iter_deciles <- build_le_mhi(
  le_df = le_iter_df %>%
    filter(Age == 0),
  mhi_df = mhi_deciles,
  value_col = "le",
  is_change = FALSE
)

save(
  le_iter_deciles,
  file = here(path_onedrive, paste0("le_iter_deciles_", date, ".Rdata"))
)

le_iter_deciles_mhi_sensitivity <- build_le_mhi(
  le_df = le_iter_df %>%
    filter(Age == 0),
  mhi_df = mhi_deciles_sensitivity,
  value_col = "le",
  is_change = FALSE
)

save(
  le_iter_deciles_mhi_sensitivity,
  file = here(path_onedrive, paste0("le_iter_deciles_mhi_sensitivity_", date, ".Rdata"))
)

le_iter_change <- compute_le_change(
  le_df = le_iter_df %>%
    filter(Age == 0),
  value = "le",
  iteration = TRUE
)

le_iter_change_deciles <- build_le_mhi(
  le_df = le_iter_change,
  mhi_df = mhi_deciles,
  value_col = "change",
  is_change = TRUE
)

save(
  le_iter_change_deciles,
  file = here(path_onedrive, paste0("le_iter_change_deciles_", date, ".Rdata"))
)

le_iter_change_deciles_mhi_sensitivity <- build_le_mhi(
  le_df = le_iter_change,
  mhi_df = mhi_deciles_sensitivity,
  value_col = "change",
  is_change = TRUE
)

save(
  le_iter_change_deciles_mhi_sensitivity,
  file = here(path_onedrive, paste0("le_iter_change_deciles_mhi_sensitivity_", date, ".Rdata"))
)

# Ventiles
le_iter_ventiles <- build_le_mhi(
  le_df = le_iter_df %>%
    filter(Age == 0),
  mhi_df = mhi_ventiles,
  value_col = "le",
  is_change = FALSE
)

save(
  le_iter_ventiles,
  file = here(path_onedrive, paste0("le_iter_ventiles_", date, ".Rdata"))
)

le_iter_change_ventiles <- build_le_mhi(
  le_df = le_iter_change,
  mhi_df = mhi_ventiles,
  value_col = "change",
  is_change = TRUE
)

save(
  le_iter_change_ventiles,
  file = here(path_onedrive, paste0("le_iter_change_ventiles_", date, ".Rdata"))
)


# Mortality Rate Plots ---------------------------------------------------


# Load mortality rate data created in 03-Mortality-Rates and 05-Models
load_gc(here(dir_name, "Philly LE", paste0("phila_death_rates_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("thetas_med_ci_", date, ".Rdata")))

# Plot estimated rates with credible intervals
rates_plot_ribbon <- thetas_med_ci %>%
  left_join(phila_death_rates$death_rates_age_tract) %>%
  left_join(phila_death_rates$death_rates_age %>%
    select(c("age", "sex", "years", "sex_years", "sex_years_lab", "theta_star"))) %>%
  mutate(
    offset = med - theta_star,
    offset_lci = lci - theta_star,
    offset_uci = uci - theta_star,
    crude = case_when(
      deaths == 0 ~ NA_real_,
      pop == 0 ~ NA_real_,
      TRUE ~ log(deaths / pop)
    ),
    knot = case_when(
      age %in% c(0, 1, 10, 20, 40, 70, 85) ~ age,
      TRUE ~ NA_real_
    ),
    rug = case_when(
      deaths == 0 ~ age,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(GEOID) %>%
  group_map(~ {
    p <- ggplot(.x, aes(x = age)) +
      geom_point(aes(x = knot, y = offset)) +
      geom_rug(aes(x = rug)) +
      geom_line(aes(y = offset)) +
      geom_line(aes(y = med)) +
      geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.3) +
      geom_ribbon(aes(ymin = offset_lci, ymax = offset_uci), alpha = 0.3) +
      geom_line(aes(y = theta_star), color = "red") +
      geom_hline(yintercept = 0) +
      geom_point(pch = 21, fill = NA, color = "black", aes(y = crude)) +
      facet_wrap(~sex_years_lab) +
      labs(title = .y$GEOID, y = "Log Mortality Rate") +
      theme_bw()
    p$GEOID <- .y$GEOID
    p
  })

pdf(file = here(working_results, paste0("rates_plot_ribbon_", date, ".pdf")))
rates_plot_ribbon
dev.off()


# Table 2 - Descriptive Statistics/Disparities ----------------------------


# Load data
load_gc(here(path_onedrive, paste0("le_iter_deciles_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("phila_pop_", date, ".Rdata")))
load_gc(here("Quarto", "Data", paste0("le_birth_", date, ".Rdata")))

# Median LE (with P10–P90)
table2_med_le <- le_birth %>%
  inner_join(phila_pop$pop_tract_sex_total_all_fixes) %>%
  group_by(sex, years, sex_years, sex_years_lab) %>%
  summarise(
    le_med = round(reldist::wtd.quantile(med, q = 0.5, weight = pop, na.rm = TRUE), 2),
    le_P10 = round(reldist::wtd.quantile(med, q = 0.1, weight = pop, na.rm = TRUE), 2),
    le_P90 = round(reldist::wtd.quantile(med, q = 0.9, weight = pop, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    value = sprintf("%.2f [%.2f - %.2f]", le_med, le_P10, le_P90),
    Type = "",
    Measure = "Median LE [P10 - P90]"
  ) %>%
  select(Type, Measure, sex, years, value) %>%
  pivot_wider(
    names_from = c(sex, years),
    values_from = value
  )

# Total disparities
table2_total <- total_disparities(
  le_df = le_iter_deciles,
  pop_df = phila_pop$pop_tract_sex_total_all_fixes
) %>%
  select(Years, Sex, AbsDif, RelDif) %>%
  mutate(
    Type = "Total"
  ) %>%
  pivot_longer(
    cols = c(AbsDif, RelDif),
    names_to = "Measure"
  ) %>%
  pivot_wider(
    names_from = c(Sex, Years),
    values_from = value
  )

# Income disparities
table2_income <- income_disparities(
  le_df = le_iter_deciles,
  pop_df = phila_pop$pop_tract_sex_total_all_fixes,
  deciles = TRUE
) %>%
  mutate(
    Type = "Income-Based"
  ) %>%
  pivot_longer(c(SII, RII), names_to = "Measure") %>% # AbsDif, RelDif,
  pivot_wider(names_from = c(Sex, Years), values_from = value) %>%
  select(-Age)

# Combine tables
table2 <- bind_rows(
  table2_med_le,
  table2_total,
  table2_income
)

write.csv(
  table2,
  file = here(working_results, paste0("table2_", date, ".csv")),
  row.names = FALSE
)

write.csv(
  table2,
  file = here(main_text, "table2.csv"),
  row.names = FALSE
)


# Figure 1 - LE w/ Credible Intervals -------------------------------------


load_gc(here("Quarto", "Data", paste0("le_birth_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("le_iter_change_deciles_", date, ".Rdata")))

# Ordering for points on plots
t1_order <- le_birth %>%
  filter(years == "2017-2019", !is.na(med)) %>%
  group_by(sex) %>%
  mutate(order = rank(med)) %>%
  select(GEOID, sex, order)

# LE
le_plot <- le_birth %>%
  filter(!is.na(med)) %>%
  left_join(t1_order, by = c("GEOID", "sex")) %>%
  ggplot(
    aes(
      x = med,
      y = reorder(med, order),
      colour = years
    )
  ) +
  geom_point() +
  geom_segment(
    aes(
      x = lci,
      xend = uci,
      yend = reorder(med, order)
    ),
    alpha = 0.4
  ) +
  plot_theme() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.placement = "outside"
  ) +
  facet_wrap(~sex, scales = "free_y") +
  scale_x_continuous(
    sec.axis = dup_axis(name = NULL),
    breaks = round(seq(25, 100, by = 5))
  ) +
  labs(x = "LE at Birth (Years)", colour = "Years")

le_birth_change <- le_iter_change_deciles %>%
  filter(Age == 0) %>%
  group_by(GEOID, Age, sex) %>%
  summarise(
    lci = quantile(change, 0.025, na.rm = TRUE),
    med = quantile(change, 0.5, na.rm = TRUE),
    uci = quantile(change, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(sex) %>%
  mutate(order = rank(med)) %>%
  ungroup()

save(
  le_birth_change,
  file = here(path_onedrive, paste0("le_birth_change_", date, ".Rdata"))
)

# Change in LE
change_plot <- le_birth_change %>%
  filter(!is.na(med)) %>%
  ggplot(
    aes(
      x = med,
      y = reorder(med, order)
    )
  ) +
  geom_point() +
  geom_segment(
    aes(
      x = lci,
      xend = uci,
      yend = reorder(med, order)
    ),
    alpha = 0.6
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  plot_theme() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.placement = "outside"
  ) +
  facet_wrap(~sex, scales = "free_y") +
  scale_x_continuous(
    sec.axis = dup_axis(name = NULL),
    breaks = round(
      seq(
        floor(min(le_birth_change$lci, na.rm = T) / 5) * 5,
        ceiling(max(le_birth_change$uci, na.rm = T) / 5) * 5,
        by = 5
      )
    )
  ) +
  labs(x = "Change in LE at Birth")

# Combine plots
figure1 <- ggarrange(le_plot, change_plot, ncol = 1)

ggsave(
  filename = here(main_text, "figure1.png"),
  plot = figure1,
  width = 10,
  height = 12,
  dpi = 600
)


# Figure 2 - Maps of LE at Birth ------------------------------------------


# Load data created above
load_gc(here("Quarto", "Data", paste0("le_birth_", date, ".Rdata")))

# Convert to le_birth dataframe to sf object
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
  breaks_le <- classIntervals(
    na.omit(data_i$med),
    n = n_breaks_le,
    style = "jenks"
  )$brks

  # Create map
  le_maps_list[[i]] <- tm_shape(data_i) +
    tm_borders(lwd = 0.8) +
    tm_polygons(
      fill = "med",
      fill.scale = tm_scale_intervals(
        style = "fixed",
        breaks = breaks_le,
        label.format = list(text.separator = "to"),
        values = brewer.pal(n = n_breaks_le, name = "Blues"),
        label.na = "NA"
      ),
      fill.legend = tm_legend(title = "LE at Birth (Years)"),
      id = "GEOID"
    ) +
    tm_title(i) +
    tm_layout(
      title.position = c("center", "top"),
      title.size = 2,
      inner.margins = c(0.05, 0.05, 0.05, 0.05),
      legend.position = c("right", "bottom"),
      legend.title.size = 1.4,
      legend.text.size = 1.4
    )
}

# Combine maps
le_maps <- tmap_arrange(
  le_maps_list,
  ncol = 2,
  outer.margins = 0.02,
  widths = c(0.5, 0.5)
)

tmap_save(
  le_maps,
  filename = here(main_text, "figure2.png"),
  width = 14,
  height = 14,
  units = "in",
  dpi = 600
)


# Figure 3 - Maps of Change in LE/Posterior Probability -------------------


load_gc(here(path_onedrive, paste0("le_birth_change_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("le_iter_change_deciles_", date, ".Rdata")))

# Convert le_birth_change dataframe to sf object
le_change_sf <- le_birth_change %>%
  inner_join(phila_sf) %>%
  mutate(med = round(med, 2)) %>%
  st_as_sf()

save(
  le_change_sf,
  file = here("Quarto", "Data", paste0("le_change_sf_", date, ".Rdata"))
)

# Initialize lists to store maps
le_change_maps_list <- list()

# Posterior probability of decrease in LE
post_prob_sf <- le_iter_change_deciles %>%
  filter(years == "2017-2019") %>%
  mutate(decr = change < 0) %>%
  group_by(GEOID, sex) %>%
  summarise(
    prob_decr = mean(decr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(prob_decr = round(prob_decr * 100, 2)) %>%
  full_join(phila_sf) %>%
  select(GEOID, sex, prob_decr, geometry) %>%
  set_lowpop_na("prob_decr") %>%
  st_as_sf()

save(
  post_prob_sf,
  file = here("Quarto", "Data", paste0("post_prob_sf_", date, ".Rdata"))
)

# Initialize list to store maps
post_prob_maps_list <- list()

# Number of breaks for legend
n_breaks_le_change <- 5

for (i in unique(le_change_sf$sex)) {
  # Filter LE change data
  data_i <- le_change_sf %>% filter(sex == i)

  # Breaks for legend
  breaks_le_change <- classIntervals(
    na.omit(data_i$med),
    n = n_breaks_le_change,
    style = "jenks"
  )$brks

  # LE change map
  le_change_maps_list[[i]] <- tm_shape(data_i) +
    tm_borders(lwd = 0.5) +
    tm_polygons(
      fill = "med",
      fill.scale = tm_scale_intervals(
        style = "fixed",
        midpoint = 0,
        values = colorRampPalette(c("red", "white", "green"))(n_breaks_le_change),
        breaks = breaks_le_change,
        label.format = list(text.separator = "to"),
        label.na = "NA"
      ),
      fill.legend = tm_legend(title = "Change in LE \nat Birth (Years)"),
      id = "GEOID"
    ) +
    tm_title(i) +
    tm_layout(
      title.position = c("center", "top"),
      title.size = 2,
      inner.margins = c(0.05, 0.05, 0.05, 0.05),
      legend.position = c("right", "bottom"),
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
        label.na = "NA"
      ),
      fill.legend = tm_legend(
        title = "Post. Prob. of a \nDecr. in LE (%)",
        ncol = 2
      ), #
      id = "GEOID"
    ) +
    tm_title(i) +
    tm_layout(
      title.position = c("center", "top"),
      title.size = 2,
      inner.margins = c(0.1, 0.05, 0.05, 0.15),
      legend.position = c("right", "bottom"),
      legend.title.size = 1.4,
      legend.text.size = 1.4
    )
}

# Combine maps
le_change_post_prob_maps <- tmap_arrange(
  c(le_change_maps_list, post_prob_maps_list)[c(1, 3, 2, 4)],
  ncol = 2,
  nrow = 2,
  outer.margins = 0.02
)

tmap_save(
  le_change_post_prob_maps,
  filename = here(main_text, "figure3.png"),
  width = 14,
  height = 14,
  units = "in",
  dpi = 600
)


# Figure 4 - LE/Change in LE by MHI Decile --------------------------------


# Load data
load_gc(here(path_onedrive, paste0("phila_pop_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("le_iter_deciles_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("le_iter_change_deciles_", date, ".Rdata")))

le_by_decile <- compute_income_summary(
  le_df = le_iter_deciles,
  value = le,
  pop_df = phila_pop$pop_tract_sex_total_all_fixes,
  type = "decile"
)

save(
  le_by_decile,
  file = here("Quarto", "Data", paste0("le_by_decile_", date, ".Rdata"))
)

le_change_by_decile <- compute_income_summary(
  le_df = le_iter_change_deciles,
  value = change,
  pop_df = phila_pop$pop_tract_sex_total_all_fixes,
  type = "decile"
) %>%
  mutate(`MHI Years` = years)

save(
  le_change_by_decile,
  file = here(path_onedrive, paste0("le_change_by_decile_", date, ".Rdata"))
)

figure4_le <- le_by_decile %>%
  filter(Age == 0) %>%
  mutate(Years = years) %>%
  plot_income_curve(
    yvar = med,
    type = "decile",
    color_var = "Years"
  ) +
  scale_x_continuous(
    limits = c(-0.2, 11),
    breaks = seq(0, 10, 2)
  ) +
  facet_wrap(~Sex) +
  labs(
    x = "Decile of Median Household Income",
    y = "LE at Birth (Years)"
  ) +
  plot_theme() +
  theme(
    strip.background = element_blank()
  )

figure4_change <- le_change_by_decile %>%
  filter(Age == 0, years == "2017-2019") %>%
  plot_income_curve(
    yvar = med,
    type = "decile",
    add_smooth = FALSE
  ) +
  guides(color = NULL) +
  scale_x_continuous(
    limits = c(-0.2, 11),
    breaks = seq(0, 10, 2)
  ) +
  facet_wrap(~Sex) +
  labs(
    x = "Decile of Median Household Income",
    y = "Change in LE at Birth (Years)"
  ) +
  plot_theme() +
  theme(
    strip.background = element_blank()
  )

# Figure 4
figure4 <- ggarrange(
  figure4_le,
  figure4_change,
  ncol = 1,
  heights = c(1, 0.85)
)

ggsave(
  here(main_text, "figure4.png"),
  figure4,
  width = 10,
  height = 11,
  dpi = 600
)


# Appendix S2 - Population Distribution ----------------------------------


# Load data
load_gc(here(path_onedrive, paste0("phila_pop_", date, ".Rdata")))

# Max population
max_pop_total <- max(phila_pop$pop_tract_sex_total_all_fixes$pop, na.rm = TRUE)/1000

# Distribution of total tract population
pop_tract_hist <- phila_pop$pop_tract_sex_total_all_fixes %>%
  inner_join(sex_years) %>%
  ggplot(aes(x = pop / 1000)) +
  geom_histogram(
    bins = 30,
    colour = "black",
    fill = "lightblue"
  ) +
  labs(
    x = "Population (thousands)",
    y = "Count"
  ) +
  facet_wrap(~sex_years_lab) +
  scale_x_continuous(
    breaks = seq(2, max_pop_total, by = 2)
  ) +
  plot_theme(20)

ggsave(
  filename = here(appendix, "S2a.png"),
  plot = pop_tract_hist,
  width = 10,
  height = 10,
  dpi = 600
)

# Distribution of single-age tract population
pop_tract_age_hist <- phila_pop$pop_age_all_fixes %>%
  inner_join(sex_years) %>%
  ggplot(aes(x = pop)) +
  geom_histogram(
    bins = 30,
    colour = "black",
    fill = "lightblue"
  ) +
  labs(
    x = "Population",
    y = "Count"
  ) +
  facet_wrap(~sex_years_lab) +
  scale_x_continuous(
    limits = c(0, 300),
    breaks = seq(0, 300, by = 50)
  ) +
  plot_theme(20)

ggsave(
  filename = here(appendix, "S2b.png"),
  plot = pop_tract_age_hist,
  width = 10,
  height = 10,
  dpi = 600
)


# Appendix S4 - Demographics of Vital Statistics and ACS Data -------------


# Load mortality data created in 02-Deaths
load_gc(here(dir_name, "Philly LE", paste0("phila_deaths_", date, ".Rdata")))
load_gc(here(dir_name, "Philly LE", paste0("phila_death_counts_", date, ".Rdata")))

# Summary table for age at death in Philadelphia for Appendix S4
phila_age_at_death_summary <- phila_deaths_nolow %>%
  group_by(years) %>%
  summarise(
    mean = sprintf("%.2f", mean(AGE_DEATH, na.rm = TRUE)),
    sd = sprintf("%.2f", sd(AGE_DEATH, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(sex = "  Total") %>%
  bind_rows(
    phila_deaths_nolow %>%
      group_by(years, sex) %>%
      summarise(
        mean = sprintf("%.2f", mean(AGE_DEATH, na.rm = TRUE)),
        sd = sprintf("%.2f", sd(AGE_DEATH, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(sex = paste0("    ", sex))
  ) %>%
  mutate(mean_sd = paste0(mean, " (", sd, ")")) %>%
  pivot_wider(
    names_from = years,
    values_from = mean_sd,
    values_fill = ""
  ) %>%
  select(-c(mean, sd)) %>%
  group_by(sex) %>%
  summarise(
    "2017-2019" = paste(`2017-2019`, collapse = ""),
    "2020-2022" = paste(`2020-2022`, collapse = ""),
    .groups = "drop"
  ) %>%
  bind_rows(
    c(sex = "Average age at death (SD)", "2017-2019" = "", "2020-2022" = ""),
    c(sex = "  Sex", "2017-2019" = "", "2020-2022" = "")
  ) %>%
  rename("Measure" = sex) %>%
  mutate(Measure = factor(Measure,
                          levels = c(
                            "Average age at death (SD)",
                            "  Total",
                            "  Sex",
                            "    Men",
                            "    Women",
                            "    Unknown"
                          )
  )) %>%
  arrange(Measure)

# Summary table of deaths in Philly by year and sex
phila_death_count_summary_sex <- phila_death_count_nolow_unknown %>%
  group_by(years, sex) %>%
  summarise(
    deaths = sum(deaths),
    .groups = "drop"
  ) %>%
  group_by(years) %>%
  mutate(
    total = sum(deaths),
    pct = deaths / total * 100,
    deaths_chr = format(deaths, big.mark = ",", scientific = FALSE),
    sex = paste0("    ", sex)
  ) %>%
  bind_rows(
    phila_death_count_nolow_unknown %>%
      group_by(years) %>%
      summarise(
        deaths = sum(deaths),
        total = sum(deaths),
        .groups = "drop"
      ) %>%
      mutate(
        sex = "  Total",
        pct = deaths / total * 100,
        deaths_chr = format(deaths, big.mark = ",", scientific = FALSE)
      )
  ) %>%
  mutate(
    deaths_pct = paste0(
      deaths_chr,
      " (",
      sprintf("%.2f", pct),
      "%)"
    )
  ) %>%
  pivot_wider(
    names_from = years,
    values_from = deaths_pct,
    values_fill = ""
  ) %>%
  select(-c(deaths, pct)) %>%
  group_by(sex) %>%
  summarise(
    "2017-2019" = paste(`2017-2019`, collapse = ""),
    "2020-2022" = paste(`2020-2022`, collapse = ""),
    .groups = "drop"
  ) %>%
  bind_rows(
    c(sex = "Number of deaths (%)", "2017-2019" = "", "2020-2022" = ""),
    c(sex = "  Sex", "2017-2019" = "", "2020-2022" = "")
  ) %>%
  rename("Measure" = sex) %>%
  mutate(Measure = factor(Measure,
                          levels = c(
                            "Number of deaths (%)",
                            "  Total",
                            "  Sex",
                            "    Men",
                            "    Women",
                            "    Unknown"
                          )
  )) %>%
  arrange(Measure)

# Summary table of race-ethnicity of Philly deaths for Table 1
phila_death_count_summary_racehisp <- phila_deaths_nolow_modified %>%
  filter(!is.na(age)) %>%
  group_by(years, racehisp) %>%
  summarise(
    deaths = n(),
    .groups = "drop"
  ) %>%
  group_by(years) %>%
  mutate(
    total = sum(deaths),
    deaths_chr = format(deaths, big.mark = ",", scientific = FALSE),
    pct = sprintf("%.2f", deaths / total * 100),
    deaths_pct = paste0(deaths_chr, " (", pct, "%)"),
    racehisp = paste0("    ", racehisp)
  ) %>%
  pivot_wider(
    names_from = years,
    values_from = deaths_pct,
    values_fill = ""
  ) %>%
  select(-c(deaths, pct)) %>%
  group_by(racehisp) %>%
  summarise(
    "2017-2019" = paste(`2017-2019`, collapse = ""),
    "2020-2022" = paste(`2020-2022`, collapse = ""),
    .groups = "drop"
  ) %>%
  rename("Measure" = racehisp) %>%
  bind_rows(c(Measure = "  Racial-ethnic group", "2017-2019" = "", "2020-2022" = "")) %>%
  mutate(Measure = factor(Measure,
                          levels = c(
                            "  Racial-ethnic group", "    Hispanic", "    Non-Hispanic American Indian, Alaska Native",
                            "    Non-Hispanic Asian American Pacific Islander", "    Non-Hispanic Black",
                            "    Non-Hispanic White", "    Non-Hispanic Other", "    Unknown"
                          )
  )) %>%
  arrange(Measure)

# Population weighted MHI and SD - pulling from 03-Population and 06-Analysis files
load_gc(here(path_onedrive, paste0("phila_pop_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("mhi_sensitivity_inflation_", date, ".Rdata")))

# Get average and sd of mhi for Philadelphia
weighted_mhi_phila <- mhi_sensitivity_inflation %>%
  inner_join(phila_pop$pop_total_all_fixes) %>%
  group_by(years) %>%
  summarise(
    mean = round(mean(mhi, na.rm = TRUE), 2),
    wtd.mean = round(weighted_mean(mhi, w = pop, na.rm = T), 2),
    sd = round(sd(mhi, na.rm = TRUE), 2),
    wtd.sd = round(sqrt(weighted_var(mhi, w = pop, na.rm = T)), 2),
    .groups = "drop"
  ) %>%
  mutate(
    wtd_mean_chr = format(wtd.mean, big.mark = ",", scientific = FALSE),
    wtd_sd_chr = format(wtd.sd, big.mark = ",", scientific = FALSE),
    wtd_mean_sd = paste0("$", wtd_mean_chr, " (", wtd_sd_chr, ")")
  ) %>%
  pivot_wider(
    names_from = years,
    values_from = wtd_mean_sd,
    values_fill = ""
  ) %>%
  select(-c(mean, wtd.mean, sd, wtd.sd, wtd_mean_chr, wtd_sd_chr)) %>%
  summarise_all(~ paste(., collapse = "")) %>%
  mutate("Measure" = "Population-weighted MHI (SD)") %>%
  relocate(Measure)

# Appendix S6
appendix_mort_summary <- bind_rows(
  phila_age_at_death_summary,
  phila_death_count_summary_sex,
  phila_death_count_summary_racehisp,
  weighted_mhi_phila
)

write.csv(
  appendix_mort_summary,
  file = here(appendix, "S4.csv"),
  row.names = FALSE
)


# Appendix S5 - Table 2 with Ventiles ------------------------------------


load_gc(here("Quarto", "Data", paste0("le_birth_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("le_iter_ventiles_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("phila_pop_", date, ".Rdata")))

# Total disparities
total_disp_ventiles <- total_disparities(
  le_df = le_iter_ventiles,
  pop_df = phila_pop$pop_tract_sex_total_all_fixes,
  deciles = FALSE
)

# Income-based disparities
income_disp_vent <- income_disparities(
  le_df = le_iter_ventiles,
  pop_df = phila_pop$pop_tract_sex_total_all_fixes,
  deciles = FALSE
)

# Median LE (with P05–P95)
table2_med_le_vent <- le_birth %>%
  inner_join(phila_pop$pop_tract_sex_total_all_fixes) %>%
  group_by(sex, years, sex_years, sex_years_lab) %>%
  summarise(
    le_med = round(reldist::wtd.quantile(med, q = 0.5, weight = pop, na.rm = TRUE), 2),
    le_P05 = round(reldist::wtd.quantile(med, q = 0.05, weight = pop, na.rm = TRUE), 2),
    le_P95 = round(reldist::wtd.quantile(med, q = 0.95, weight = pop, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    value = sprintf("%.2f [%.2f - %.2f]", le_med, le_P05, le_P95),
    Type = "",
    Measure = "Median LE [P05 - P95]"
  ) %>%
  select(Type, Measure, sex, years, value) %>%
  pivot_wider(
    names_from = c(sex, years),
    values_from = value
  )

# Total disparities
table2_total_disp_vent <- total_disp_ventiles %>%
  filter(Age == 0) %>%
  select(Years, Sex, AbsDif, RelDif) %>%
  mutate(Type = "Total") %>%
  pivot_longer(
    cols = c(AbsDif, RelDif),
    names_to = "Measure"
  ) %>%
  pivot_wider(
    names_from = c(Sex, Years),
    values_from = value
  )

# Income disparities
table2_income_disp_vent <- income_disp_vent %>%
  filter(Age == 0) %>%
  mutate(Type = "Income-Based") %>%
  pivot_longer(
    cols = c(SII, RII),
    names_to = "Measure"
  ) %>%
  pivot_wider(
    names_from = c(Sex, Years),
    values_from = value
  ) %>%
  select(-Age)

# Combine tables
table2_vent <- bind_rows(
  table2_med_le_vent,
  table2_total_disp_vent,
  table2_income_disp_vent
)

write.csv(
  table2_vent,
  here(appendix, "S5.csv"),
  row.names = FALSE
)


# Appendix S6 - Population adjustment sensitivity analysis ---------------------


load_gc(here(path_onedrive, paste0("le_df_no_pop_adjust_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("le_df_", date, ".Rdata")))

le_df_comp <- le_df %>%
  inner_join(le_df_no_pop_adjust, by = c("GEOID", "Age", "sex", "years")) %>%
  rename(
    le = med.x,
    le_no_pop_adjust = med.y
  ) %>%
  inner_join(sex_years) %>%
  filter(Age == 0) %>%
  mutate(diff = le_no_pop_adjust - le)

le_comp <- le_df_comp %>%
  ggplot(
    aes(
      x = le,
      y = le_no_pop_adjust
    )
  ) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = 0,
    col = "red"
  ) +
  facet_wrap(~sex_years_lab) +
  plot_theme() +
  labs(
    x = "Median LE with Population Adjustments",
    y = "Median LE without Population Adjustments"
  )

ggsave(
  filename = here(appendix, "S6.png"),
  plot = le_comp,
  width = 10,
  height = 10,
  units = "in",
  dpi = 600
)


# Appendix S7 - Abridged Lifetables  -------------------------------------------


load_gc(here(path_onedrive, paste0("lambdas_", date, ".Rdata")))
load_gc(here(dir_name, "Philly LE", paste0("phila_death_rates_", date, ".Rdata")))
load(here(path_onedrive, paste0("phila_pop_", date, ".Rdata")))
load_gc(here(path_onedrive, "age_grp.Rdata"))
load_gc(here("Quarto", "Data", paste0("le_birth_", date, ".Rdata")))

n <- phila_death_rates$death_rates_age_tract %>%
  select(tract, GEOID, sex, years, age, pop)

iter_cols <- grep("^iter_", names(lambdas), value = TRUE)

lambdas_abridged <- as.data.table(
  n %>%
    inner_join(age_grp) %>%
    inner_join(lambdas) %>%
    filter(!(GEOID %in% low_pop))
)[
  ,
  {
    pop_sum <- sum(pop)

    vals <- if (pop_sum == 0) {
      lapply(.SD, mean)
    } else {
      lapply(.SD, function(x) sum(x * pop) / pop_sum)
    }

    c(list(pop = pop_sum), vals)
  },
  by = .(
    tract,
    GEOID,
    sex,
    years,
    sex_years,
    sex_years_lab,
    age_grp
  ),
  .SDcols = iter_cols
] %>%
  as.data.frame()

le_abridged <- run_lt_parallel(
  lambdas_abridged,
  label = "Abridged",
  type = "abridged",
  save_path = here(path_onedrive, paste0("le_abridged_", date, ".Rdata")),
  obj_name = "le_abridged"
)

le_abridged_iter_df <- build_le_iter(
  le_list = le_abridged,
  sex_years_df = sex_years,
  save_path = here(path_onedrive, paste0("le_abridged_iter_df_", date, ".Rdata")),
  obj_name = "le_abridged_iter_df"
)

le_abridged_df <- summarise_le_ci(
  df = le_abridged_iter_df,
  save_path = here(path_onedrive, paste0("le_abridged_df_", date, ".Rdata")),
  obj_name = "le_abridged_df"
)

le_birth_comp <- le_abridged_df %>%
  filter(Age == 0) %>%
  rename(
    med_abr = med,
    lci_abr = lci,
    uci_abr = uci
  ) %>%
  inner_join(sex_years) %>%
  inner_join(le_birth) %>%
  ggplot(
    aes(
      x = med,
      y = med_abr
    )
  ) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = 0,
    col = "red"
  ) +
  facet_wrap(~sex_years_lab) +
  labs(
    x = "LE at Birth (Full Lifetable)",
    y = "LE at Birth (Abridged Lifetable)"
  ) +
  plot_theme()

ggsave(
  filename = here(appendix, "S7.png"),
  plot = le_birth_comp,
  width = 10,
  height = 10,
  dpi = 600
)


# Appendix S8 - MHI Maps --------------------------------------------------


# Load Data
load_gc(here(path_onedrive, paste0("mhi_sensitivity_inflation_", date, ".Rdata")))

# Convert dataframe to sf object
mhi_sf <- mhi_sensitivity_inflation %>%
  inner_join(phila_sf) %>%
  st_as_sf()

# Initialize list to store maps
mhi_maps_list <- list()

# Number of breaks for legend
n_breaks_mhi <- 5

# Create map for each period
for (i in unique(mhi_sf$years)) {
  # Filter data for a specific `sex_years`
  data_i <- mhi_sf %>% filter(years == i)

  # Breaks for legend
  breaks_mhi <- classIntervals(
    na.omit(data_i$mhi),
    n = n_breaks_mhi,
    style = "jenks"
  )$brks

  mhi_maps_list[[i]] <- tm_shape(data_i) +
    tm_borders(lwd = 0.8) +
    tm_polygons(
      fill = "mhi",
      fill.scale = tm_scale_intervals(
        style = "fixed",
        breaks = breaks_mhi,
        values = brewer.pal(n = n_breaks_mhi, name = "Greens"),
        label.format = list(text.separator = "to"),
        label.na = "NA"
      ),
      fill.legend = tm_legend(title = "MHI ($)"),
      id = "GEOID"
    ) +
    tm_title(i) +
    tmap_options(component.autoscale = TRUE) +
    tm_layout(
      title.position = c("center", "top"),
      title.size = 2,
      inner.margins = c(0.02, 0.02, 0.02, 0.02),
      legend.position = c("right", "bottom"),
      legend.title.size = 1.5,
      legend.text.size = 1.5
    )
}

# Combine maps
mhi_maps <- tmap_arrange(
  mhi_maps_list,
  ncol = 2,
  outer.margins = 0.02,
  widths = c(0.5, 0.5)
)

tmap_save(
  mhi_maps,
  filename = here(appendix, "S8.png"),
  width = 14,
  height = 8,
  units = "in",
  dpi = 600
)


# Appendix S9 - LE Distribution by Decile ---------------------------------


# Load data
load_gc(here(path_onedrive, paste0("le_iter_deciles_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("phila_pop_", date, ".Rdata")))

le_by_geoid_decile <- le_iter_deciles %>%
  filter(!is.na(le)) %>%
  group_by(Age, years, sex, GEOID, decile_income) %>%
  summarise(
    le = median(le),
    .groups = "drop"
  )

# Plots of LE by MHI decile with SII - main analysis
le_by_decile_boxplots <- le_by_geoid_decile %>%
  mutate(Years = years) %>%
  ggplot(aes(
    x = decile_income,
    y = le
  )) +
  geom_boxplot(
    aes(
      group = interaction(decile_income, years),
      color = Years,
      fill = Years
    ),
    alpha = 0.35
  ) +
  labs(
    x = "Decile of Median Household Income",
    y = "LE at Birth (Years)"
  ) +
  guides(col = NULL) +
  scale_x_continuous(
    limits = c(-0.2, 11),
    breaks = seq(0, 10, by = 2)
  ) +
  plot_theme() +
  theme(
    strip.background = element_blank()
  ) +
  facet_wrap(~sex)

ggsave(
  filename = here(appendix, "S9.png"),
  plot = le_by_decile_boxplots,
  device = "png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 600
)


# Appendix S10 - LE/Change in LE by MHI Decile (Sensitivity) ---------------


# Load time-varying MHI data
load_gc(here(path_onedrive, paste0("le_iter_deciles_mhi_sensitivity_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("le_iter_change_deciles_mhi_sensitivity_", date, ".Rdata")))

le_by_decile_mhi_sensitivity <- compute_income_summary(
  le_df = le_iter_deciles_mhi_sensitivity,
  value = le,
  pop_df = phila_pop$pop_tract_sex_total_all_fixes,
  type = "decile"
)

le_change_by_decile_mhi_sensitivity <- compute_income_summary(
  le_df = le_iter_change_deciles_mhi_sensitivity,
  value = change,
  pop_df = phila_pop$pop_tract_sex_total_all_fixes,
  type = "decile"
) %>%
  mutate(`MHI Years` = years)

# Plot LE by time-varying MHI
le_decile_plot_mhi_sensitivity <- le_by_decile_mhi_sensitivity %>%
  filter(Age == 0, !is.na(decile_income)) %>%
  mutate(Years = years) %>%
  plot_income_curve(
    yvar = med,
    type = "decile",
    color_var = "Years",
    add_smooth = TRUE
  ) +
  scale_x_continuous(limits = c(-0.2, 11), breaks = seq(0, 10, 2)) +
  facet_wrap(~Sex) +
  labs(
    x = "Decile of Median Household Income",
    y = "LE at Birth (Years)"
  ) +
  plot_theme() +
  theme(
    strip.background = element_blank()
  )

# Plot change in LE by time-varying MHI
le_change_decile_plot_mhi_sensitivity <- le_change_by_decile_mhi_sensitivity %>%
  filter(Age == 0, !is.na(decile_income)) %>%
  mutate(Years = years) %>%
  plot_income_curve(
    yvar = med,
    color_var = "Years",
    add_smooth = FALSE
  ) +
  scale_x_continuous(limits = c(-0.2, 11), breaks = seq(0, 10, 2)) +
  facet_wrap(~Sex) +
  labs(
    x = "Decile of Median Household Income",
    y = "Change in LE at Birth (Years)"
  ) +
  plot_theme() +
  theme(strip.background = element_blank())

# Combine plots
mhi_sensitivity_plot <- ggarrange(
  le_decile_plot_mhi_sensitivity,
  le_change_decile_plot_mhi_sensitivity,
  ncol = 1,
  heights = c(1, 0.85)
)

ggsave(
  here(appendix, "S10.png"),
  mhi_sensitivity_plot,
  width = 10,
  height = 11,
  units = "in",
  dpi = 600
)


# Appendix S11 Figure 4 with Ventiles --------------------------------


# Load data
load_gc(here(dir_name, "Philly LE", paste0("phila_death_rates_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("phila_pop_", date, ".Rdata")))
load_gc(here(path_onedrive, paste0("le_iter_change_ventiles_", date, ".Rdata")))

# LE by ventile
le_by_ventile <- compute_income_summary(
  le_df = le_iter_ventiles,
  value = le,
  pop_df = phila_pop$pop_tract_sex_total_all_fixes,
  type = "ventile"
)

# Change in LE by ventile
le_change_by_ventile <- compute_income_summary(
  le_df = le_iter_change_ventiles,
  value = change,
  pop_df = phila_pop$pop_tract_sex_total_all_fixes,
  type = "ventile"
) %>%
  mutate(`MHI Years` = years)

# Plot LE by ventile
appendix_le_ventile <- le_by_ventile %>%
  filter(Age == 0) %>%
  mutate(Years = years) %>%
  plot_income_curve(
    yvar = med,
    type = "ventile",
    color_var = "Years"
  ) +
  scale_x_continuous(limits = c(-0.2, 21), breaks = seq(0, 20, 2)) +
  facet_wrap(~Sex) +
  labs(
    x = "Ventile of Median Household Income",
    y = "LE at Birth (Years)"
  ) +
  plot_theme() +
  theme(
    strip.background = element_blank()
  )

# Plot change in LE by ventile
appendix_change_ventile <- le_change_by_ventile %>%
  filter(Age == 0, years == "2017-2019") %>%
  plot_income_curve(
    yvar = med,
    type = "ventile",
    add_smooth = FALSE
  ) +
  guides(color = NULL) +
  scale_x_continuous(limits = c(-0.2, 21), breaks = seq(0, 20, 2)) +
  facet_wrap(~Sex) +
  labs(
    x = "Ventile of Median Household Income",
    y = "Change in LE at Birth (Years)"
  ) +
  plot_theme() +
  theme(
    strip.background = element_blank()
  )

# Combine plots
appendix_ventiles <- ggarrange(
  appendix_le_ventile,
  appendix_change_ventile,
  ncol = 1,
  heights = c(1, 0.85)
)

ggsave(
  here(appendix, "S11.png"),
  appendix_ventiles,
  width = 10,
  height = 11,
  dpi = 600
)
