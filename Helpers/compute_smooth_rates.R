#' Compute age-specific mortality rates
#'
#' Requires `sex_years` and `low_pop` in the global environment.
#'
#' @param death_df Dataframe containing death counts by census tract, period, age, and sex
#' @param pop_df Dataframe containing population estimates by census tract, period, age, and sex
#'
#' @returns A list containing `death_rates_age_tract` (dataframe with tract-level mortality and log mortality rates by age and sex) and `death_rates_age` (dataframe with aggregated age-specific mortality and log mortality rates with smoothed log mortality schedules for each sex-period strata)

compute_smooth_rates <- function(
  death_df,
  pop_df
) {
  # Join deaths and population files
  joined_df <- full_join(death_df, pop_df, by = c("GEOID", "sex", "years", "age")) %>%
    filter(!is.na(age))

  # Cap deaths to population
  death_rates_age_tract <- joined_df %>%
    mutate(
      deaths_capped = deaths > pop,
      deaths = case_when(
        deaths > pop ~ floor(pop),
        TRUE ~ deaths
      ),
      rate = deaths / pop,
      log_rate = log(rate)
    ) %>%
    inner_join(sex_years, by = c("sex", "years")) %>%
    arrange(GEOID, sex_years, age)

  # Diagnostics
  n_geo <- death_rates_age_tract %>%
    pull(GEOID) %>%
    unique() %>%
    length()
  n_deaths_capped <- sum(death_rates_age_tract$deaths_capped, na.rm = TRUE)
  deaths_removed <- abs(sum(death_rates_age_tract$deaths, na.rm = TRUE) - sum(joined_df$deaths, na.rm = TRUE))
  deaths_removed_2017 <- abs(sum(filter(death_rates_age_tract, years == "2017-2019")$deaths, na.rm = TRUE) - sum(filter(joined_df, years == "2017-2019")$deaths, na.rm = TRUE))
  deaths_removed_2020 <- abs(sum(filter(death_rates_age_tract, years == "2020-2022")$deaths, na.rm = TRUE) - sum(filter(joined_df, years == "2020-2022")$deaths, na.rm = TRUE))
  low_pop_deaths <- sum((death_rates_age_tract %>% filter(GEOID %in% low_pop))$deaths)
  low_pop_pop <- sum((death_rates_age_tract %>% filter(GEOID %in% low_pop))$pop)
  total_deaths <- sum(death_rates_age_tract$deaths)
  total_pop <- sum(death_rates_age_tract$pop)

  # Print diagnostics
  print_message(
    "unique GEOIDs" = n_geo,
    "GEOID x years x sex x age combinations where deaths were capped" = n_deaths_capped,
    "deaths removed because deaths > pop" = paste0(deaths_removed, " (", round(deaths_removed / (total_deaths + deaths_removed) * 100, 1), "% of total)"),
    "deaths removed because deaths > pop (2017-2019)" = deaths_removed_2017,
    "deaths removed because deaths > pop (2020-2022)" = deaths_removed_2020,
    "deaths in low pop tracts (should be 0)" = low_pop_deaths,
    "pop in low pop tracts (should be 0)" = low_pop_pop,
    "total deaths" = total_deaths,
    "total pop" = total_pop
  )

  # Summarize across GEOIDs
  death_rates_age <- death_rates_age_tract %>%
    group_by(age, sex, years, sex_years) %>%
    summarise(
      pop = sum(pop, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      rate = deaths / pop,
      log_rate = log(rate)
    ) %>%
    inner_join(sex_years, by = c("sex", "years", "sex_years")) %>%
    arrange(sex_years, age)

  # Calculate smooth rates
  theta_star_df <- death_rates_age %>%
    group_by(sex_years) %>%
    group_modify(~ {
      data.frame(
        theta_star = loess(log_rate ~ age, data = .x, degree = 2, span = 0.25)$fitted,
        age = .x$age
      )
    }) %>%
    ungroup()

  # Join smooth rates
  death_rates_age <- death_rates_age %>%
    inner_join(theta_star_df, by = c("age", "sex_years"))

  # Output
  list(
    death_rates_age_tract = death_rates_age_tract,
    death_rates_age = death_rates_age
  )
}
