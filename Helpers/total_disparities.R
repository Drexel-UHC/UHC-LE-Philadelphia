#' Compute population-weighted total disparity measures
#'
#' @param le_df Dataframe containing iteration-level life expectancy estimates
#' @param pop_df Dataframe containing population estimates at the same level as life expectancy
#' @param deciles Logical indicating whether disparities should be calculated using the 10th and 90th weighted percentiles (`TRUE`) or using the 5th and 95th weighted percentiles (`FALSE`)
#'
#' @returns A dataframe containing median and 95% CIs of population-weighted absolute and relative total life expectancy disparities

total_disparities <- function(
  le_df,
  pop_df,
  deciles = TRUE
) {
  # Remove scientific notation
  options(scipen = 999)

  if (deciles) {
    pctiles <- c(0.1, 0.9)
  } else {
    pctiles <- c(0.05, 0.95)
  }

  le_df %>%
    filter(!is.na(le)) %>%
    inner_join(pop_df) %>%
    group_by(iteration, Age, sex, years, sex_years, sex_years_lab) %>%
    group_modify(~ {
      quants <- reldist::wtd.quantile(x = .x$le, q = pctiles, weight = .x$pop)
      data.frame(
        dif      = quants[2] - quants[1],
        ratio    = quants[2] / quants[1]
      ) %>% as_tibble()
    }) %>%
    ungroup() %>%
    group_by(Age, sex, years, sex_years, sex_years_lab) %>%
    summarise(
      med_dif = as.numeric(quantile(dif, probs = 0.5)),
      uci_dif = as.numeric(quantile(dif, probs = 0.975)),
      lci_dif = as.numeric(quantile(dif, probs = 0.025)),
      med_ratio = as.numeric(quantile(ratio, probs = 0.5)),
      uci_ratio = as.numeric(quantile(ratio, probs = 0.975)),
      lci_ratio = as.numeric(quantile(ratio, probs = 0.025)),
      .groups = "drop"
    ) %>%
    transmute(
      Age    = Age,
      Years  = years,
      Sex    = sex,
      AbsDif = sprintf("%.2f (%.2f, %.2f)", med_dif, lci_dif, uci_dif),
      RelDif = sprintf("%.2f (%.2f, %.2f)", med_ratio, lci_ratio, uci_ratio)
    )
}
