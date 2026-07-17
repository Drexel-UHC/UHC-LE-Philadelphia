#' Compute income-based disparities in life expectancy
#'
#' @param le_df Dataframe containing iteration-level life expectancy estimates
#' @param deciles Logical indicating whether disparities should be calculated using the 10th and 90th weighted percentiles (`TRUE`) or using the 5th and 95th weighted percentiles (`FALSE`)
#'
#' @returns A dataframe containing medians and 95% CIs of SII and RII estimates

income_disparities <- function(
  le_df,
  deciles = TRUE
) {

  rank_var <- if (deciles) "decile_income1" else "ventile_income1"
  prefix <- if (deciles) "decile_" else "ventile_"

  # Filter away NA le
  le_df <- le_df %>%
    filter(!is.na(.data[["le"]]))

  summary_vars <- c(
    "Age", "years", "sex",
    "sex_years", "sex_years_lab"
  )

  # SII/RII
  income_ineq <- le_df %>%
    group_by(across(c(all_of(summary_vars), "iteration"))) %>%
    group_modify(~ {
      y <- .x[["le"]]

      model_sii <- lm(
        reformulate(rank_var, response = "y"),
        data = .x
      )

      model_rii <- lm(
        reformulate(rank_var, response = "log(pmax(y, 1e-8))"),
        data = .x
      )

      bind_rows(
        tidy(model_sii) %>%
          filter(term == rank_var) %>%
          mutate(type = "sii"),
        tidy(model_rii) %>%
          filter(term == rank_var) %>%
          mutate(type = "rii")
      ) %>%
        select(estimate, type)
    }) %>%
    pivot_wider(
      names_from = type,
      values_from = estimate
    ) %>%
    group_by(across(all_of(summary_vars))) %>%
    summarise(
      med_sii = median(sii, na.rm = TRUE),
      uci_sii = quantile(sii, 0.975, na.rm = TRUE),
      lci_sii = quantile(sii, 0.025, na.rm = TRUE),
      med_rii = exp(median(rii, na.rm = TRUE)),
      uci_rii = exp(quantile(rii, 0.975, na.rm = TRUE)),
      lci_rii = exp(quantile(rii, 0.025, na.rm = TRUE)),
      .groups = "drop"
    )

  # Formatting for tables
  fmt <- function(x) sprintf("%.2f", round(x, 2))

  out <- income_ineq %>%
    transmute(
      Age = Age,
      Years = years,
      Sex = sex,
      SII = paste0(
        fmt(med_sii), " (",
        fmt(lci_sii), ", ",
        fmt(uci_sii), ")"
      ),
      RII = paste0(
        fmt(med_rii), " (",
        fmt(lci_rii), ", ",
        fmt(uci_rii), ")"
      )
    ) %>% 
    arrange(Age, Sex, Years)

  out
}
