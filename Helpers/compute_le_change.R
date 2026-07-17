#' Compute change in LE between periods
#'
#' @param df List containing numeric outputs from nimble model
#' @param value Character variable matching parameter name from nimble model
#' @param iteration Character vector indicating what numbers in parameter names represent
#'
#' @return Dataframe of parameter values with iterations in columns

compute_le_change <- function(
  le_df,
  value = "le",
  iteration = TRUE
) {
  group_vars <- c("GEOID", "Age", "sex")

  if (iteration) {
    group_vars <- c(group_vars, "iteration")
  }

  le_df %>%
    set_lowpop_na(value) %>%
    pivot_wider(
      names_from = years,
      values_from = all_of(value),
      values_fill = NA
    ) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      t1 = sum(`2017-2019`, na.rm = TRUE),
      t2 = sum(`2020-2022`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(change = t2 - t1) %>%
    set_lowpop_na("change")
}
