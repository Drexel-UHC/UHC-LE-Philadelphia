#' Compute change in LE between periods
#'
#' Requires `set_lowpop_na()` and `low_pop` in the global environment (defined in 06-Analysis).
#'
#' @param le_df Dataframe of iteration-level life expectancy (GEOID, Age, sex, years, iteration) (output of `build_le_iter()`)
#' @param value Name of the life expectancy column
#' @param iteration Logical indicating whether to keep iteration-level results
#'
#' @returns Dataframe with life expectancy in each period and the change between periods

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
