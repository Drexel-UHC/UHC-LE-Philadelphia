#' Build a life expectancy by mhi dataset
#'
#' @param df Dataframe containing life expectancy estimates
#' @param income_df Dataframe containing median household income estimates
#' @param value_col Unquoted name of variable that will be changed to NA if GEOID belongs to pre-specified group of census tracts
#' @param is_change Logical indicating whetehr the input data represent change in life expectancy
#'
#' @returns

build_le_mhi <- function(
  df,
  income_df,
  value_col,
  is_change = FALSE
) {
  if (is_change) {
    df <- bind_rows(
      df %>% mutate(years = "2017-2019"),
      df %>% mutate(years = "2020-2022")
    )
  }

  df %>%
    inner_join(income_df, by = c("GEOID", "years")) %>%
    inner_join(sex_years, by = c("sex", "years")) %>%
    set_lowpop_na(value_col)
}
