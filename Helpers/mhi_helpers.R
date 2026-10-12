#' Create income decile or ventile groupings
#'
#' @param mhi_df Dataframe containing at least `mhi` and `years`
#' @param pop_df Dataframe containing tract x period population totals
#' @param type Character string specifying the income grouping scheme:
#'  `"decile"` (10 groups) or `"ventile"` (20 groups)
#'
#' @returns Dataframe with mhi grouped into deciles or ventiles

add_income_groups <- function(
  mhi_df,
  pop_df,
  type = c("decile", "ventile")
) {
  type <- match.arg(type)
  n_groups <- if (type == "decile") 10 else 20
  name <- paste0(type, "_income")

  mhi_df %>%
    left_join(pop_df, by = c("GEOID", "years")) %>%
    rename(total_pop = pop) %>%
    group_by(years) %>%
    mutate(
      !!name := {
        valid <- total_pop > 0 & !is.na(mhi) & !is.na(total_pop)
        out <- rep(NA_real_, n())
        if (any(valid)) {
          b <- reldist::wtd.quantile(
            mhi[valid],
            q = seq(0, 1, length.out = n_groups + 1),
            weight = total_pop[valid],
            na.rm = TRUE
          )
          b[1] <- -Inf
          b[length(b)] <- Inf
          out[valid] <- as.numeric(cut(mhi[valid], breaks = b, include.lowest = TRUE))
        }
        out
      },
      !!paste0(name, "1") := (.data[[name]] - 1) / (n_groups - 1)
    ) %>%
    ungroup()
}

#' Compute population-weighted summaries by income group
#'
#' @param le_df Dataframe containing posterior samples of LE by income groups (output of `build_le_mhi()`)
#' @param value Unquoted variable name to summarize
#' @param pop_df Dataframe containing population estimates used for weighting
#' @param type Character string specifying whether to use income deciles or ventiles
#'
#' @returns Dataframe containing median and 95% credible interval of population-weighted mean LE by income group

compute_income_summary <- function(
  le_df,
  value,
  pop_df,
  type = c("decile", "ventile")
) {
  type <- match.arg(type)

  value <- rlang::ensym(value)

  income_var <- if (type == "decile") {
    rlang::sym("decile_income")
  } else {
    rlang::sym("ventile_income")
  }

  le_df %>%
    filter(!is.na(!!value)) %>%
    left_join(pop_df, by = c("GEOID", "sex", "years")) %>%
    filter(!is.na(pop), pop > 0) %>%
    group_by(Age, years, sex, iteration, !!income_var) %>%
    summarise(
      wtd_mean = weighted.mean(!!value, w = pop),
      .groups = "drop"
    ) %>%
    group_by(Age, years, sex, !!income_var) %>%
    summarise(
      med = median(wtd_mean),
      lci = quantile(wtd_mean, 0.025),
      uci = quantile(wtd_mean, 0.975),
      .groups = "drop"
    ) %>%
    rename(Sex = sex)
}

#' Plot outcome by income rank
#'
#' @param df Dataframe containing outcome values by income grouping (output of `compute_income_summary()`)
#' @param yvar Unquoted variable name to display on the y-axis
#' @param type Character string specifying whether to use income deciles or ventiles
#' @param color_var Optional column name used to color lines and points
#' @param add_smooth Logical indicating whether to add a linear trend line
#'
#' @returns A ggplot object

plot_income_curve <- function(
  df,
  yvar,
  type = c("decile", "ventile"),
  color_var = NULL,
  add_smooth = TRUE
) {
  type <- match.arg(type)

  yvar <- rlang::ensym(yvar)

  income_var <- if (type == "decile") {
    rlang::sym("decile_income")
  } else {
    rlang::sym("ventile_income")
  }

  if (is.null(color_var)) {
    p <- ggplot(
      df,
      aes(
        x = !!income_var,
        y = !!yvar
      )
    ) +
      geom_line(color = "black") +
      geom_point(color = "black")

    if (add_smooth) {
      p <- p +
        geom_smooth(
          method = "lm",
          se = FALSE,
          linetype = "dashed",
          color = "black"
        )
    }
  } else {
    color_var <- rlang::ensym(color_var)

    p <- ggplot(
      df,
      aes(
        x = !!income_var,
        y = !!yvar,
        color = !!color_var
      )
    ) +
      geom_line(aes(group = !!color_var)) +
      geom_point()

    if (add_smooth) {
      p <- p +
        geom_smooth(
          method = "lm",
          se = FALSE,
          linetype = "dashed",
          aes(group = !!color_var)
        )
    }
  }

  p
}

#' Build a life expectancy by mhi dataset
#'
#' Requires `sex_years` and `set_lowpop_na()` in the global environment.
#'
#' @param le_df Dataframe containing life expectancy estimates (output of `build_le_iter()` or `compute_le_change()`)
#' @param mhi_df Dataframe containing median household income estimates (output of `add_income_groups()`)
#' @param value_col Name of variable that will be changed to NA if GEOID belongs to pre-specified group of census tracts
#' @param is_change Logical indicating whether the input data represent change in life expectancy
#'
#' @returns Dataframe of life expectancy with tract-level mhi

build_le_mhi <- function(
  le_df,
  mhi_df,
  value_col,
  is_change = FALSE
) {
  if (is_change) {
    le_df <- bind_rows(
      le_df %>% mutate(years = "2017-2019"),
      le_df %>% mutate(years = "2020-2022")
    )
  }

  le_df %>%
    inner_join(mhi_df, by = c("GEOID", "years")) %>%
    inner_join(sex_years, by = c("sex", "years")) %>%
    set_lowpop_na(value_col)
}
