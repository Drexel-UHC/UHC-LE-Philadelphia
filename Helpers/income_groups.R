#' Create income decile or ventile groupings
#'
#' @param df Dataframe containing at least `mhi` and `years`
#' @param type Character string specifying the income grouping scheme:
#'  `"decile"` (10 groups) or `"ventile"` (20 groups)
#'
#' @return Dataframe with mhi grouped into deciles or ventiles

add_income_groups <- function(
  df,
  type = c("decile", "ventile")
) {
  type <- match.arg(type)

  n_groups <- if_else(type == "decile", 10, 20)
  name <- paste0(type, "_income")

  df %>%
    group_by(years) %>%
    mutate(
      !!name := as.numeric(cut(
        mhi,
        breaks = quantile(
          mhi,
          probs = seq(0, 1, length.out = n_groups + 1),
          na.rm = TRUE
        ),
        include.lowest = TRUE
      )),
      !!paste0(name, "1") :=
        (.data[[name]] - 1) / (n_groups - 1)
    ) %>%
    ungroup()
}

#' Compute population-weighted summaries by income group
#'
#' @param df Dataframe containing posterior samples of LE by income groups
#' @param value Unquoted variable name to summarize
#' @param pop_df Dataframe containing population
#' @param type Character string specifying whether to use income deciles or ventiles
#'
#' @return Dataframe containing median and 95% credible interval of population-weighted mean LE by income group

compute_income_summary <- function(
  df,
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

  df %>%
    filter(!is.na(!!value)) %>%
    inner_join(pop_df, by = c("GEOID", "sex", "years")) %>%
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
#' @param df Dataframe containing outcome values by income grouping
#' @param yvar Unquoted variable name to display on the y-axis
#' @param type Character string specifying whether to use income deciles or ventiles
#' @param color_var Optional unquoted grouping variable used to color lines and points
#' @param add_smooth Logical indicating whether to add a linear trend line
#'
#' @return A ggplot object

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
