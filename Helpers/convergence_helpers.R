#' Create traceplots of parameters from nimble
#'
#' @param data Dataframe of parameter values with iterations in columns (output of `extract_param()`)
#' @param group_vars Character vector indicating levels at which parameters are grouped
#' @param facet_var Column used to facet traceplots (defaults to `sex_years_lab`)
#' @param value_lab Character string or expression for the y-axis label
#'
#' @returns List of traceplots by groupings

make_traceplots <- function(
  data,
  group_vars,
  facet_var = "sex_years_lab",
  value_lab = "Value"
) {
  iter_cols <- grep("^iter_", names(data), value = TRUE)

  split_data <- data %>%
    group_by(across(all_of(group_vars))) %>%
    group_split()

  keys <- data %>%
    group_by(across(all_of(group_vars))) %>%
    group_keys()

  map2(
    split_data,
    seq_len(nrow(keys)),
    ~ {
      title <- paste(
        names(keys),
        unlist(keys[.y, ]),
        sep = "=",
        collapse = ", "
      )

      plot_df <- map_dfr(
        seq_len(nrow(.x)),
        function(i) {
          tibble(
            iteration = seq_along(iter_cols),
            value = as.numeric(.x[i, iter_cols]),
            facet = .x[[facet_var]][i]
          )
        }
      )

      ggplot(plot_df, aes(iteration, value)) +
        geom_line(alpha = 0.7) +
        facet_wrap(~facet) +
        theme_bw() +
        labs(
          title = title,
          x = "Iteration",
          y = value_lab
        )
    }
  )
}

#' Compute effective sample size (ESS) of nimble parameters
#'
#' @param data Dataframe of parameter values with iterations in columns (output of `extract_param()`)
#'
#' @returns Dataframe including ESS for each parameter

compute_ess <- function(data) {
  iter_cols <- grep("^iter_", names(data), value = TRUE)
  mat <- as.matrix(data[iter_cols])

  ess <- apply(mat, 1, function(x) effectiveSize(mcmc(x)))

  data %>%
    mutate(ess = ess) %>%
    select(-all_of(iter_cols))
}
