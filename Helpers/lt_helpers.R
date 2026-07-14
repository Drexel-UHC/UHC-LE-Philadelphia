#' Compute life tables from posterior lambda samples
#'
#' @param df Wide-format data frame for a single sex_years group
#' @param type Character input for either single-year or abridged lifetable
#' @return data.table with GEOID, sex_years, Age, ex, iteration

compute_lt <- function(
  df,
  type = c("single", "abridged")
) {
  type <- match.arg(type)

  dt <- as.data.table(df)

  iter_cols <- grep("^iter_", colnames(dt), value = TRUE)

  sx <- dt$sex_years[1]

  split_dt <- split(dt, by = "GEOID", keep.by = TRUE)

  rate_matrices <- lapply(split_dt, function(x) {
    if (type == "single") {
      x <- x[order(age)]
    } else {
      x <- x[order(age_grp)]
    }
    as.matrix(x[, ..iter_cols])
  })

  geo_names <- names(rate_matrices)

  if (type == "single") {
    ages <- sort(unique(dt$age))
  } else {
    ages <- sort(unique(dt$age_grp))
  }

  rbindlist(
    lapply(seq_along(geo_names), function(i) {
      g <- geo_names[i]

      mat <- rate_matrices[[g]]

      if (is.null(mat) || nrow(mat) == 0) {
        return(NULL)
      }

      if (nrow(mat) != length(ages)) {
        stop(sprintf(
          "Age mismatch in GEOID %s: rows=%d, expected=%d",
          g,
          nrow(mat),
          length(ages)
        ))
      }

      rbindlist(
        lapply(seq_len(ncol(mat)), function(j) {
          rates <- mat[, j]

          if (type == "single") {
            lt <- lt_single_mx(
              nMx = rates,
              Age = ages
            )
          } else {
            lt <- lt_abridged(
              nMx = rates,
              Age = ages
            )
          }

          data.table(
            GEOID     = g,
            sex_years = sx,
            Age       = lt$Age,
            ex        = lt$ex,
            iteration = j
          )
        })
      )
    })
  )
}

#' Run life table calculations in parallel across sex_years groups
#'
#' @param lambdas_df Wide-format tibble
#' @param label Character label for runtime message
#' @param workers Number of workers
#' @param save_path Optional save location
#' @param obj_name Character input for object name in global environment
#'
#' @return Named list of data.tables

run_lt_parallel <- function(
  lambdas_df,
  label,
  type = c("single", "abridged"),
  workers = min(
    parallel::detectCores() - 1,
    length(unique(lambdas_df$sex_years))
  ),
  save_path = NULL,
  obj_name = "le_nimble"
) {
  type <- match.arg(type)

  lambdas_split <- lambdas_df %>%
    select(-any_of("geometry")) %>%
    group_split(sex_years)

  names(lambdas_split) <- unique(lambdas_df$sex_years)

  plan(multisession, workers = workers)
  on.exit(plan(sequential))

  start_time <- Sys.time()

  results <- future_map(
    lambdas_split,
    compute_lt,
    type = type,
    .options = furrr_options(seed = TRUE),
    .progress = FALSE
  )

  names(results) <- names(lambdas_split)

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "hours"))
  cat(sprintf("%s lifetable calculations took %.2f hours to run.\n", label, elapsed))

  if (!is.null(save_path)) {
    tmp_env <- new.env()
    tmp_env[[obj_name]] <- results
    save(list = obj_name, file = save_path, envir = tmp_env)
    cat(sprintf("Saved object '%s' to %s\n", obj_name, save_path))
  }

  results
}

#' Build dataframe with lifetable results
#'
#' @param le_list Named list of data.tables
#' @param sex_years_df Dataframe crosswalking sex and years
#' @param save_path Optional save location
#' @param obj_name Character input for object name in global environment
#'
#' @return Dataframe of life expectancy

build_le_iter <- function(
  le_list,
  sex_years_df,
  save_path = NULL,
  obj_name = "le_iter_df"
) {
  le_iter_df <- rbindlist(le_list, use.names = TRUE) %>%
    rename(le = ex) %>%
    inner_join(sex_years_df, by = "sex_years") %>%
    select(GEOID, iteration, Age, sex, years, le) %>%
    filter(Age %in% c(0, 25, 65))

  if (!is.null(save_path)) {
    tmp_env <- new.env()
    tmp_env[[obj_name]] <- le_iter_df
    save(list = obj_name, file = save_path, envir = tmp_env)
    cat(sprintf("Saved object '%s' to %s\n", obj_name, save_path))
  }

  return(le_iter_df)
}

summarise_le_ci <- function(
  df,
  save_path = NULL,
  obj_name = "le_df"
) {
  le_df <- df %>%
    arrange(GEOID, Age, sex, years, iteration) %>%
    group_by(GEOID, Age, sex, years) %>%
    summarise(
      lci = quantile(le, 0.025, na.rm = TRUE),
      med = quantile(le, 0.5, na.rm = TRUE),
      uci = quantile(le, 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  if (!is.null(save_path)) {
    tmp_env <- new.env()
    tmp_env[[obj_name]] <- le_df
    save(list = obj_name, file = save_path, envir = tmp_env)
    cat(sprintf("Saved object '%s' to %s\n", obj_name, save_path))
  }

  return(le_df)
}
