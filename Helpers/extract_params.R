#' Extract parameter value from nimble results
#'
#' @param nimble_results List containing numeric outputs from nimble model
#' @param param Character variable matching parameter name from nimble model
#' @param index_names Character vector indicating what numbers in parameter names represent
#' @param tract_ids Dataframe crosswalking tract ids with GEOIDs
#' @param sex_years_df Dataframe crosswalking sex x year combinations
#' @param save_path Character input for where results should be saved
#' @param obj_name Character input for object name in environment
#'
#' @return Dataframe of parameter values with iterations in columns

extract_param <- function(
  nimble_results,
  param,
  index_names = NULL,
  tract_ids = NULL,
  sex_years_df = NULL,
  save_path = NULL,
  obj_name = param
) {
  out <- rbindlist(
    lapply(seq_along(nimble_results), function(i) {
      mat <- nimble_results[[i]]$output

      param_cols <- grep(
        paste0("^", param, "(\\[|$)"),
        colnames(mat),
        value = TRUE
      )

      if (length(param_cols) == 0) {
        warning(
          sprintf(
            "No columns found for parameter '%s' in sex_years %s",
            param,
            names(nimble_results)[i]
          )
        )
        return(NULL)
      }

      mat <- mat[, param_cols, drop = FALSE]

      # One row per parameter element, one column per MCMC iteration
      dt <- as.data.table(t(mat))
      setnames(dt, paste0("iter_", seq_len(ncol(dt))))

      # Extract parameter indices
      idx_list <- lapply(
        param_cols,
        function(x) {
          inside <- sub(".*\\[(.*)\\]", "\\1", x)

          as.integer(
            strsplit(inside, ",")[[1]]
          )
        }
      )

      n_idx <- max(lengths(idx_list))

      if (n_idx > 0) {
        idx <- do.call(
          rbind,
          lapply(idx_list, function(x) {
            c(
              as.integer(x),
              rep(NA_integer_, n_idx - length(x))
            )
          })
        )

        if (is.null(index_names)) {
          index_names <- paste0("idx_", seq_len(n_idx))
        }

        stopifnot(length(index_names) == n_idx)

        for (j in seq_len(n_idx)) {
          dt[, (index_names[j]) := idx[, j]]
        }
      }

      # Keep as integer for joins
      dt[, sex_years := as.integer(names(nimble_results)[i])]

      dt
    }),
    fill = TRUE
  )

  out <- as_tibble(out)

  # Optional joins
  if (!is.null(tract_ids) && "tract" %in% names(out)) {
    out <- out %>%
      inner_join(tract_ids, by = "tract")
  }

  if (!is.null(sex_years_df)) {
    out <- out %>%
      inner_join(sex_years_df, by = "sex_years")
  }

  if (param == "lambda" && "age" %in% names(out)) {
    out <- out %>%
      mutate(age = age - 1L)
  }

  # Put all metadata columns before iterations
  iter_cols <- grep("^iter_", names(out), value = TRUE)

  out <- out %>%
    relocate(-all_of(iter_cols))

  # Sort if sex_years exists
  if ("sex_years" %in% names(out)) {
    out <- out %>%
      arrange(sex_years)
  }

  if (!is.null(save_path)) {
    tmp_env <- new.env()
    tmp_env[[obj_name]] <- out

    save(
      list = obj_name,
      file = save_path,
      envir = tmp_env
    )

    cat(
      sprintf(
        "Saved object '%s' to %s\n",
        obj_name,
        save_path
      )
    )
  }

  out
}
