#' Print formatted messages
#'
#' Prints named objects as label-value pairs with consistent formatting.
#'
#' @param ... Named objects to print
#' @param sep Character string used to separate printed lines -- defaults to newline
#'
#' @returns Prints formatted output to the console

print_message <- function(..., sep = "\n") {
  args <- list(...)

  lines <- mapply(
    function(label, value) {
      if (is.numeric(value)) {
        value <- format(value, big.mark = ",", scientific = FALSE)
      }
      sprintf("%s: %s", label, value)
    },
    names(args),
    args,
    USE.NAMES = FALSE
  )

  cat(paste("#", lines, collapse = sep), "\n")
}
