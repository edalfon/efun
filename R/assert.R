#' Assert a condition within a data frame
#'
#' The `assert` function evaluates one or more conditions within the context
#' of a data frame. If all conditions hold true (evaluate to `TRUE`), the data
#' frame is returned (so as to seemingly work in dplyr pipelines).
#' Otherwise, an informative error message is printed and
#' the function stops execution. When running interactively, the function
#' opens a View on the data.frame with a column evaluating assertion conditions
#' (to make it easy interactively exploring what went wrong). If not
#' interactively, it prints the rows that fail (resorting to tibble's print
#' method).
#'
#' This is an old function, just adapted to make it work with latests versions
#' of pckgs. In the meantime we have used several different approaches from
#' other packages. From the good-old stopifnot, to packages such as assertthat,
#' assertive, assertr, the great testthat, among others I cannot remember now.
#' Yet, we keep going back to this simple yet convinient approach. So let's
#' put it here in efun, 'cause anyway, which we pull a few functions
#' from here again and again in different projects.
#'
#' @param .data A data frame containing the variables used in the conditions.
#' @param ... One or more expressions to be evaluated within the data frame.
#' @param msg A character vector specifying the message to include in the error
#'   message if the assertion fails.
#'
#' @return The data frame `.data` if all conditions are met. Otherwise, an error
#'   is thrown.
#'
#' @export
assert <- function(.data, ..., msg = "Assertion does not hold") {
  condition_eval <- with(.data, ...)
  if (all(condition_eval)) {
    return(.data)
  } else {
    msg <- paste0(
      msg, ": `", deparse(substitute(...)), "` is false ",
      sum(!condition_eval),
      " out of ", length(condition_eval), " times (",
      sprintf("%.1f%%", 100 * sum(!condition_eval) / length(condition_eval)),
      ")\n"
    )
    cat(msg)

    .assert_fails <- dplyr::mutate(.data, .condition_eval = condition_eval)
    if (rlang::is_interactive()) {
      View(.assert_fails) # TODO: should we sample?
    } else {
      .assert_fails |>
        dplyr::filter(!.condition_eval) |>
        print() # TODO: should we sample?, or just rely on tibble's print?
    }
    stop(msg)
  }
}
