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
#'            message if the assertion fails.
#' @param na.rm passed to `all()`: logical. If true NA values are removed
#'              before the result is computed.
#'
#' @return The data frame `.data` if all conditions are met. Otherwise, an error
#'   is thrown.
#'
#' @export
assert <- function(.data, ..., msg = "Assertion does not hold", na.rm = TRUE) {
  conds <- rlang::enquos(..., .named = TRUE)
  conds_eval <- purrr::imap(conds, \(x, idx) rlang::eval_tidy(x, data = .data))

  if (all(purrr::map_lgl(conds_eval, \(x) all(x, na.rm = TRUE)))) {
    return(.data)
  } else {
    fail_msg <- purrr::imap(conds_eval, \(x, idx) {
      n_fail <- sum(!x)
      if (n_fail > 0) {
        paste0(
          "\t`", idx, "` is false ",
          n_fail, " out of ", length(x), " times (",
          sprintf("%.1f%%", 100 * n_fail / length(x)), ")\n"
        )
      }
    }) |> purrr::compact() # we wanto to get rid of NULL, als error msg

    .assert_fails <- dplyr::mutate(.data, ...)
    if (rlang::is_interactive()) {
      utils::View(.assert_fails) # TODO: should we sample?
    } else {
      .assert_fails |>
        # dplyr::filter(!.condition_eval) |> # TODO: should we keep filtering?
        print() # TODO: should we sample?, or just rely on tibble's print?
    }
    stop(msg, "\n", fail_msg)
  }
}
