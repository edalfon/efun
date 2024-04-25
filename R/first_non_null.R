#' Return the first non-null argument
#'
#' This is an old coalesce functions for its arguments. It was actually
#' called coalesce, but now dplyr has also one of those. So let's just
#' The `first_non_null` function takes any number of arguments and returns the
#' first argument that is not null (i.e., `NA`). If all arguments are null, the
#' function returns `NULL`.
#'
#' @param ... One or more arguments of any type.
#'
#' @return The first non-null argument or `NULL` if all arguments are null.
#'
#' @export
first_non_null <- function(...) {
  for (arg in list(...)) {
    if (!is.null(arg)) {
      return(arg)
    }
  }
  return(NULL)
}
