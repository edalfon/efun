
#' Find the mode in a vector x
#'
#' @param x a vector
#'
#' @return object of length 1, the same type as x
#' @export
#'
#' @examples
#' modemfv_vctrs(ggplot2::diamonds$cut)
modemfv_vctrs <- function(x) {

  NULL -> key

  x |>
    stats::na.omit() |>
    vctrs::vec_count("count") |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(key)
}


#' Find the mode in a vector x
#'
#' @param x a vector
#' @param na.rm logical to indicate if NA should be removed (defaults to TRUE)
#'
#' @return object of length 1, the same type as x
#' @export
#'
#' @examples
#' modemfv(ggplot2::diamonds$carat)
modemfv <- function(x, na.rm = TRUE) {

  if (isTRUE(na.rm)) {
    x <- stats::na.omit(x)
  }

  unique_values <- unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]
}

