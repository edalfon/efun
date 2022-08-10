
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
modemfv_vctrs <- function(x) {

  x |>
    na.omit() |>
    vctrs::vec_count("count") |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(key)
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
modemfv <- function(x, na.rm = TRUE) {

  if (isTRUE(na.rm)) {
    x <- na.omit(x)
  }

  unique_values <- unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]
}

testdsf <- function() {


  bench::mark(
    modemfv(ggplot2::diamonds$carat),
    modemfv_vctrs(ggplot2::diamonds$carat)
  )

  bench::mark(
    modemfv(ggplot2::diamonds$cut),
    modemfv_vctrs(ggplot2::diamonds$cut)
  )

}
