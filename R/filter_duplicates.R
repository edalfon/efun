#' Filter duplicated row by the columns ...
#'
#' @param data a data.frame-like
#' @param ... columns (unquoted) to consider to identify duplicated rows
#'
#' @return a tibble, subset of data, containing only duplicated rows by the
#' columns indicated by ..., and including an additional column .ndups with the
#' number of duplicated rows for each group
#' The returned data are sorted by the columns, to be able to inspect
#' the duplicated together
#' @export
#'
#' @examples
#' filter_duplicates_dplyr(ggplot2::diamonds, carat, cut, price)
filter_duplicates_dplyr <- function(data, ...) {

  NULL -> .ndups

  data |>
    dplyr::group_by(...) |>
    dplyr::mutate(.ndups = n()) |>
    dplyr::filter(.ndups > 1) |>
    dplyr::arrange(...)
}

#' Filter duplicated row by the columns indicated in by
#'
#' @param data a data.frame-like
#' @param by a character vector indicating the columns to consider to identify
#' duplicated rows
#'
#' @return a data.table, subset of the original data, containing only
#' duplicated rows by the columns indicated in `by`, and including an
#' additional column .ndups with the number of duplicated rows for each group.
#' The returned data are sorted by the columns, to be able to inspect
#' the duplicated together
#' @importFrom data.table `:=`
#' @export
#'
#' @examples
#' filter_duplicates(ggplot2::diamonds, by = c("carat", "cut", "price"))
filter_duplicates <- function(data, by) {

  NULL -> .ndups -> .N

  data_dt <- data.table::as.data.table(data)
  data_dt[, .ndups := .N, by = by]
  data_dups <- data_dt[data_dt$.ndups > 1, ]
  data.table::setorderv(data_dups, cols = by)
  data_dups
}
