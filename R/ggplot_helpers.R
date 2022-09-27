#' Calculate hjust/vjust to align labels to the end of bars in ggplot2
#'
#' @param x numeric
#' @param pivot numeric
#'
#' @return hjust or vjust
#' @export
#'
#' @examples
#' library(ggplot2)
#' mtcars |>
#' tibble::rownames_to_column() |>
#' ggplot(aes(y = reorder(rowname, mpg), x = mpg, label = mpg)) +
#' geom_col(fill = "deepskyblue") +
#' geom_text(aes(hjust = bar_just(mpg)))
bar_just <- function(x, pivot = 0.5) {

  NULL -> xjust -> xnum -> ymid

  data.frame(xnum = x) |>
    dplyr::mutate(ymid = (max(xnum)-0)*pivot) |>
    dplyr::mutate(xjust = ifelse(xnum < ymid, -0.1, 1.1)) |>
    dplyr::pull(xjust)
}

#' Calculate hjust/vjust to align labels to the end of bars in ggplot2
#'
#' @param x numeric
#' @param pivot numeric
#' @param facet facets to group by
#'
#' @return hjust or vjust
#' @export
bar_just_facet <- function(x, facet, pivot = 0.5) {

  NULL -> xjust -> xnum -> ymid -> xfacet

  data.frame(xnum = x, xfacet = facet) %>%
    dplyr::group_by(xfacet) %>%
    dplyr::mutate(ymid = (max(xnum)-0)*pivot) %>%
    dplyr::mutate(xjust = ifelse(xnum < ymid, -0.1, 1.1)) %>%
    dplyr::pull(xjust)
}


