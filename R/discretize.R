
#' Calculate breaks to discretize a numeric vector, using different methods
#'
#' @param x a numeric vector
#' @param n integer indicating the number of intervals/bins desired
#' @param method a character of length 1, indicating the method to use. Possible
#'               values are c("jenks", "isowidth", "quantile")
#'
#' @return a numeric vector of length <= n + 1.
#' @export
#'
#' @examples
#' get_breaks(1:100, 5)
get_breaks <- function(x, n = 5, method = break_methods()) {

  method <- match.arg(method)

  rng <- range(x, na.rm = TRUE, finite = TRUE)

  breaks <- switch (method,
    "jenks" = {BAMMtools::getJenksBreaks(x, k = n + 1)},
    "isowidth" = {seq(rng[1], rng[2], length.out = n + 1)},
    "quantile" = {stats::quantile(x, seq(0, 1, length.out = n + 1),
                                  na.rm = TRUE)}
  )

  unique(breaks)
}

#' List break methods
#' @export
break_methods <- function() {
  c("jenks", "isowidth", "quantile")
}


#' Discretize a numeric vector
#'
#' Wrap base::cut.default to make it convenient to use different binning
#' methods (see get_breaks), and custom functions for labels and number formats
#'
#' @inheritParams get_breaks
#' @param labels_formatter a function to produce the labels to pass to cut
#' @param number_formatter a function to format numbers, before building labels
#' @inheritParams base::cut.default
#'
#' @return a vector as returned by cut, which is a factor, unless labels = FALSE
#'        which results in an integer vector of level codes.
#' @export
#'
#' @examples
#' set.seed(1234567)
#'
#' # quick example
#' set.seed(1234567)
#' discretize(runif(100))
#'
#' # customize binning method
#' set.seed(1234567)
#' discretize(runif(100), method = "quantile")
#'
#' # customize number format
#' set.seed(1234567)
#' discretize(runif(100), number_formatter = scales::percent)
#'
#' # customize labels
#' set.seed(1234567)
#' discretize(runif(100), labels_formatter = label_breaks_interval)
#'
#' set.seed(1234567)
#' discretize(runif(100), labels_formatter = label_breaks_cut)
discretize <- function(x, n = 5, method = break_methods(),
                       labels_formatter = label_breaks_value,
                       number_formatter = scales::number_format(big.mark = ","),
                       ordered_result = TRUE) {

  stopifnot(length(x) > 1)

  breaks <- get_breaks(x, n, method)

  cut.default(
    x,
    breaks = breaks,
    labels = breaks %>% number_formatter() %>% labels_formatter(),
    include.lowest = TRUE,
    ordered_result = ordered_result
  )
}

#' Label breaks as an interval
#'
#' @param breaks a vector
#'
#' @return a vector of length, length(breaks) - 1
#' @export
label_breaks_interval <- function(breaks) {
  paste0(breaks[1:length(breaks) - 1], "-", breaks[2:length(breaks)])
}

#' Label breaks as base::cut would do
#'
#' @param breaks a vector
#'
#' @return a vector of length, length(breaks) - 1
#' @export
label_breaks_cut <- function(breaks) {
  breaks_levels <- paste0("(", breaks[1:length(breaks) - 1],
                          ", ", breaks[2:length(breaks)], "]")
  substr(breaks_levels[1], 1, 1) <- "["
  breaks_levels
}

#' Label breaks using only point values (discarding lowest)
#'
#' @param breaks a vector
#'
#' @return a vector of length, length(breaks) - 1
#' @export
label_breaks_value <- function(breaks) {
  breaks[2:length(breaks)]
}

# TODO: #####
guess_scale <- function(x) {

}

guess_accuracy <- function(x) {

  min_diff <- x %>%
    sort() %>%
    diff() %>%
    min()

  round(log10(1 / min_diff) + 1)
  # TODO: see how the accuracy argument from scales::number do this stuff
}

