# TODO: document this and add also a ecdf
# TODO: check crosstalk, which is compatible with plotly, to let the user
#       change transformations on the fly
#       https://rstudio.github.io/crosstalk/widgets.html
#       Perhaps also take a look at this
#       https://www.thetidytrekker.com/post/dull-dashboards

utils::globalVariables(c(".", "x", "y", "Info", "x_original", "frac"))

#' Title
#'
#' @param data data
#' @param xvar var to plot
#' @param fillvar variable to fill denstogram by
#' @param facets variable to facet the denstogram by
#' @param grid_wrap whether to use a "grid" or "wrap" in facetting
#' @param scales passed to facets
#' @param facets_ncol cols for facets
#' @param facets_nrow rows for facets
#' @param trans transformation to apply from the `scales` package
#' @param probs quantiles to include in the plot
#' @param plotly whether to convert it to an interactive plot using plotly
#' @inheritParams stats::density
#'
#' @return ggplot object
#' @export
#' @md
#'
#' @examples
#' denstogram(data = ggplot2::diamonds, xvar = price)
#' denstogram(data = ggplot2::diamonds, xvar = price, fillvar = color)
#' denstogram(data = ggplot2::diamonds, xvar = price, fillvar = cut)
#' denstogram(data = ggplot2::diamonds, xvar = price, facets = cut ~ .)
#' denstogram(data = ggplot2::diamonds, xvar = price, facets = cut ~ .,
#'            grid_wrap = "wrap")
#' denstogram(data = ggplot2::diamonds, xvar = price, facets = cut ~ color)
#' denstogram(data = ggplot2::diamonds, xvar = price, fillvar = cut,
#'            facets = color ~ .)
#' denstogram(data = ggplot2::diamonds, xvar = price, fillvar = cut,
#'            facets = color ~ ., grid_wrap = "wrap")
denstogram <- function(
  data,
  xvar,
  fillvar = NULL,
  facets = NULL,
  grid_wrap = c("grid", "wrap"),
  scales = "fixed",
  facets_ncol = NULL,
  facets_nrow = NULL,
  trans = scales::identity_trans(),
  probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1),
  na.rm = TRUE,
  summ_fn = function(x) stats::median(x, na.rm = na.rm),
  plotly = TRUE
) {

  grid_wrap <- match.arg(grid_wrap)

  if (missing(fillvar) & missing(facets)) {

    density_xvar <- denstogram_data(dplyr::pull(data, {{ xvar }}), trans, na.rm)

    #summ_xvar <- summ_fn(dplyr::pull(data, {{ xvar }}))

  } else {

    eall_vars <- all.vars(stats::as.formula(facets)) # extract the facet vars
    eall_vars <- eall_vars[!(eall_vars %in% c("."))] # excluding the dot

    grouping_enquo <- rlang::syms(eall_vars)

    density_xvar <- data %>%
      group_by(!!!grouping_enquo, {{ fillvar }}) %>%
      # TODO: update code to use new functions in dplyr.
      #       do() is superseded as of dplyr 1.0.0
      dplyr::do(denstogram_data(
        xvar = dplyr::pull(., {{ xvar }}), trans = trans, na.rm = na.rm
      )) %>%
      ungroup() |>
      # Need to discretize fillvar grouping variables passed as numeric,
      # so that the legend and color work
      dplyr::mutate(dplyr::across(
        .cols = {{fillvar}}, #& tidyselect:::where(is.numeric),
        .fns = as.character
      ))
  }

  # build info
  # TODO: make sure it shows until max and min
  # TODO: include number of observations
  # TODO: atomatic decimals f needed, or the full nuber for the max and min

  custom_breaks <- data %>%
    dplyr::pull({{ xvar }}) %>%
    stats::quantile(probs, na.rm = na.rm) %>%
    trans$transform()

  denstogram_breakslabels <- function(xbreaks) {
    paste0(
      "  ", # to make sure long numbers do not look together one larger number
      scales::comma(trans$inverse(xbreaks)),
      "\n[",
      scales::percent(probs, 1),
      "]"
    )
  }

  ggpob <- ggplot2::ggplot(density_xvar, aes(x = x, y = y, label = Info)) +
    theme_bw() +
    scale_y_continuous("", breaks = 0, expand = expansion(mult = c(0, .05))) +
    scale_x_continuous(
      "",
      labels = denstogram_breakslabels,
      breaks = custom_breaks,
      minor_breaks = NULL,
      guide = guide_axis(check.overlap = TRUE)
    )

  if (missing(fillvar)) {
    ggpob <- ggpob +
      ggplot2::geom_area(fill = "blue", alpha = 0.5)
  } else if (!missing(fillvar)) {
    ggpob <- ggpob +
      ggplot2::geom_area(aes(
        fill = {{ fillvar }},
        group = {{ fillvar }}
      ),
      alpha = 0.5
      )
  }

  if (!missing(facets)) {
    if (grid_wrap == "grid") {
      ggpob <- ggpob + ggplot2::facet_grid(facets, scales = scales)
    } # TODO: facets argument is softly deprecated in favor of rows and cols
    #       arguments, that receive a set of variables quoted by vars
    else {
      ggpob <- ggpob + ggplot2::facet_wrap(
        facets = facets, ncol = facets_ncol, nrow = facets_nrow, scales = scales
      )
    }
  }

  if (plotly) {
    ggpob <- plotly::ggplotly(ggpob,
      tooltip = c("label", "fill"),
      dynamicTicks = FALSE
    )
  }

  ggpob
}


denstogram_data <- function(xvar, trans = scales::identity_trans(),
                            na.rm = TRUE) {

  NULL -> nobs

  density_xvar <- xvar %>%
    trans$transform() %>%
    stats::density(cut = 0, na.rm = na.rm)

  density_xvar <- data.frame(x = density_xvar$x, y = density_xvar$y)

  xvar_ecdf <- stats::ecdf(xvar)

  density_xvar <- density_xvar %>%
    mutate(x_original = trans$inverse(x)) %>%
    mutate(frac = xvar_ecdf(x_original)) %>%
    mutate(nobs = frac * length(xvar)) %>%
    mutate(Info = paste0(
      " \n ",
      "x: ", scales::comma(x_original, accuracy = .01),
      " \n ",
      "n: ", scales::comma(nobs, accuracy = 1), " [",
      scales::percent(frac, accuracy = .1), "]"
    ))

  density_xvar
}
