# TODO: document this and add also a ecdf
# TODO: check crosstalk, which is compatible with plotly, to let the user
#       change transformations on the fly
#       https://rstudio.github.io/crosstalk/widgets.html
#       Perhaps also take a look at this
#       https://www.thetidytrekker.com/post/dull-dashboards

utils::globalVariables(c(".", "x", "y", "Info", "x_original", "frac"))

#' Plot a denstogram
#'
#' A denstogram is a density plot, with some more info (the ecdf) to be
#' displayed as a tooltip and the breaks of the axis showing quantiles.
#'
#' @param data data to plot
#' @param xvar var to plot
#' @param fillvar variable to fill denstogram by
#' @param facets_rows variable to facet the denstogram by
#' @param facets_cols variable to facet the denstogram by
#' @param facets_grid_wrap whether to use a "grid" or "wrap" in facetting
#' @param facets_scales passed to facets
#' @param facets_ncol cols for facets
#' @param facets_nrow rows for facets
#' @param trans transformation to apply from the `scales` package
#' @param probs quantiles to include in the plot
#' @param plotly whether to convert it to an interactive plot using plotly
#' @param summary_geom which geom to add to the plot, to display a summary
#' indicator calculated by summary_fn
#' @param summary_fn function to calculate a summary indicator. Must receive
#' one numeric vector and return a numeric vector of length 1
#' @param yaxis_title character vector length 1
#' @param xaxis_title character vector length 1
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
#' denstogram(data = ggplot2::diamonds, xvar = price, facets_rows = cut)
#' denstogram(data = ggplot2::diamonds, xvar = price, facets_rows = cut,
#'            facets_grid_wrap = "wrap")
#' denstogram(data = ggplot2::diamonds, xvar = price,
#'            facets_rows = cut, facets_cols = color)
#' denstogram(data = ggplot2::diamonds, xvar = price,
#'            facets_rows = cut, facets_cols = color, facets_grid_wrap = "wrap")
#' denstogram(data = ggplot2::diamonds, xvar = price, fillvar = cut,
#'            facets_row = color)
#' denstogram(data = ggplot2::diamonds, xvar = price, fillvar = cut,
#'            facets_row = color, facets_grid_wrap = "wrap")
denstogram <- function(
  data,
  xvar,
  fillvar = NULL,
  facets_rows = NULL,
  facets_cols = NULL,
  facets_grid_wrap = c("grid", "wrap"),
  facets_scales = "fixed",
  facets_ncol = NULL,
  facets_nrow = NULL,
  trans = scales::identity_trans(),
  probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1),
  na.rm = TRUE,
  summary_geom = c("none", "line", "text", "both"),
  summary_fn = function(x) stats::median(x, na.rm = na.rm),
  yaxis_title = "Density",
  xaxis_title = "",
  plotly = TRUE
) {

  facets_grid_wrap <- match.arg(facets_grid_wrap)
  summary_geom <- match.arg(summary_geom)

  if (missing(fillvar) & missing(facets_rows) & missing(facets_cols)) {

    density_xvar <- denstogram_data(dplyr::pull(data, {{ xvar }}), trans, na.rm)

    summ_xvar <- data.frame(
      summ_xvar = dplyr::pull(data, {{ xvar }}) |>
        trans$transform() |>
        summary_fn()
    )

  } else {

    density_xvar <- data %>%
      group_by({{facets_rows}}, {{facets_cols}}, {{fillvar}}) %>%
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

    summ_xvar <- data |>
      group_by({{facets_rows}}, {{facets_cols}}, {{fillvar}}) %>%
      dplyr::summarise(summ_xvar = trans$transform(summary_fn({{ xvar }})))
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
    scale_y_continuous(yaxis_title, breaks = 0,
                       expand = expansion(mult = c(0, .05))) +
    scale_x_continuous(
      xaxis_title,
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

  if (!missing(facets_rows) | !missing(facets_cols)) {

    if (facets_grid_wrap == "grid") {

      ggpob <- ggpob + ggplot2::facet_grid(
        rows = vars({{facets_rows}}),
        cols = vars({{facets_cols}}),
        scales = facets_scales
      )

    }
    else {

      ggpob <- ggpob + ggplot2::facet_wrap(
        facets = vars({{facets_rows}}, {{facets_cols}}),
        ncol = facets_ncol,
        nrow = facets_nrow,
        scales = facets_scales
      )

    }
  }

  if (summary_geom == "line" | summary_geom == "both") {
    ggpob <- ggpob +
      ggplot2::geom_vline(
        data = summ_xvar,
        aes(xintercept = summ_xvar),
        color = "white"
      )
  }
  if (summary_geom == "text" | summary_geom == "both") {
    ggpob <- ggpob +
      ggplot2::geom_text(
        data = summ_xvar,
        aes(label = scales::comma(trans$inverse(summ_xvar)),
            y = 0, x = summ_xvar),
        #fontface = "bold",
        vjust = -0.1,
        hjust = -0.1
      )
  }

  if (plotly) {
    ggpob <- plotly::ggplotly(ggpob,
      tooltip = c("label", "fill"),
      dynamicTicks = FALSE
    ) %>% plotly::style(textposition = "top right")
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
