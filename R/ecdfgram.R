

#' Plot Empirical Cummulative Distribution Function for xvar
#'
#' @param data data to plot
#' @param xvar xvar to plot
#' @param grpvar var to group the data by
#' @param trans transformation to apply from the `scales` package
#' @param xaxis_n_breaks number of breaks on the x_axis
#' @param xaxis_format_args #TODO
#' @param xaxis_title x axis title
#' @param yaxis_title y axis title
#'
#' @return a plotly object
#' @export
#' @md
#'
#' @examples
#' ecdfgram(ggplot2::diamonds, price, color)
ecdfgram <- function(data, xvar, grpvar = NULL,
                     trans = scales::identity_trans(),
                     xaxis_n_breaks = 7,
                     xaxis_title = "",
                     yaxis_title = "Cummulative Frequency",
                     xaxis_format_args = list()) {

  NULL -> x_vals

  ecdfgram_data <- get_ecdfgram_data(data, {{xvar}}, {{grpvar}}, trans) |>
    mutate(text = paste0(
      "<b>Cummulative %:</b> %{y} <br>",
      "<b>Value:</b> ", scales::comma(trans$inverse(x_vals))
    ))

  fig <- plotly::plot_ly(data = ecdfgram_data)

  if (!missing(grpvar)) {
    color_grp <- ecdfgram_data |> dplyr::pull({{grpvar}})
  } else {
    color_grp <- NULL
  }

  # Neet to use magrittr pipe and not base pipe. See a comment in this SO
  # https://stackoverflow.com/questions/72445027/plotly-clashes-with-dplyr-programming
  fig <- fig %>%
    plotly::add_trace(
      x = ~x_vals,
      y = ~y_vals,
      type = "scatter",
      mode = "lines",
      color = color_grp,
      hoveron = "plotly_hover",
      # https://plotly-r.com/controlling-tooltips.html
      hovertemplate = ~text
    )


  # We do not want to use the ticks in the x-axis as automatically generated
  # by plotly, because when there is a transformation, it would show the values
  # transformed and not the original units.
  # Yet, we want:
  # - to have relatively nice tick values (~rounded and so on)
  # - make sure the minimum and maximum values are part of the ticks
  ticks_xaxis <- c(
    trans$inverse(min(ecdfgram_data$x_vals)),
    trans$breaks(trans$inverse(ecdfgram_data$x_vals), n = xaxis_n_breaks),
    trans$inverse(max(ecdfgram_data$x_vals))
  ) |> trans$transform()
  labels_xaxis <- trans$inverse(ticks_xaxis) |>
    scales::comma() # auto add millions and so on

  # https://plotly.com/r/tick-formatting/
  # https://plotly.com/r/reference/layout/xaxis/
  fig <- fig |>
    plotly::layout(
      yaxis = list(title = yaxis_title, tickformat = ".0%"),
      xaxis = list(
        title = xaxis_title,
        ticktext = labels_xaxis,
        tickvals = ticks_xaxis,
        tickmode = "array"
      ),
      # https://plotly.com/r/legend/#positioning-the-legend-inside-the-plot
      legend = list(x = 0.8, y = 0.1)
    )

  fig
}



get_ecdfgram_data <- function(data, xvar, grpvar = NULL,
                              trans = scales::identity_trans()) {

  ecdfgram_data <- data |>
    # we want to have grouping variables as categorical, so if a numeric variable
    # was passed as grpvar, we would rather mutate it to character
    # TODO: check only if it is numeric, so that we do not lose factors,
    # order and so on
    dplyr::mutate(dplyr::across({{grpvar}}, as.character)) |>
    dplyr::mutate(dplyr::across({{xvar}}, trans$transform)) |>
    dplyr::group_by({{grpvar}}) |>
    dplyr::summarise(
      x_vals = stats::knots(stats::ecdf({{xvar}})),
      y_vals = stats::ecdf({{xvar}})(
        stats::knots(stats::ecdf({{xvar}}))
      )
    ) |>
    dplyr::ungroup()

  ecdfgram_data
}

