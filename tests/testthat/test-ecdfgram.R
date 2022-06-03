
test_that("ecdfgram works", {

  expect_error({

    ecdfgram(ggplot2::diamonds, price)
    ecdfgram(ggplot2::diamonds, price, color) |>
      plotly::highlight(on = "plotly_hover", off = "plotly_deselect",
                        opacityDim = 0.1, color = "black")

    ecdfgram(ggplot2::diamonds, price, color,
             xaxis_title = "Price (not log transformed)")

    get_ecdfgram_data(mtcars, mpg)

  }, regexp = NA)




})


