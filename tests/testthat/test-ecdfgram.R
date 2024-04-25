test_that("ecdfgram works", {
  expect_no_error({
    ecdfgram(ggplot2::diamonds, price)
    ecdfgram(ggplot2::diamonds, price, color) |>
      plotly::highlight(
        on = "plotly_hover", off = "plotly_deselect",
        opacityDim = 0.1, color = "black"
      )

    ecdfgram(ggplot2::diamonds, price, color,
      xaxis_title = "Price (not log transformed)"
    )

    get_ecdfgram_data(mtcars, mpg)
  })
})

test_that("get_ecdfgram_data does not throw a warning", {
  # dplyr 1.1.0 decided to revert summarize to "its “safe”
  # behavior of requiring 1 row per group.", so we need to migrate
  # to use reframe
  # https://www.tidyverse.org/blog/2023/02/dplyr-1-1-0-pick-reframe-arrange/
  # https://dplyr.tidyverse.org/reference/reframe.html

  expect_no_warning(get_ecdfgram_data(ggplot2::diamonds, price))
})
