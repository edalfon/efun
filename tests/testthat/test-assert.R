test_that("assert works", {
  expect_error(mtcars |> assert(mpg < 33))

  expect_identical(mtcars |> assert(mpg < 34), mtcars)
})
