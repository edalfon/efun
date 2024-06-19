test_that("assert works", {
  expect_error(mtcars |> assert(mpg < 33))

  expect_identical(mtcars |> assert(mpg < 34), mtcars)
})

test_that("assert does not fail on grouped data frames", {
  expect_error(
    object = mtcars |> dplyr::group_by(cyl) |> assert(hp < 150),
    regexp = "Assertion does not hold"
  )
})
