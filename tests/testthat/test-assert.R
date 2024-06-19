test_that("assert() works", {
  expect_error(mtcars |> assert(mpg < 33))

  expect_identical(mtcars |> assert(mpg < 34), mtcars)
})

test_that("assert() does not fail on grouped data frames", {
  expect_error(
    object = mtcars |> dplyr::group_by(cyl) |> assert(hp < 150),
    regexp = "Assertion does not hold"
  )
})

test_that("assert() does not fail testing data with NAs", {
  expect_no_error(
    object = mtcars |>
      dplyr::mutate(mpg = dplyr::na_if(mpg, 15)) |>
      assert(mpg > 10)
  )
})

test_that("assert() can handle multiple conditions", {
  expect_no_error(
    object = mtcars |>
      dplyr::mutate(mpg = dplyr::na_if(mpg, 15)) |>
      assert(hp < 1500, am < 100)
  )

  expect_error(
    object = mtcars |>
      dplyr::mutate(mpg = dplyr::na_if(mpg, 15)) |>
      assert(hp < 1500, am < 1),
    regexp = "Assertion does not hold.*am < 1.*"
  )

  expect_error(
    object = mtcars |>
      dplyr::mutate(mpg = dplyr::na_if(mpg, 15)) |>
      assert(hp < 1, am < 100),
    regexp = "Assertion does not hold.*hp < 1.*"
  )
})
