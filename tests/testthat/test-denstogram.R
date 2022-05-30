test_that("denstogram do not fail", {


  # This error is trigerred because fillvar has values with cardinality 1
  # and then density fails because cannot calculate it for a sample size of 1
  # TODO: check that and provide more informative error message?
  expect_error(
    denstogram(data = ggplot2::diamonds, xvar = price, fillvar = carat),
    "need at least 2 points to select a bandwidth automatically"
  )

  # Testing that it can deal with grouping variables passed as numeric ones
  expect_error(
    denstogram(
      data = ggplot2::diamonds |> mutate(color = as.numeric(color)),
      xvar = price,
      fillvar = color
    ),
    regexp = NA # If NA, asserts that there should be no errors.
  )


})
