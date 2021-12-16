test_that("get_breaks is robust enough", {

  all_args <- expand.grid(
    x = list(
      runif(100),
      rnorm(100),
      1:2,
      c(1, NA, 2)
    ),
    method = break_methods(),
    n = 1:10,
    stringsAsFactors = FALSE
  ) %>% tibble::tibble()

  purrr::pwalk(all_args, function(x, n, method) {
    breaks <- get_breaks(x, n, method)

    # test that the number of breaks returned not greater than 1 + requested
    # less is possible, e.g., shorter vector than n; vector with no variability
    expect_lte(length(get_breaks(x, !!n, !!method)), !!(n + 1)) #!! ?quasi_label
    #expect_lte(length(breaks), !!(n + 1), label = glue::glue("method: {method}, n: {n}"))

    expect_s3_class(discretize(!!x, !!n, !!method), "factor")
  })

  # all_tests <- all_args %>%
  #   mutate(breaks = purrr::pmap(., get_breaks)) %>%
  #   dplyr::rowwise() %>%
  #   mutate(length = length(breaks))
  #purrr::walk2(all_tests$length, all_tests$n, expect_lte)

})


