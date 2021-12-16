test_that("summarize_obj returns a 1-row tibble", {

  obj_summary <- summarize_obj(letters)
  expect_true(inherits(obj_summary, c("tbl_df", "data.frame")))
  expect_equal(nrow(obj_summary), 1)

  obj_summary <- summarize_obj(rnorm(1000))
  expect_true(inherits(obj_summary, c("tbl_df", "data.frame")))
  expect_equal(nrow(obj_summary), 1)

  obj_summary <- summarize_obj(sample(c(letters, NA), 1000, TRUE))
  expect_true(inherits(obj_summary, c("tbl_df", "data.frame")))
  expect_equal(nrow(obj_summary), 1)
})

test_that("summarize_obj can handle dates", {
  # taking examples from as.Date
  x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
  y <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
  obj <- c(as.Date(x, "%d%b%Y"), as.Date(y, "%m/%d/%y"))
  obj_summary <- summarize_obj(obj)
  expect_equal(obj_summary$sd, "6,143 days")
})

test_that("summarize_obj can handle NAs, Inf, NaN vectors", {
  # taking examples from as.Date
  x <- c(1/0, 1/0, 1/0, 1/0, 1/0, 1/0, 1/0, 1/0, 1/0, 1/0)
  obj_summary <- summarize_obj(x)
  expect_equal(obj_summary$n_na, length(x))

  x <- as.numeric(letters)
  obj_summary <- summarize_obj(x)
  expect_equal(obj_summary$n_na, length(x))
})


test_that("summarize_df deals ok with several data.frame examples", {

  test_suite <- function(tdf) {
    summdf <- summarize_df(tdf)
    # summary data.frame should have as many row as columns has the original df
    expect_equal(ncol(tdf), nrow(summdf))
    # the column n_obj should be constant and equal to the tdf number of rows
    expect_lte(length(unique(summdf$n_obj)), 1)
    expect_equal(unique(summdf$n_obj), nrow(tdf))

    summdf
  }

  lakers_plus <- lubridate::lakers %>%
    dplyr::mutate(date_lubri = lubridate::ymd(date)) %>%
    dplyr::mutate(datetime_lubri = lubridate::ymd_hm(paste(date, time)))

  test_suite(datasets::mtcars)
  test_suite(datasets::iris)
  test_suite(ggplot2::diamonds)
  test_suite(lakers_plus) # a df with dates and date time vars
  #test_suite(tdf <- data.frame()) # edge case, an empty df

  # haven imported data examples from this post
  # https://josiahparry.com/post/2019-12-14-spss-haven/
  # noise <- haven::read_sav(
  #   "http://staff.bath.ac.uk/pssiw/stats2/noisedata.sav"
  # )
  # test_suite(noise)
  #
  # personality <- haven::read_sav(
  #   "http://staff.bath.ac.uk/pssiw/stats2/personality.sav"
  # )
  # test_suite(personality)
  #
  # nlsw88 <- haven::read_dta('http://www.stata-press.com/data/r15/nlsw88.dta')
  # test_suite(nlsw88)

})


