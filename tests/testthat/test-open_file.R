test_that("open_file works", {

  file_to_open <- tempfile(fileext = ".txt")
  write("Hello World!", file = file_to_open, append = TRUE)

  testthat::expect_error(open_file(file_to_open), NA)
  
})
