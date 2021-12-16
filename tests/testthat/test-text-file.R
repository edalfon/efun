test_that("file_head and file_tail works", {

  # just a csv version of mtcars, created using
  # readr::write_csv(mtcars, "mtcars.csv")
  src_file <- test_path("test-data/mtcars.csv")

  # file_head ####
  # produce expected output
  expect_vector(file_head(src_file, 10, FALSE), character(), 10)
  expect_vector(file_head(src_file, 10, TRUE), character(), 1)

  # if n < file nlines, should simply return all the file
  expect_vector(file_head(src_file, 100, FALSE), character(), 33)
  expect_vector(file_head(src_file, 100, TRUE), character(), 1)

  # file_tail ####
  # produce expected output
  expect_vector(file_tail(src_file, 10, FALSE), character(), 10)
  expect_vector(file_tail(src_file, 10, TRUE), character(), 1)

  # if n < file nlines, should simply return all the file
  expect_vector(file_tail(src_file, 100, FALSE), character(), 33)
  expect_vector(file_tail(src_file, 100, TRUE), character(), 1)

})
