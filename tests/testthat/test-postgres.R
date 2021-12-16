library(dplyr)
test_that("pg_create_table works", {

  # let's try some tests with duckdb, which is embedded but powerful enough
  # and support mostly similar syntax as Postgres flavor
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  withr::defer({ # https://testthat.r-lib.org/articles/test-fixtures.html
    duckdb::dbDisconnect(con, shutdown=TRUE)
  })

  #test_csv_path <- normalizePath(test_path("test-data/mtcars.csv"))

  test_csv_path <- tempfile(fileext = ".csv")
  write.table(mtcars, test_csv_path, row.names = FALSE)

  # Let's just create a table using mtcars csv file and check that
  # the table exists with 0 rows, and that the names are also the same
  create_sql <- pg_create_table(
    con = con,
    file_path = test_csv_path,
    table = "mtcars",
    execute = TRUE
  )
  cnt_res <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS cnt FROM mtcars")
  expect_identical(cnt_res$cnt, 0)
  mtcars_names <- dplyr::tbl(con, "mtcars") %>% collect() %>% names()
  expect_setequal(mtcars_names, names(mtcars))

  # If we try to create it again, it should fail because the table already
  # exists
  expect_error(pg_create_table(
    con = con,
    file_path = test_csv_path,
    table = "mtcars",
    execute = TRUE
  ))

  # But then we can test that if_not_exists works
  # set it to TRUE and now there should be no error
  expect_error(pg_create_table(
    con = con,
    file_path = test_csv_path,
    table = "mtcars",
    if_not_exists = TRUE,
    execute = TRUE
  ), regexp = NA) # If NA, asserts that there should be no errors.

  # we can also test that drop_table works
  # set it to TRUE and now there should be no error, because
  # OK, we cannot test this with duckdb; it "Cannot prepare multiple statements
  # at once!", ..., but Postgres does
  # expect_error(pg_create_table(
  #   con = con,
  #   file_path = test_csv_path,
  #   table = "mtcars",
  #   drop_table = TRUE,
  #   execute = TRUE
  # ), regexp = NA) # If NA, asserts that there should be no errors.


})
