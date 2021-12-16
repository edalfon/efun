library(dplyr)
test_that("pg_create_table works", {

  # let's try some tests with duckdb, which is embedded but powerful enough
  # and support mostly similar syntax as Postgres flavor
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  withr::defer({ # https://testthat.r-lib.org/articles/test-fixtures.html
    duckdb::dbDisconnect(con, shutdown=TRUE)
  })

  # Let's just create a table using mtcars csv file and check that
  # the table exists with 0 rows, and that the names are also the same
  create_sql <- pg_create_table(
    con = con,
    file_path = test_path("test-data/mtcars.csv"),
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
    file_path = test_path("test-data/mtcars.csv"),
    table = "mtcars",
    execute = TRUE
  ))

  # But then we can test that if_not_exists works
  # set it to TRUE and now there should be no error
  expect_error(pg_create_table(
    con = con,
    file_path = test_path("test-data/mtcars.csv"),
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
  #   file_path = test_path("test-data/mtcars.csv"),
  #   table = "mtcars",
  #   drop_table = TRUE,
  #   execute = TRUE
  # ), regexp = NA) # If NA, asserts that there should be no errors.


  # con <- DBI::dbConnect(
  #   odbc::odbc(),
  #   driver = "PostgreSQL Unicode(x64)",
  #   database = "sufi",
  #   uid = "postgres",
  #   pwd = "postgres",
  #   # TODO: check this
  #   # The defaults should provide reasonable behavior, in particular a local
  #   # connection for host = NULL. For some DBMS (e.g., PostgreSQL), this is
  #   # different to a TCP/IP connection to localhost.
  #   host = NULL, #,"localhost",
  #   port = 5432,
  #   encoding = "WINDOWS-1252",
  #   # https://cran.r-project.org/web/packages/DBI/vignettes/spec.html
  #   bigint = "numeric"
  # )


  # csvfile <- "\\\\MYCLOUD-LFCT98\\Public\\morbi\\sufi_original\\ANTES_SEL\\I2014_CONT.TXT"
#
#   create_sql <- efun::pg_create_foreign_table(
#     con = con,
#     file_path = csvfile,
#     table = "drop_me_please",
#     sep = ";",
#     colClasses = list(
#       character = c("COD_MUNI", "ACTIVIDAD", "CONSEC", "NUM"),
#       double = c("VALOR", "VALOR_USU"),
#       integer = c("DIAS_ESTAN")
#     ),
#     drop_table = TRUE,
#     execute = TRUE,
#     null = "<NA>"
#   )
#
# DBI::dbGetQuery(con, "SELECT * FROM drop_me_please LIMIT 10")

  # csvfile <- "\\\\MYCLOUD-LFCT98\\Public\\morbi\\sufi_original\\text.txt"
  #
  # ingest_delim_to_postgres(
  #   con = con,
  #   file_path = csvfile,
  #   table = "toyme",
  #   create_table = TRUE,
  #   drop_table = TRUE
  # )


})
