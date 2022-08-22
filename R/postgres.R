# Postgres #####################################################################

# We want to handle raw text files as well as compressed files. For the
# latter, we take advantage of the option program of fil_fdw to use 7z
# to uncompress the file
compressed_exts <- function() {
  c(
    "7z", "bz2", "bzip2", "tbz2", "tbz", "gz", "gzip", "tgz", "tar", "wim",
    "swm", "xz", "txz", "zip", "zipx", "jar", "xpi", "odt", "ods", "docx",
    "xlsx", "epub", "apm", "ar", "a", "deb", "lib", "arj", "cab", "chm",
    "chw", "chi", "chq", "msi", "msp", "doc", "xls", "ppt", "cpio", "dmg",
    "cramfs", "ext", "ext2", "ext3", "ext4", "img", "fat", "img", "hfs",
    "hfsx", "hxs", "hxi", "hxr", "hxq", "hxw", "lit", "ihex", "iso", "img",
    "lzh", "lha", "lzma", "mbr", "mslz", "mub", "nsis", "ntfs", "img", "mbr",
    "rar", "r00", "rpm", "ppmd", "qcow", "qcow2", "qcow2c", "squashfs", "udf",
    "iso", "img", "scap", "uefif", "vdi", "vhd", "vmdk", "wim", "esd",
    "xar", "pkg", "z", "taz"
  )
}

#' Copy data directly from a delimited text-file (csv) to a table in PostgreSQL
#'
#' This is a convenience function meant to be used to import data into a
#' locally-run PostgreSQL server. It basically uses PostgreSQL's COPY command
#' to import the data and by default, it automates the creation of the table
#' taking column names from the header (first row) of the file and guessing
#' column types ala ?data.table::fread
#'
#' This function uses PostgreSQL's COPY FROM filename command.
#' https://www.postgresql.org/docs/current/static/sql-copy.html.
#' According to the documentation, "The file must be accessible to the server
#' and the name must be specified from the viewpoint of the server". Thererfore,
#' this function is intended to be used when you run the PostgreSQL server
#' locally (if you manage to put the file in the server machine, you could also
#' use it, though).
#'
#' - Use UNLOGGED tables and, if possible, create the table and copy command
#' within the same transaction (to improve performance).
#' [See some discussion here](https://nbsoftsolutions.com/blog/disecting-the-postgres-bulk-insert-and-binary-format)
#'
#' - TODO: use also the COPY FROM PROGRAM syntax, to import directly from zip
#' files [https://www.postgresql.org/docs/current/static/sql-copy.html](https://www.postgresql.org/docs/current/static/sql-copy.html)
#'
#' - TODO: also perhaps use named pipes to read from zip files directly
#' http://www.ralree.com/2009/09/04/reading-compressed-files-with-postgres-
#' using-named-pipes/
#' https://stackoverflow.com/questions/41738829/importing-zipped-csv-file-into-postgresql
#'
#'
#' @inheritParams DBI::sqlCreateTable
#' @inheritParams data.table::fread
#' @inheritParams pg_create_table
#' @param quote "quoting character to be used when a data value is quoted"
#' the default value is just a dirty-little-trick to use a value very
#' unlikely to appear
#' @param escape "character that should appear before a data character that
#' matches the QUOTE value"
#' @param null "Specifies the string that represents a null value."
#' @param create_table boolean TRUE if the table should be created. Otherwise,
#' it assumes the table exists
#'
#' @return a dplyr reference to the table
#' @export
#' @md
pg_copy_file <- function(
  con,
  file_path,
  table,
  # arguments to fread
  sep = ";",
  nrows = 10000,
  header = TRUE,
  na.strings = c("", "NA", "NULL"),
  colClasses = NULL,
  col.names,
  encoding = "UTF-8",
  ...,
  unlogged = FALSE,
  drop_table = FALSE,
  cascade = FALSE,
  if_not_exists = FALSE,
  create_table = FALSE,
  execute = FALSE,
  quote = "\b",
  escape = quote,
  null = "NULL"
) {

  tictoc::tic("... Total")

  if (isTRUE(create_table)) {
    create_table_sql <- pg_create_table(
      con = con,
      file_path = file_path,
      table = table,
      # arguments to fread
      sep = sep,
      nrows = nrows,
      header = header,
      na.strings = na.strings,
      colClasses = colClasses,
      col.names = col.names,
      encoding = encoding,
      ...,
      unlogged = unlogged,
      drop_table = drop_table,
      cascade = cascade,
      if_not_exists = if_not_exists,
      execute = FALSE
    )
  } else {
    create_table_sql <- ""
  }

  system2(
    command = 'icacls',
    args = paste0(
      '"', normalizePath(file_path, winslash = '\\'), '"',
      ' /grant *S-1-5-32-545:R'
    ))

  tictoc::tic("... Copy table")

  tryCatch({
    # Put the copy within a transaction where the table was
    # created is even a bit faster than the mere UNLOGGED
    DBI::dbBegin(con)

    query_res <- DBI::dbExecute(con, paste0(create_table_sql, "\n") %T>% cat())

    rows_copy <- DBI::dbExecute(con, glue::glue("
      COPY {DBI::dbQuoteIdentifier(con, table)@.Data}
      FROM '{normalizePath(file_path)}'
      WITH (
        FORMAT 'csv',
        HEADER '{header}',
        DELIMITER E'{sep}',
        QUOTE E'{quote}',
        ESCAPE E'{escape}',
        NULL E'{null}',
        ENCODING '{encoding}'
      );
    ") %T>% cat("\n"))

    DBI::dbCommit(con)

    cat(
      "\n",
      scales::comma(rows_copy),
      " records imported to table '", table, "' in the '", con@info$dbname,
      "' database in PostgreSQL\n\n",
      sep = ""
    )
  }, error = function(e) {
    print(e)
    DBI::dbRollback(con) %T>% cat("ERROR!, Rolling back...")
  })

  tictoc::toc()
  tictoc::toc()

  dplyr::tbl(con, table)
}

#' Compose a SQL query to create a table in a Postgres database, matching
#' the data in a text file
#'
#' Here we simply read a fraction of the file to detect column names and
#' data types, then create the SQL create statement and finally add some
#' modifiers. By default this function does not execute the query.
#'
#' We use `data.table::fread` to guess the data types in the text files
#' `fread` detects all controls automatically (e.g. header, sep), but you
#' can always pass specific values to override the detection. This is handy,
#' for example, to override the type detected for some variables
#' (e.g. a numeric value that you would rather keep as a character).
#' ```
#' colClasses = list(
#'   character = c("municipality_code", "state_code"),
#'   double = c("value"),
#'   integer = c("age")
#' )
#' ```
#' @inheritParams DBI::sqlCreateTable
#' @param file_path path to a text file (csv, tab-delimited, etc.)
#' @inheritParams data.table::fread
#' @param ... other arguments passed to data.table::fread
#' @param unlogged logical, whether to create an UNLOGGED table
#' @param drop_table logical, whether to drop the table before creating it
#' @param cascade logical, whether to add CASCADE to the DROP statement
#' @param if_not_exists logical, to add IF NOT EXISTS to the query
#' @param execute logical, whether to execute the query using `con`
#'
#' @return invisibly returns a character vector (length 1) with the sql
#' statement
#' @export
#' @md
pg_create_table <- function(
  con,
  file_path,
  table,
  # arguments to fread
  sep = "auto",
  nrows = 10000,
  header = "auto",
  na.strings = c("", "NA", "NULL"),
  colClasses = NULL,
  col.names,
  encoding = "UTF-8",
  ...,
  unlogged = FALSE,
  drop_table = FALSE,
  cascade = FALSE,
  if_not_exists = FALSE,
  execute = FALSE
) {

  # We want to first load a fraction of the file to get the structure and
  # data types
  read_glimpse <- data.table::fread(
    # 'cause fread can fail for very large files even though you try to read
    # just the first few lines see
    # https://stackoverflow.com/questions/52492986/fread-to-read-top-n-rows-
    # from-a-large-file
    # cmd = glue::glue(
    #   'powershell Get-content "{normalizePath(file_path)}" -head {nrows}'
    # ),
    text = efun::file_head(file_path, nrows),
    sep = sep,
    nrows = nrows,
    header = header,
    na.strings = na.strings,
    colClasses = colClasses,
    col.names = col.names,
    encoding = encoding,
    ...
  )
  read_glimpse <- janitor::clean_names(read_glimpse)

  # Obtain a Postgres compatible CREATE TABLE sql sentence for the data.frame
  create_table_sql <- DBI::sqlCreateTable(
    con = con,
    table = table,
    fields = read_glimpse,
    row.names = FALSE
  )
  create_table_sql <- create_table_sql@.Data

  # Add modifiers
  if (isTRUE(if_not_exists)) {
    create_table_sql <- stringr::str_replace(
      string = create_table_sql,
      pattern = "CREATE TABLE ",
      replacement = "CREATE TABLE IF NOT EXISTS "
    )
  }
  if (isTRUE(unlogged)) {
    create_table_sql <- stringr::str_replace(
      string = create_table_sql,
      pattern = "CREATE TABLE ",
      replacement = "CREATE UNLOGGED TABLE "
    )
  }
  if (isTRUE(drop_table)) {
    create_table_sql <- paste0(
      "DROP TABLE IF EXISTS ",
      DBI::dbQuoteIdentifier(con, table)@.Data,
      ifelse(isTRUE(cascade), " CASCADE", ""),
      ";\n",
      create_table_sql
    )
  }

  # Finally execute the query
  if (isTRUE(execute)) {
    cat("Executing SQL statement...\n")
    cat(create_table_sql)
    DBI::dbExecute(con, create_table_sql)
  }

  invisible(create_table_sql)
}


#' Create a foreign table in Postgres, to directly access data in a text file
#'
#' The use case for this function is as follows:
#' You get a large file (csv, tab-delimited) and would rather wrangle such
#' data in postgres. Instead of go ahead and copy all the data into a table,
#' let's create a foreign table that you can query just like a regular table.
#'
#' Some notes:
#' - The file must be accesible to the postgres server. This function tries to
#'   edit file's privileges to make sure is readable from the server.
#'   However, if the file is in a network share, that may not be enough because
#'   the server should be able to access the network resource. It is likely
#'   the default user running the server cannot access network shared resources,
#'   so the workaround is to change the user running the server
#'
#' See Postgres documentation:
#' - [Foreign table https://www.postgresql.org/docs/10/sql-createforeigntable
#' .html](https://www.postgresql.org/docs/10/sql-createforeigntable.html)
#' - [Foreign-data wrapper for files https://www.postgresql.org/docs/10/file-
#' fdw.html](https://www.postgresql.org/docs/10/file-fdw.html)
#' - [Options documented in COPY https://www.postgresql.org/docs/10/sql-copy
#' .html](https://www.postgresql.org/docs/10/sql-copy.html)
#'
#' @inheritParams DBI::sqlCreateTable
#' @inheritParams data.table::fread
#' @inheritParams pg_create_table
#' @inheritParams pg_copy_file
#' @param create_foreign_server logical, to create the extension and server
#' for the foreign data wrapper ([https://www.postgresql.org/docs/10/
#' file-fdw.html](https://www.postgresql.org/docs/10/file-fdw.html))
#'
#' @return invisibly returns a character vector (length 1) with the sql
#' statement
#' @export
#' @md
pg_create_foreign_table <- function(
  con,
  file_path,
  table,
  # arguments to fread
  sep = ";",
  nrows = 10000,
  header = TRUE,
  na.strings = c("", "NA", "NULL"),
  colClasses = NULL,
  col.names,
  encoding = "UTF-8",
  ...,
  drop_table = FALSE,
  cascade = FALSE,
  if_not_exists = FALSE,
  create_foreign_server = TRUE,
  execute = FALSE,
  quote = "\b",
  escape = quote,
  null = "NULL"
) {

  create_table_sql <- pg_create_table(
    con = con,
    file_path = file_path,
    table = table,
    # arguments to fread
    sep = sep,
    nrows = nrows,
    header = header,
    na.strings = na.strings,
    colClasses = colClasses,
    col.names = col.names,
    encoding = encoding,
    ...,
    drop_table = FALSE, # because drop table != drop foreign table
    if_not_exists = if_not_exists,
    execute = FALSE
  )

  create_table_sql <- stringr::str_replace(
    string = create_table_sql,
    pattern = "CREATE TABLE ",
    replacement = "CREATE FOREIGN TABLE "
  )

  if (isTRUE(drop_table)) {
    create_table_sql <- paste0(
      "DROP FOREIGN TABLE IF EXISTS ",
      DBI::dbQuoteIdentifier(con, table)@.Data,
      ifelse(isTRUE(cascade), " CASCADE", ""),
      ";\n",
      create_table_sql
    )
  }

  if (isTRUE(create_foreign_server)) {
    create_table_sql <- paste0(
      # "DROP EXTENSION IF EXISTS file_fdw CASCADE;", " \n",
      "CREATE EXTENSION IF NOT EXISTS file_fdw;", " \n",
      # "DROP SERVER IF EXISTS csv_src CASCADE;", " \n",
      "CREATE SERVER IF NOT EXISTS csv_src FOREIGN DATA WRAPPER file_fdw;\n",
      create_table_sql
    )
  }

  # The postgres server needs to access the file and requires privileges
  # so make sure about that. Warning, this is windows only
  system2(
    command = 'icacls',
    args = paste0(
      '"', normalizePath(file_path, winslash = '\\'), '"',
      ' /grant *S-1-5-32-545:R'
    ))

  if (tools::file_ext(file_path) %in% compressed_exts()) {
    # TODO
    # filename_or_program <- paste0(
    #   "PROGRAM ",
    #   "'",
    #   "\"", normalizePath(sevenzip_path), "\"",
    #   " e -so ",
    #   "\"", normalizePath(file_path), "\"", " || exit 0",
    #   "'"
    # )
  } else {
    filename_or_program <- paste0(
      "FILENAME '", normalizePath(file_path), "'"
    )
  }

  # https://www.postgresql.org/docs/10/sql-syntax-lexical.html
  # 4.1.2.2. String Constants With C-Style Escapes
  create_table_sql <- glue::glue(
    "{create_table_sql}
    SERVER csv_src
    OPTIONS (
      {filename_or_program},
      FORMAT 'csv',
      HEADER '{header}',
      DELIMITER E'{sep}',
      QUOTE E'{quote}',
      ESCAPE E'{escape}',
      NULL E'{null}',
      ENCODING '{encoding}'
    );"
  )

  cat("\n", create_table_sql, "\n")
  if (isTRUE(execute)) {
    cat("Executing SQL statement...\n")
    DBI::dbExecute(con, create_table_sql)
  }

  invisible(create_table_sql)
}

#' Check if `table_name` exists in the Postgres database of given `con`
#'
#' Uses Postgres' `to_regclass`, taking advantage of the fact that it will
#' "return null rather than throwing an error if the name is not found"
#' https://www.postgresql.org/docs/10/functions-info.html
#' This is probably the fastest way to test if a table (relation) exists.
#' Alternatives would be querying `pg_tables` or `pg_catalog`
#'
#' @param con DBI connection to the database
#' @param table_name character with the table name you want to check
#'
#' @return boolean
#' @export
#' @md
pg_exists_table <- function(con, table_name) {

  table_quoted <- DBI::dbQuoteString(con, table_name)@.Data
  query_res <- DBI::dbGetQuery(
    con, glue::glue(
    "SELECT to_regclass({table_quoted}) AS to_regclass;"
    )
  )

  all(!is.na(query_res$to_regclass))
}


#' Query table's size in the Postgres database of given `con`
#'
#' Uses Postgres' `pg_total_relation_size` to get the value, and
#' `pg_size_pretty` to try and make it human readable directly
#'
#' @param con DBI connection to the database
#' @param table_name character with the table name you want to check
#' @param pretty logical, if TRUE use `pg_size_pretty`
#'
#' @return character if pretty=TRUE, numeric otherwise
#' @export
#' @md
pg_total_relation_size <- function(con, table_name, pretty = TRUE) {

  table_quoted <- DBI::dbQuoteString(con, table_name)@.Data
  query_res <- DBI::dbGetQuery(
    con, glue::glue(
      "SELECT
          pg_total_relation_size({table_quoted}) AS size,
          pg_size_pretty(pg_total_relation_size({table_quoted})) AS pretty_size
        ;
        "
    )
  )

  if (isTRUE(pretty)) {
    size <- query_res$pretty_size
  } else {
    size <- query_res$size
  }
  size
}


#' List running queries in Postgres
#'
#' @param con pg connection
#'
#' @return a tibble
#' @export
pg_running_queries <- function(con) {
  tibble::tibble(DBI::dbGetQuery(con, "
    SELECT pid, age(clock_timestamp(), query_start), usename, query
    FROM pg_stat_activity
    WHERE query != '<IDLE>' AND query NOT ILIKE '%pg_stat_activity%'
    ORDER BY query_start desc;
  "))
}

#' Kill a running query in Postgres
#'
#' @param con pg connection
#' @param pid pid of the query to kill
#'
#' @return see pg_cancel_backend
#' @export
pg_kill_query <- function(con, pid) {
  tibble::tibble(DBI::dbExecute(con, glue::glue("
    SELECT pg_cancel_backend({pid});
  ")))
}

#' Copy data to a postgres instance
#'
#' Hernando, my use case for this is those cases where the data are small
#' enough to load it in R, but not so small so one can not put up with
#' a bunch of insert statements (the default when you use dplyr::copy_to())
#' Sometimes it is more convenient to initially load and wrangle a dataset in R
#' but when you later need it up in postgres, this is a quick-and-dirty way to
#' do it (if the data are not just a few rows but millions, it would take
#' forever to use a bunch of insert statements)
#'
#' @param data data to copy to postgres
#' @inheritParams pg_copy_file
#'
#' @return this is used for the side effect
#' @export
pg_copy_data <- function(con, data, table_name,
                         sep = "|",
                         drop_table = TRUE, create_table = TRUE,
                         if_not_exists = TRUE,
                         execute = TRUE) {

  tmp_file <- tempfile("pg_")
  data.table::fwrite(
    x = data,
    file = tmp_file,
    sep = sep
  )
  file_glimpse(tmp_file)

  pg_copy_file(
    con = con,
    file_path = tmp_file,
    table = table_name,
    sep = sep,
    nrows = 100,
    colClasses = purrr::imap_chr(data, ~class(.x)),
    drop_table = drop_table,
    create_table = create_table,
    if_not_exists = if_not_exists,
    execute = execute
  )
}


