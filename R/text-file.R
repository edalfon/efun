#' Glimpse first n lines of a file
#'
#' It uses powershell (currently a Windows function only)
#'
#' @param file_path file path (character)
#' @param n number of lines (integer)
#' @param one_string logical, if TRUE (default) it collapses all lines in
#' a singe string
#'
#' @return a character vector (length n; if one_string = TRUE, length 1)
#' @export
file_head <- function(file_path, n = 10, one_string = TRUE) {

  args <- glue::glue('Get-Content "{normalizePath(file_path)}" -head {n}')
  # lines <- system2("powershell", args = args, stdout = TRUE, stderr = TRUE)
  # although add dependency, sys is faster..., relevantly faster for many lines
  res <- sys::exec_internal("powershell", args = args)
  lines <- sys::as_text(res$stdout)
  if (isTRUE(one_string)) {
    lines <- paste0(lines, collapse = "\n")
  }
  lines
}

#' Glimpse last n lines of a file
#'
#' It uses powershell (currently a Windows function only)
#'
#' @param file_path file path (character)
#' @param n number of lines (integer)
#' @param one_string logical, if TRUE (default) it collapses all lines in
#' a singe string
#'
#' @return a character vector (length n; if one_string = TRUE, length 1)
#' @export
file_tail <- function(file_path, n = 10, one_string = TRUE) {

  args <- glue::glue('Get-Content "{normalizePath(file_path)}" -tail {n}')
  res <- sys::exec_internal("powershell", args = args)
  lines <- sys::as_text(res$stdout)
  if (isTRUE(one_string)) {
    lines <- paste0(lines, collapse = "\n")
  }
  lines
}


#' Guess data types in a delimited text file (thin wrapper on data.table::fread)
#'
#' @inheritParams data.table::fread
#' @param ... further arguments passed to data.table::fread
#'
#' @return data.table
#' @export
#' @md
guess_types <- function(
  file,
  sep = "auto",
  sep2 = "auto",
  dec = ".",
  quote = "\"",
  nrows = 10000,
  header = "auto",
  na.strings = c("", "NA", "NULL"),
  skip = "__auto__",
  select = NULL,
  drop = NULL,
  colClasses = NULL,
  col.names,
  check.names = FALSE,
  encoding = "unknown",
  ...
) {

  read_glimpse <- data.table::fread(
    # 'cause fread can fail for very large files even though you try to read
    # just the first few lines see
    # https://stackoverflow.com/questions/52492986/fread-to-read-top-n-rows-
    # from-a-large-file
    cmd = glue::glue("powershell Get-content {file} -head {nrows}"),
    sep = sep,
    sep2 = sep2,
    dec = dec,
    quote = quote,
    nrows = nrows,
    header = header,
    na.strings = na.strings,
    colClasses = colClasses,
    encoding = encoding,
    ...
  )
  read_glimpse <- janitor::clean_names(read_glimpse)
  read_glimpse
}
