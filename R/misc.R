#' Print memory use (total and each object in the global environment)
#'
#' By default, using Mb as units. See ?memory.size()
#' Also, this is for windows only
#'
#' @seealso object.size, memory.size, memory.limit
#' @export
memory_use <- function() {

  NULL -> size

  in_use <- utils::memory.size()
  max_use <- utils::memory.limit()

  print(paste(
    'R is using', scales::comma(in_use), 'MB out of',
    scales::comma(max_use), 'MB', '-',
    scales::percent(in_use/max_use, accuracy = 0.1)
  ))

  purrr::map_df(ls(envir=.GlobalEnv), function (obj_name) {
    tibble::tibble(
      obj_name = obj_name,
      size = obj_name %>% get() %>% utils::object.size()
    )
  }) %>%
    arrange(desc(size)) %>%
    mutate(size = format(size, units = "Mb"))
}


#' Function that defines the operator %#% to concatenate strings
#'
#' This simple calls paste with sep = ""
#' @param x character object, to be converted to character vector
#'        (passing it to paste)
#' @param y character object, to be converted to character vector
#'        (passing it to paste)
#' @return a character vector
#' @keywords paste
#' @export
`%#%` <- function(x, y) {
  paste0(x, y)
}

#' Function that defines the operator %#% to concatenate strings
#'
#' This simple calls paste with sep = ""
#' @param x character object, to be converted to character vector
#'        (passing it to paste)
#' @param y character object, to be converted to character vector
#'        (passing it to paste)
#' @return a character vector
#' @keywords paste
#' @export
`%#_%` <- function(x, y) {
  paste(x, y, sep = "_")
}

#' Function that defines the operator %#% to concatenate strings
#'
#' This simple calls paste with sep = " "
#' @param x character object, to be converted to character vector
#'        (passing it to paste)
#' @param y character object, to be converted to character vector
#'        (passing it to paste)
#' @return a character vector
#' @keywords paste
#' @export
`%# %` <- function(x, y) {
  paste(x, y, sep = " ")
}


#' base::attr with a default value instead of NULL, and always exact
#'
#' @param default_value value to return when object x has no attribute which
#' @inheritParams base::attr
#'
#' @export
attr <-  function(x, which, default_value = "", exact = TRUE) {
  attr_value <- base::attr(x, which, exact)
  if (is.null(attr_value)) attr_value <- default_value
  attr_value
}


#' Append a timestamp to `x`, typically a path
#'
#' This function just appends a timestamp (format YYYYMMDDHHMMSS), separated by
#' an underscore (_) to whatever `x` is (so, `x` is coerced to character).
#' By default, it assumes `x` is a path and then adds the timestamp before
#' the last extension (if there is any).
#'
#' @param x a character or an object coercible to character
#' @param is_path logical to indicate if x is a path, then it removes extension
#' before appending the timestamp
#'
#' @return a character appending a timestamp to x
#' @export
#' @md
#'
#' @examples
#' # typical usage
#' timestamp_it("this/path/may/not/exist/")
#' timestamp_it("this/path/may/not/exist.txt")
#' # weird usage
#' timestamp_it(mtcars, FALSE)
timestamp_it <- function(x, is_path = TRUE) {
  tmstmp <- format(Sys.time(), "%Y%m%d%H%M%S")
  if (isTRUE(is_path)) {
    x_ext <- fs::path_ext(x)
    x_path <- fs::path_ext_remove(x)
    fs::path_ext_set(paste0(x_path, "_", tmstmp), x_ext)
  } else {
    paste0(x, "_", tmstmp)
  }
}
