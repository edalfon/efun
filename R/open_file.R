
#' Open a file or URL in the system's default application
#'
#' @param file_path a vector
#'
#' @return see ?system2
#' @export
open_file <- function(file_path) {

  full_path <- shQuote(normalizePath(file_path))

  switch(Sys.info()["sysname"], 
    "Windows" = system2("open", full_path, wait = FALSE),
    "Darwin" = system2("open", full_path, wait = FALSE),
    "Linux"   = system2("xdg-open", full_path, wait = FALSE),
    "SunOS"   = system2("xdg-open", full_path, wait = FALSE),
    "FreeBSD" = system2("handlr", paste("open", full_path), wait = FALSE)
  )
}
