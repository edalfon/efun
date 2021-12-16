#' Read from the clipboard to create a data.frame
#'
#' Just for convenience to quickly input data from excel, plain text, etc.
#' by copy-'paste'
#'
#' @param sep use tab as default separator
#' @param dec decimal separator to use
#' @param ... passed to read.table
#' @param header by default assume first line of data is column names
#'
#' @seealso read.table()
#' @keywords read.table, clipboard
#' @export
clipboard_readfrom <- function(sep="\t", header=TRUE, dec=".", ...) {
  # get the os like this is fragile/buggy, but meanwhile this does the trick
  # There are other arguably more robust approaches out there, but license ...
  # https://conjugateprior.org/2015/06/identifying-the-os-from-r/
  os <- Sys.info()["sysname"]
  if(os=="windows") {
    data.frame(utils::read.table(
      "clipboard", sep=sep, header=header, dec=dec,
      stringsAsFactors=FALSE, ...
    ))
  } else {
    data.frame(utils::read.table(
      pipe("pbpaste"), sep=sep, header=header, dec=dec,
      stringsAsFactors=FALSE, ...
    ))
  }
}

#' Copy an object to the clipboard
#'
#' It copies an object to the clipboard using tab as separator
#' (so as to paste in excel)
#'
#' @param eobject R object to copy to the clipboard
#'
#' @seealso write.table()
#' @keywords write.table, clipboard
#' @export
clipboard_writeto <- function(eobject) {
  os <- Sys.info()["sysname"]
  if(grepl(pattern = "windows", x = os, ignore.case = TRUE)) {
    utils::write.table(eobject, file="clipboard-65536", sep="\t", col.names=NA)
  } else {
    clip <- pipe("pbcopy", "w")
    utils::write.table(eobject, file=clip, sep="\t", col.names=NA)
    close(clip)
  }
}

