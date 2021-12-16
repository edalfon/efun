#' Time `knitr` chunks, by default, all of them
#'
#' This function takes knitr's official example on using chunk hooks to
#' report how much time each chunk takes to run:
#' [https://bookdown.org/yihui/rmarkdown-cookbook/time-chunk.html](https://bookdown.org/yihui/rmarkdown-cookbook/time-chunk.html)
#'
#' And just wrap it in a function so you can simply write `efun::time_chunk()`
#' in your top chunk, instead of the whole example.
#'
#' @param all whether to time all chunks by default (set the default value for
#' chunk option `time_it`)
#'
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' # Simply put this line on the top chunk of your .Rmd file
#' efun::time_chunk(all = TRUE)
#' }
time_chunk <- function(all = TRUE) {
  all_times <- list()  # store the time for each chunk
  knitr::knit_hooks$set(time_it = local({
    now <- NULL
    function(before, options) {
      if (before) {
        # record the current time before each chunk
        now <<- Sys.time()
      } else {
        # calculate the time difference after a chunk
        res <- round(difftime(Sys.time(), now), 2)
        all_times[[options$label]] <<- res
        # return a character string to show the time
        paste("Time:", format(res))
      }
    }
  }))
  knitr::opts_chunk$set(time_it = all)
}
