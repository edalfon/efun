#' Add chunk hook funtionality to print EXPLAIN results from DuckDB
#'
#' ChatGPT generated explanaition: this code defines a custom knitr hook
#' (duckdb_explain) that modifies the rendering of SQL code chunks.
#' When executed before a code chunk, it formats the SQL query
#' explanation using HTML tags for preformatted code display.
#' When executed after a code chunk, it reverts the rendering
#' behavior to the default. This is useful for producing nicely
#' formatted query explanations in knitr documents.
#'
#' @param all whether to set all chunks as duckdb_explain
#'
#' @return function called for its side effects
#' @export
#'
#' @examples
#' \dontrun{
#' # Simply put this line on the setup chunk of your .Rmd file
#' # and then set the chunk option `duckdb_explain` to TRUE when
#' # you want to override standard printing of sql chunks
#' efun::duckdb_explain_hook()
#' }
duckdb_explain_hook <- function (all = NULL)
{
  # Add a custom chunk hook, as documented here:
  # https://bookdown.org/yihui/rmarkdown-cookbook/chunk-hooks.html
  # Which basically allows you to execute code before and after
  # the chunk, when when the value of this chunk option is not NULL.
  # So let's call the chunk option `duckdb_explain`
  knitr::knit_hooks$set(duckdb_explain = function(before) {
    if (before) {
      #' Now, before running the chunk we want to sets a custom
      #' printing function for SQL code chunks. It overrides the
      #' default behavior for rendering SQL code, by setting the
      #' `sql.print` option of knitr. When you set this, knitr
      #' will call that function passing the data that results from
      #' the query and then print the result using cat. Note also
      #' that we wrap this in code display tags, because when you
      #' use the sql.print option, knitr will force the results='asis'.
      #' See line 653 of the sql knitr engine.
      #' https://github.com/yihui/knitr/blob/master/R/engine.R
      #' if (!is.null(sql.print)) {
      #'   options$results = 'asis'
      #'   cat(sql.print(data))
      #' }
      knitr::opts_knit$set(sql.print = function(x) {
        paste0("<pre><code>", x$explain_value, "</code></pre>")
      })
    }
    else {
      knitr::opts_knit$set(sql.print = NULL)
    }
  })
  knitr::opts_chunk$set(duckdb_explain = all)
}


