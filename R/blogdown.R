
#' Stop blogdown (hugo/jekyll) server
#'
#' Just a wrapper on `blogdown::stop_server()` to make it an addin
#'
#' @export
blogdown_stop_server <- function () {
  blogdown::stop_server()
}


