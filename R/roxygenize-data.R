



toyme <- function() {

  sourcefile <- "data-raw/wow.R"
  roc_env <- roxygen2::env_file(sourcefile)
  rd_blocks <- roxygen2::parse_file(sourcefile, roc_env)
  help_topics <- roxygen2::roclet_process(roxygen2::rd_roclet(), rd_blocks, roc_env, dirname(sourcefile))

  roxygen2::roclet_output(roxygen2::rd_roclet(), help_topics, ".", options=list(wrap=FALSE), check = FALSE, is_first = TRUE)

  ""
}

