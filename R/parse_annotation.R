#' Parse an hierarchical annotation into a data.frame
#'
#' This function converts a markdown-style hierarchical annotation
#' into a data.frame with from -> to columns. For example a text like this:#'
#'   #Heading 1
#'   #Heading 2
#'   ##Heading 2.1
#'   ##Heading 2.2
#'   #Heading 3
#' would be converted to a data.frame with all the links
#' (e.g. Heading 2 -> Heading 2.1)
#'
#' @param note a length 1 character vector, with an annotation to be parsed
#' @param pattern a length 1 character vector, with the pattern to parse the
#' annotation. This is passed to `stringr::str_split` and
#' `stringr::str_extract_all`. The default value matches 1 or more hashtags (#)
#' and succesive hashtags indicate the level of the title/code
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' parse_single_annotation(
#'   "#1 ##1.1 ##1.2 ###1.2.1 ###1.2.2 ##1.3 #2 ##2.1 ###2.1.1"
#' )
parse_single_annotation <- function(note, pattern = "#+") {

  stopifnot(length(note) == 1)

  splits <- note |>
    stringr::str_split(pattern, simplify = TRUE) |>
    stringr::str_trim()
  levels_str <- stringr::str_extract_all(note, pattern, simplify = TRUE)
  levels_int <- stringr::str_length(levels_str)

  # We want to be a bit forgiving and allow ONE inconsistency in the format,
  # namely, allow the first separator to be omitted. For example:
  #   Title 1 ## Title 1.1
  # The note above should have been written
  #   # Title 1 ## Title 1.1
  # But we want to allow the first alternative as well, so we will remove the
  # first item only if it is empty (otherwise, there will be text that we
  # assume will be a level 1 code).
  if (splits[[1]] ==  "") {
    splits <- splits[-1]
  } else {
    levels_int <- append(1, levels_int)
  }

  stopifnot(length(splits) == length(levels_int))

  ancestors_level <- integer(1)
  ancestors <- character(0)
  edges <- tibble::tibble(from = character(0), to = character(0))
  for (i in seq_along(splits)) {

    # we
    if (levels_int[[i]] <= dplyr::last(ancestors_level)) {
      level_diff <- dplyr::last(ancestors_level) - levels_int[[i]] + 1
      ancestors <- utils::head(ancestors, -level_diff)
      ancestors_level <- utils::head(ancestors_level, -level_diff)
    }

    #cat(i, ": ", last(ancestors), "->", splits[[i]], "\n")
    edges <- tibble::add_row(edges, from = dplyr::last(ancestors),
                             to = splits[[i]])

    # always add the current element as the ancestor for the next gen
    ancestors <- append(ancestors, splits[[i]])
    ancestors_level <- append(ancestors_level, levels_int[[i]])
  }

  edges
}

#' Parse an hierarchical annotation into a data.frame
#'
#' @param notes a character vector
#' @inheritParams parse_single_annotation
#' @seealso parse_single_annotation
#'
#' @return a list of data.frame
#' @export
parse_annotations <- function(notes, pattern = "#+") {

  purrr::map(notes, ~parse_single_annotation(.x, pattern))
}
