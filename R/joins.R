utils::globalVariables(c('.data', '.match', '.row_id'))

#' Left join lhs and rhs, using a phased approach
#'
#' We use three 'phases' to left-join `lhs` and `rhs`
#' - **Exact match**: well, exact match both on `exact_by` and `phased_by`
#' - **Quasi-exact match**: transform `phased_by` vars on `lhs` and `rhs` using
#'   `quasi_fun` and then matches on the transformed variables. Default for
#'   `quasi_fun` is `efun::normalize_text`, which removes spaces, dots, commas
#'   and non-ascii characters to avoid encoding issues
#' - **Fuzzy match using a two-way 'contains' approach**: this is powered by
#'   `fuzzyjoin::fuzzy_left_join` using as matching function
#'   `match_fun = ~ stringr::str_detect(.x, .y) | stringr::str_detect(.y, .x)`
#'
#' We apply those phases in that order, and every phase works only on the
#' unmatched rows from previous phases.
#'
#' @param lhs A data.frame-like
#' @param rhs A data.frame-like
#' @param phased_by A character vector of variables to join by, using a named
#' vector to join by different variables from `lhs` and `rhs` (à la `dplyr`.
#' See ?dplyr::join). The variables indicated here will be subject to the phased
#' approach.
#' @param exact_by An optional character vector of variables to join by, using
#' always an exact match.
#' @param drop_join_vars Whether to drop auxiliary variables for the match
#' (e.g. transformed variables). Defaults to TRUE, but for debugging may be
#' useful to set to FALSE.
#' @param quasi_fun The function to apply to `phased_by` variables, for the
#' quasi-exact match. Defaults to efun::normalize_text
#' @inheritParams dplyr::left_join
#'
#' @return a joined data-frame
#' @export
#' @md
phased_left_join <- function(lhs,
                             rhs,
                             phased_by,
                             exact_by = NULL,
                             drop_join_vars = TRUE,
                             quasi_fun = normalize_text,
                             suffix = c(".x", ".y")) {

  # TODO: check args
  vctrs::vec_assert(phased_by, ptype = character(), size = 1)
  # TODO: check .match and .join_var do not exists

  if (is.null(names(phased_by))) {
    names(phased_by) <- phased_by
  }
  lhs_phased_by <- names(phased_by)
  rhs_phased_by <- phased_by

  all_by <- c(".join_by" = ".join_by", exact_by)

  lhs <- lhs %>% mutate(.row_id = row_number())

  # Exact match ####
  exact_match <- lhs %>%
    mutate(.join_by = .data[[lhs_phased_by]]) %>%
    left_join(
      y = rhs %>% mutate(.join_by = .data[[rhs_phased_by]],
                          .match = "exact"),
      by = all_by,
      suffix = suffix,
      keep = TRUE # implies even join vars with common names will be suffixed
    )

  n_exact <- sum(!is.na(exact_match$.match))
  n_tomatch <- sum(is.na(exact_match$.match))
  usethis::ui_info(
    "{n_exact} rows exact match. {n_tomatch} rows to try quasi-exact"
  )

  # Quasi-exact match ####
  quasiexact_match <- lhs %>%
    # operates on non-exact-match
    filter(.row_id %in% exact_match[is.na(exact_match$.match), ]$.row_id) %>%
    mutate(.join_by = quasi_fun(.data[[lhs_phased_by]])) %>%
    left_join(
      y = rhs %>% mutate(.join_by = quasi_fun(.data[[rhs_phased_by]]),
                          .match = "quasi"),
      by = all_by,
      suffix = suffix,
      keep = TRUE
    )

  n_quasi <- sum(!is.na(quasiexact_match$.match))
  n_tomatch <- sum(is.na(quasiexact_match$.match))
  usethis::ui_info("{n_quasi} rows quasi-exact match. {n_tomatch} rows to try fuzzy-match")

  # Fuzzy match ####
  fuzzy_match <- lhs %>%
    filter(.row_id %in% exact_match[is.na(exact_match$.match), ]$.row_id) %>%
    filter(.row_id %in%
             quasiexact_match[is.na(quasiexact_match$.match), ]$.row_id) %>%
    mutate(.join_by = quasi_fun(.data[[lhs_phased_by]])) %>%
    fuzzyjoin::fuzzy_left_join(
      y = rhs %>% mutate(.join_by = quasi_fun(.data[[rhs_phased_by]]),
                          .match = "fuzzy"),
      by = all_by,
      # two-way stringr::str_detect; note purrr-style anonymous fn
      # TODO: make it a list to apply this function only on .join_var
      match_fun = ~ stringr::str_detect(.x, .y) | stringr::str_detect(.y, .x)
    ) %>%
    # need this because fuzzyjoin does not allow to customize the suffix
    # TODO: deal with preexisting variables ending in .x or .y
    rename_with(
      .cols = ends_with(".x"),
      .fn = ~stringr::str_replace(.x, ".x$", suffix[1])
    ) %>%
    rename_with(
      .cols = ends_with(".y"),
      .fn = ~stringr::str_replace(.x, ".y$", suffix[2])
    )

  n_fuzzy <- sum(!is.na(fuzzy_match$.match))
  n_tomatch <- sum(is.na(fuzzy_match$.match))
  usethis::ui_info(" {n_fuzzy} rows fuzzy match. {n_tomatch} rows still unmatched")

  # TODO: should we also include distance-based fuzzy match?

  # TODO: should we keep the original order?
  final_match <- dplyr::bind_rows(
    exact_match %>% filter(!is.na(.match)),
    quasiexact_match %>% filter(!is.na(.match)),
    fuzzy_match %>% filter(!is.na(.match)),
    # we want to keep all records, even non-matching ones. But row binding
    # directly from lhs could lead to inconsistent columns names
    fuzzy_match %>% filter(is.na(.match))
  )

  if (drop_join_vars) {
    final_match <- final_match %>% select(-.match, -starts_with(".join_by"))
  }

  final_match
}


#' Normalize text, removing spaces, common punctuation and non-ascii characters
#'
#' @param text A character vector
#'
#' @return A transformed character vector
#' @export
#'
#' @examples
#' normalize_text("§Bogotá, D.C.")
normalize_text <- function(text) {
  text %>%
    as.character() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(" ", "") %>%
    stringr::str_replace_all("[:punct:]", "")
}

