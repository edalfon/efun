# summarize data.frame ####

utils::globalVariables(c("value", "perc"))

#' Summarize a data.frame by applying `summarize_obj` to each column
#'
#' Iterates through all the objects of a dataset, call `summarize_obj` on each,
#' and aggregates the results in a data.frame. This one is not meant to be
#' called directly, because it returns a data.frame full of information but
#' not really visual friendly. But we export it anyway, in case it is useful
#' at some point.
#'
#' @param df data.frame object to summarize
#' @return a data.frame summarizing the original data.frame with information
#' such as length (number of elements), number of na, number of unique elements,
#' sum, min, max, mean, median, percentiles, most frequent values
#' @export
#' @md
#' @examples
#' summarize_df(mtcars)
summarize_df <- function(df) {

  if(!("data.frame" %in% class(df))) {
    warning("This function only supports data.frame-like objects")
  }

  df_summary <- purrr::map_dfr(df, summarize_obj, .id = "varname")

  df_summary
}

#' Summarize an object, typically a vector (a column from a data.frame)
#'
#' Summarize an object (typically, a variable -column- in a
#' data.frame). This function is intended to be called by `summarize_df`
#'
#' @param obj R object to summarize
#' @param n_top Number of top unique elements to include in the summary
#'
#' @return a one-row tibble with information about the object such as class,
#' length (number of elements), number of na, number of unique elements, sum,
#' min, max, mean, median, percentiles, most frequent values
#' @export
#' @md
#' @examples
#' summarize_obj(letters)
#' summarize_obj(runif(100))
summarize_obj <- function(obj, n_top = 7) {

  NULL -> key -> count -> count_p -> top_str -> rowname -> .

  if("list" %in% class(obj) | "data.frame" %in% class(obj)){
    warning("summarize_obj does not support '", class(obj), "' objects")
  }

  obj_type <- typeof(obj) %>% paste(collapse = ", ")
  obj_class <- class(obj) %>% paste(collapse = ", ")
  obj_label <- attr(obj, "label")
  obj_label <- ifelse(is.null(obj_label), "", toString(obj_label))

  freq_table <- vctrs::vec_count(obj, sort = "count")

  n_obj <- length(obj)

  n_na <- freq_table %>% filter(is_invalid_number(key)) %>% pull(count)
  n_na <- ifelse(length(n_na) == 0, 0, sum(n_na))
  p_na <- n_na / n_obj

  n_uniq <- nrow(freq_table) - (1 * (n_na > 0)) # count uniq, excluding NA
  p_uniq <- n_uniq / n_obj

  top_values <- freq_table %>%
    mutate(key = as.character(key)) %>% # coerce to character to enable row_bind
    slice(1:n_top) %>%
    mutate(count_p = count / n_obj) %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_wider(
      names_from = rowname,
      values_from = c(key, count, count_p),
      names_glue = "top{rowname}_{.value}",
      names_sort = TRUE
    )

  num_stats <- get_stats(obj)

  obj_hist <- numeric(0)
  if (inherits(obj, c("numeric", "integer", "Date")) &
      !all(is_invalid_number(obj)) ) {
    obj_hist <- graphics::hist.default(as.numeric(obj), plot = FALSE)$counts
  } else {
    # this is very much adhoc, it is ugly and looks kinda hacky, but, it is way
    # faster than more principled alternatives. So let' stick with this.
    # After all, this is to be shown in a little inline-bar-plot, just to give
    # a sense of how concentrated would be this discrete/categorical variable
    # so it does not aim to be super accurate
    max_bars <- 100
    obj_bars <- nrow(freq_table)
    if (obj_bars <= max_bars) {
      obj_hist <- freq_table$count
    } else {
      obj_hist <- c(
        freq_table$count[1],
        sort(sample(freq_table$count[2:(obj_bars - 1)], max_bars - 2), TRUE),
        freq_table$count[obj_bars]
      )
    }
  }

  obj_stats <- tibble::tibble(
    obj_type = obj_type,
    obj_class = obj_class,
    obj_label = obj_label,
    n_obj = n_obj,
    n_na = n_na,
    p_na = p_na,
    n_uniq = n_uniq,
    p_uniq = p_uniq,
    !!!top_values,
    !!!num_stats,
    obj_hist = list(obj_hist)
  )

  obj_stats
}

#' Test if x is NA, NaN, Inf, -Inf, NULL
#'
#' Can be helpful as a workaround for gotchas such as:
#' is.numeric(NaN)
#' is.numeric(Inf)
#' is.numeric(NA)
#' is.numeric(NULL)
#'
#' @param x a vector
#'
#' @return boolean
#' @export
#'
#' @examples
#' is_invalid_number(c(NA, Inf, mean(Inf, -Inf)))
is_invalid_number <- function(x) {
  is.na(x) | is.nan(x) | is.infinite(x) | is.null(x)
}

#' Calculate several stats for obj
#'
#' Using S3 methods to dispatch get_stats according to the obj class
#'
#' @param obj
#'
#' @return a named vector of stats
#' @keywords internal
get_stats <- function(obj) {
  UseMethod("get_stats", obj)
}

get_stats.default <- function(obj) {

  num_stats <- c(mean = NA, sd = NA, min = NA, q25 = NA, median = NA,
                 q75 = NA, max = NA)

  num_stats
}

get_stats.numeric <- function(obj) {

  num_stats <- c(
    mean(obj, na.rm = TRUE),
    stats::sd(obj, na.rm = TRUE),
    stats::quantile(obj, probs = seq(0, 1, 0.25), na.rm = TRUE, names = FALSE)
  ) %>%
    magrittr::set_names(names(get_stats.default())) %>%
    scales::comma(accuracy = NULL)
    # controversial?, make it all string to enable different data types formatting

  num_stats
}

get_stats.Date <- function(obj) {

  num_stats <- c(
    mean(obj, na.rm = TRUE) %>%
      format.Date(format = "%Y-%m-%d"),
    stats::sd(obj, na.rm = TRUE) %>%
      scales::comma(accuracy = NULL, suffix = " days"),
    stats::quantile(obj, probs = seq(0, 1, 0.25), na.rm = TRUE, names = FALSE, type = 1) %>%
      format.Date(format = "%Y-%m-%d")
  ) %>% magrittr::set_names(names(get_stats.default()))

  num_stats
}

get_stats.POSIXct <- function(obj) {

  num_stats <- c(
    mean(obj, na.rm = TRUE) %>%
      format.POSIXct(),
    stats::sd(obj, na.rm = TRUE) %>%
      scales::comma(accuracy = NULL, suffix = " seconds"),
    stats::quantile(obj, seq(0, 1, 0.25), na.rm = TRUE, names = FALSE, type = 1) %>%
      format.POSIXct()
  ) %>% magrittr::set_names(names(get_stats.default()))

  num_stats
}

# e.g. haven_labelled has class double, but not numeric.
# in that case, is.numeric(obj) is TRUE, but inherits(obj, "numeric") is not
# Then dispatch default and not
# get_stats.double <- function(obj) get_stats.numeric(obj)
# get_stats.integer <- function(obj) get_stats.numeric(obj)

# visualize data.frame summary ####

#' @keywords internal
make_tooltip <- function(text, tooltip) {
  paste0("<span title='", tooltip, "'>", text,"</span>")
}

#' Where we make a mess
#'
#' the part where abridge_df is most strongly opinionated is in the mix of
#' stats for numeric and non-numeric variables in the same column.
#' Basically, a bunch of columns where we show stats (e.g. mean) for numeric
#' variables, and top X value and counts for non-numeric variables.
#' So in this function we pass the relevant variables from the summary df
#' and it makes this conditional
#'
#' @param stat_var stat var from the summary df, such as mean, sd, q25, etc.
#' @param top_key value of the top X
#' @param top_count count of the top X
#' @param top_count_p percentage of the top X
#' @param top_k X, a number indicating the top
#' @keywords internal
make_mix <- function(stat_var, top_key, top_count, top_count_p, top_k) {

  text <- case_when(
    !is.na(stat_var) ~ paste0("<b>", stat_var, "</b>"),
    is.na(stat_var) & !is.na(top_count) ~ paste0(
      "<b>", stringr::str_trunc(top_key, 14), "</b><br>",
      "<small><i>",
      scales::comma(top_count, accuracy = 1), " [",
      scales::percent(top_count_p, accuracy = 0.1), "]",
      "</i></small>"
    ),
    TRUE ~ ""
  )

  tooltip <- case_when(
    !is.na(top_count) ~ paste0(
      "Top ", top_k,":\n\n",
      top_key, "\n",
      scales::comma(top_count, accuracy = 1), " [",
      scales::percent(top_count_p, accuracy = 0.1), "]"
    ),
    TRUE ~ ""
  )

  make_tooltip(text, tooltip)
}


#' Summarize a data.frame using an opinionated -and perhaps controversial-
#' approach
#'
#' A one-function-call that produces a compact yet full of information
#' summary of a data.frame, presented in a DataTables (via `DT`)
#'
#' @param df data.frame-like object to summarize
#' @param file path to file to save the htmlwidget
#' @param ... passed to htmlwidgets::saveWidget
#'
#' @return a DT datatables htmlwidget object
#' @export
#' @md
#'
#' @examples
#' abridge_df(mtcars)
#' abridge_df(ggplot2::diamonds)
#' nlsw88 <- haven::read_dta('http://www.stata-press.com/data/r15/nlsw88.dta')
#' abridge_df(nlsw88)
#' abridge_df(forcats::gss_cat)
abridge_df <- function(df, file = NULL, ...) {

  data_name <- deparse(substitute(df))

  summary_df <- summarize_df(df)
  attr(summary_df, "data_name") <- data_name
  summary_dt <- visualize_summary(summary_df)

  if (!missing(file)) {
    DT::saveWidget(summary_dt, file, ...)
  }

  summary_dt
}

#' Put the summary in a DT for compact, useful and hopefully pretty
#' visualization
#'
#' This function takes as argument a summary data.frame as returned by
#' `summarize_df`, which contains all the information extracted from the
#' original data.frame to be presented in the DT summary.
#'
#' We take advantage of DT to:
#' - style columns with color bars to easilly pinpoint variables with many/few
#'   NAs and high/low cardinality variables
#' - Include tooltips in several places to put more information available,
#'   without using space on the table (not using the datatables ellipsis
#'   plugin, though. Some cases would be kind of hacky, so it ends up being
#'   cleaner just changing the content of the data directly to include tooltips)
#' - of course, enable sorting by the different stats
#' - and we use `sparkline` to include inline histograms for each variable#'
#'
#' @param summary_df
#'
#' @return a DT datatables htmlwidget object
#' @md
#' @keywords internal
visualize_summary <- function(summary_df) {

  NULL -> n_na -> n_uniq -> obj_class -> obj_hist -> obj_label ->
  obj_type -> p_na -> p_uniq -> q25 -> q75 -> median -> sd -> st1 -> st2 ->
  st3 -> st4 -> st5 -> st6 -> st7 -> top1_count -> top1_count_p ->
  top1_key -> top2_count -> top2_count_p -> top2_key -> top3_count ->
  top3_count_p -> top3_key -> top4_count -> top4_count_p -> top4_key ->
  top5_count -> top5_count_p -> top5_key -> top6_count -> top6_count_p ->
  top6_key -> top7_count -> top7_count_p -> top7_key -> varname -> Variable ->
  Type -> Hist

  summary_df %>%
    mutate(Variable = make_tooltip(
      text = paste0("<b>", varname, "</b>"),
      tooltip = obj_label
    )) %>%
    mutate(Type = make_tooltip(
      stringr::str_trunc(obj_class, 14),
      paste0("class:\n", obj_class, "\n\ntypeof:\n", obj_type, "")
    )) %>%
    mutate(st1 = make_mix(mean,   top1_key, top1_count, top1_count_p, 1)) %>%
    mutate(st2 = make_mix(sd,     top2_key, top2_count, top2_count_p, 2)) %>%
    mutate(st3 = make_mix(min,    top3_key, top3_count, top3_count_p, 3)) %>%
    mutate(st4 = make_mix(q25,    top4_key, top4_count, top4_count_p, 4)) %>%
    mutate(st5 = make_mix(median, top5_key, top5_count, top5_count_p, 5)) %>%
    mutate(st6 = make_mix(q75,    top6_key, top6_count, top6_count_p, 6)) %>%
    mutate(st7 = make_mix(max,    top7_key, top7_count, top7_count_p, 7)) %>%
    rowwise() %>%
    mutate(Hist = sparkline::spk_chr(
      obj_hist, type = "bar", zeroColor = "#ffffff", chartRangeMin = "0",
      barColor = ifelse(!is.na(mean), "#3366cc", "#00ff7f")
    )) %>%
    select(
      Var = Variable,
      Type,
      `NA<br>(#)` = n_na,
      `NA<br>(%)` = p_na,
      `Uniq<br>(#)` = n_uniq,
      `Uniq<br>(%)` = p_uniq,
      `mean<br>Top1` = st1,
      `sd<br>Top2` = st2,
      `min<br>Top3` = st3,
      `q25<br>Top4` = st4,
      `median<br>Top5` = st5,
      `q75<br>Top6` = st6,
      `max<br>Top7` = st7,
      Hist
    ) %>%
    rename_with(.fn = make_tooltip, .cols = 1, tooltip = paste0(
      "Variable name. If the variable has an associated label, it would ",
      "appear as tooltip"
    )) %>%
    rename_with(.fn = make_tooltip, .cols = 2, tooltip = paste0(
      "Data type of the variable as returned by class(). In the tooltip also ",
      "appears the typeof()"
    )) %>%
    DT::datatable(
      escape = FALSE,
      class = "display cell-border responsive nowrap compact",
      extensions = c("FixedHeader"),
      caption = htmltools::tags$caption(htmltools::tags$mark(
        style = "caption-side: top; text-align: center; background-color: #cceeff;",
        "'",
        htmltools::strong(htmltools::em(attr(summary_df, "data_name"))),
        "' has ",
        htmltools::strong(scales::comma(mean(summary_df$n_obj), accuracy = 1)),
        " rows x ",
        htmltools::strong(scales::comma(nrow(summary_df), accuracy = 1)),
        " cols"
      )),
      options = list(
        pageLength = -1,
        info = FALSE,
        lengthMenu = list(c(-1, 10, 25, 50), c("All", "10", "25", "50")) ,
        fixedHeader = TRUE,
        columnDefs = list(list(orderable = TRUE, targets = 0:14)),
        initComplete = DT::JS(paste0(
          "function(settings, json) {$(this.api().table().header()).css(",
          "{'font-size': '11px', 'text-align': 'center'});}"
        ))
      )
    ) %>%
    DT::formatRound(c(3, 5), digits = 0) %>%
    DT::formatPercentage(c(4, 6), digits = 1) %>%
    DT::formatStyle(columns = 0:13, `font-size` = '14px') %>%
    DT::formatStyle(columns = c(0, 7:13), `text-align` = 'center') %>%
    DT::formatStyle(
      columns = 3, # NA count
      background = DT::styleColorBar(summary_df$n_na, "red")
    ) %>%
    DT::formatStyle(
      columns = 4, # NA percentage
      background = DT::styleColorBar(0:1, "red")
    ) %>%
    DT::formatStyle(
      columns = 5, # Unique count
      background = DT::styleColorBar(summary_df$n_uniq, "deepskyblue")
    ) %>%
    DT::formatStyle(
      columns = 6, # Unique percentage
      background = DT::styleColorBar(0:1, "deepskyblue")
    ) %>%
    DT::formatStyle(
      columns = c(0, 14), # Histogram sparklines
      padding = "1px"
    ) %>%
    DT::formatStyle(
      columns = 0:13,
      `padding-top` = "1px",
      `padding-bottom` = "1px",
      `padding-right` = "2px",
      `padding-left` = "2px",
    ) %>%
    sparkline::spk_add_deps()
}



# tabulate ####

#' One-way tabulate table
#'
#' Returned as tibble, with percentages
#'
#' @param vctr a vector to tabulate
#' @inheritParams dplyr::count
#'
#' @return tibble with columns value, n, perc
#' @export
#' @examples
#' vctr <- sample(LETTERS[1:5], 1000, TRUE)
#' tab(vctr)
#' # Includes NAs count
#' vctr <- sample(c(LETTERS[1:5], NA), 1000, TRUE)
#' tab(vctr)
#' # For factors, by default keeps levels not present in data
#' vctr <- factor(
#'   x = sample(c(LETTERS[1:5], NA), 1000, TRUE),
#'   levels = c(LETTERS[1:5], "Levels", "not present", "in data")
#' )
#' tab(vctr)
tab <- function(vctr, .drop = FALSE){
  tibble::tibble(value = vctr) %>%
    count(value, sort = TRUE, .drop = .drop) %>%
    mutate(perc = n/sum(n, na.rm = TRUE)) %>%
    mutate(
      n = formattable::comma(n, digits = 0),
      perc = formattable::percent(perc, digits = 1)
    )
}

# Old implementation using base::table, new one above is based on dplyr::count
# An implementation based on vctrs::vec_count would be slightly faster, but
# "it only counts combinations that appear in the input". And that's something
# we want to keep and dplyr::count on top of vctrs::vec_count does that
tab2 <- function(vct) {
  vct <- as.vector(vct)
  vct_table <- sort(table(vct, useNA = "ifany"))
  freq <- as.numeric(as.character(unname(vct_table)))
  data.frame(
    value = names(vct_table),
    freq = formattable::comma(freq, digits = 0),
    perc = formattable::percent(freq/sum(freq), digits = 1)
  )
}

#' Check if a variable is unique, i.e., there are as many unique values in the
#' object as the total number of values
#'
#' @param obj a vector
#'
#' @seealso unique length
#' @export
#' @examples
#' is_unique(1:7)
is_unique <- function (obj) {
  n_all <- length(obj)
  n_unique <- length(unique(obj))
  cat(n_unique, "unique values\n")
  cat(n_all, "all values\n")
  n_all == n_unique
}


#' Check if the `...` columns of the `df` have a one-to-one relation
#'
#' Aim to test cases like:
#' - A table with `person_id` and `birth_date`: each `person_id` must always
#'   have the same `birth_date`. Calling `are_paired(df, person_id, birth_date)`
#'   lets you test that. `birth_date`, of course, can be duplicated. Even
#'   `person_id` could also be duplicated.
#' - data with `region_name` and `region_code`
#'
#' TODO: let's find a better name for this
#' TODO: check better algorithms
#'
#' @param df a data.frame
#' @param ... unquoted columns of the `df` to test. If empty, all columns used
#'
#' @return logical
#' @export
#' @md
#'
#' @examples
#' are_paired(mtcars)
#' are_paired(mtcars, mpg, cyl)
are_paired <- function (df, ...) {

  if (rlang::dots_n(...) > 0) {
    df <- dplyr::select(df, ...)
  }
  if (rlang::dots_n(...) == 1) warning("Does not really make sense for 1 var!")

  # there must be better algorithms for this (check for example how to
  # determine composite indexes in databases, which has been extensively
  # researched).
  # Meanwhile, the strategy here is just take the column with highest
  # cardinality and test that it is equal to the cardinality of the
  # whole set of columns
  nuniq_all <- NROW(unique(df)) # deduplicate, so not assume an id exists
  nuniq_each <- purrr::map_int(df, vctrs::vec_unique_count)

  cat(nuniq_all, "combinations for all columns\n")
  cat("number of unique values for each column as follows:\n")
  print(nuniq_each)
  any(nuniq_each == nuniq_all)
}



#' "Delete by ellipsis" column names, putting the original
#' column names as a tooltip
#'
#' @param data data whose columns will be renamed
#' @param ellipsis indicator to append to the column names to inducate trunc
#' @param min_width minimum with to guarantee for the column names
#'
#' @return data with new column names
#' @export
ellipt_colnames <- function(data, ellipsis = "", min_width = 3) {

  min_width <- max(stringr::str_length(ellipsis), min_width)

  col_widths <- data |>
    dplyr::ungroup() |>
    dplyr::summarise(dplyr::across(
      .fns = ~max(min_width, stringr::str_length(.x))
    ))

  col_names <- names(data)

  col_truncnames <- purrr::map2_chr(
    .x = col_names,
    .y = col_widths,
    .f = ~stringr::str_trunc(
      string = .x,
      width = .y,
      ellipsis = ellipsis
    )
  )

  col_tooltipnames <- purrr::map2_chr(
    .x = col_names,
    .y = col_truncnames,
    .f = ~efun:::make_tooltip(
      text = .y,
      tooltip = .x
    )
  )

  names(data) <- col_tooltipnames
  data
}

