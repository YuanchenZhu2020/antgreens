#' Print caption for DT::datatable in bookdown
#'
#' @description `dt_table_caption()` print the HTML tags of table caption for
#' table generated by package DT, which is a HTML widget that cannot be labeled using
#' table numbering system hence is reference-able. The chunk option `result='asis'`
#' should be **set** when using this function since the HTML tag of caption should be
#' considered as part of the document output.
#'
#' @param text_ref_str a character that can be **text reference** or **label**, e.g.,
#' "(ref:ref-name)" or "ref-name". Note that it **cannot** parse '-' in character.
#' @param caption a character ot NA, default NA. The caption of table is the name of
#' **text reference** when it set NA, or you can specify a character as the caption of
#' table instead of it.
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' # Type one
#' t1 <- "(ref:text-ref-name)"
#' dt_table_caption(t1)
#' # Type two
#' t2 <- "text-ref-name"
#' dt_table_caption(t2)
#' # possible mistakes and the behaviors
#' t3 <- "ref:text-ref-name"
#' dt_table_caption(t3)
#' t4 <- "text_ref_name"
#' dt_table_caption(t4)
dt_table_caption <- function(text_ref_str, caption = NA) {
  # replace pattern /^\\(ref:/g, e.g. "(ref:refabcd)" -> "refabcd)"
  # replace pattern /^[^-a-zA-Z0-9]|ref:$/g, e.g. "refabcd)" -> "refabcd", "ref:a-b" -> "refa-b"
  # mainly deal with 3 types of input:
  #   (ref:label-name), ref:label-name, label-name
  label <- gsub("[^-a-zA-Z0-9]", '', gsub("^\\(ref:", '', text_ref_str))
  if (!is.na(caption)) {
    text_ref_str <- caption
  }
  cat(
    "<table>",
    paste0("<caption>", "(#tab:", label, ")", text_ref_str, "</caption>"),
    "</table>",
    sep = "\n"
  )
}

#' Special Data Table
#'
#' @description This function is the wrapper of `DT::datatable()` and set default
#' values of some parameters frequently used. You can use `sp_dable()` to output
#' the caption reference-able and specify the columns to be sorted with DESC or ACS
#' method. Auto detecting column names depending on global options is the most usefully
#' function of this function. You can also set `detect_colnames = FALSE` and then
#' specify the `colnames` with a character vector of column names of the data.
#'
#' @param data a data frame to be displayed with `DT::datatable()`.
#' @param ref_text a character. It can be text reference character like `"(ref:<label>)"`
#' or the caption of table like `"Caption of table"`.
#' @param detect_colnames logic, default TRUE. If set `TRUE` then the function will
#' detect colnames depending on the global options `report.options$colnameMap` and
#' the column names of `data`.
#' @param colnames character vector of column names. When you want to use
#' auto-detection of column names, you can leave it missing since this function will not
#' consider the parameter.
#' @param order nested list of column index and sorting method. You can specify multiple
#' columns to be sorted. Every sorting option is in the form of
#' `list(<index of column>, <sorting method>)` (Note that the index of column is begin
#' at 0) and all sorting options should be put in a list. The sorting method can be
#' "desc" or "acs".
#' @param ... other params of `DT::datatable()` except `data` and `colnames`.
#'
#' @return None. But print HTML reference-able caption and HTML widget of DT::datatable.
#' @export
#' @importFrom rlang !!!
sp_dable <- function(
  data, ref_text, detect_colnames = TRUE, colnames, order, ...
) {
  arguments <- list(...)

  # use auto detect column names
  if (isTRUE(detect_colnames)) {
    colnames_map <- getOption("report.options")[["colnameMap"]]
    if (!is.null(colnames_map)) {
      colnames <- sapply(
        base::colnames(data),
        function(coln) {
          if (!is.null(colnames_map[[coln]])) colnames_map[[coln]] else coln
        }
      )
    }
    else {
      warning("You donnot set the `report.options$colnameMap` in `options()` before using colnames detection.")
      colnames <- base::colnames(data)
    }
  }
  else {
    if (missing(colnames)) colnames <- base::colnames(data)
  }

  # get order options
  order_option <- NULL
  if (!missing(order) && is.list(order)) {
    order_option <- lapply(order, function(x) {
      column_names <- colnames(data)
      if (x[[1]] %in% column_names) {
        return(list(which(x[[1]] == column_names) - 1, x[[2]]))
      }
      else {
        message(sprintf("'%s' is not the column of input data frame.", x[[1]]))
        return(list(NULL, NULL))
      }
    })
  }

  default_params <- list(
    extensions = "Buttons",
    colnames = unname(colnames),
    rownames = FALSE,
    filter = list(position = "top"),
    options = list(),
    # default argument value
    class = "display", caption = NULL, escape = TRUE, style = "auto",
    width = NULL, height = NULL, elementId = NULL, editable = FALSE,
    selection = c("multiple", "single", "none"), plugins = NULL,
    callback = dplyr::expr(DT::JS("return table;")),
    fillContainer = dplyr::expr(getOption("DT.fillContainer", NULL)),
    autoHideNavigation = dplyr::expr(getOption("DT.autoHideNavigation", NULL))
  )

  update_params <- utils::modifyList(
    default_params,
    arguments
  )
  # update order options
  if (is.null(update_params$options$order) && !is.null(order_option)) {
    update_params$options$order <- order_option
  }
  else if (!is.null(update_params$options$order) && !is.null(order_option)) {
    add_order_option <- Filter(
      function(x) {
        new_order <- update_params$options$order
        cols <- sapply(new_order, function(x) {x[[1]]})
        return(!(x[[1]] %in% cols))
      },
      order_option
    )
    update_params$options$order <- c(update_params$options$order, add_order_option)
  }
  else {}

  dt_table_caption(ref_text)
  # I can not use DT::datatable(data, !!!update_params) directly, and it has not
  # been figured out yet. But the expression can be run in the form of:
  # rlang::eval_tidy(dplyr::expr(DT::datatable(data, !!!update_params)))
  rlang::eval_tidy(
    rlang::call2("datatable", dplyr::expr(data), !!!update_params, .ns = "DT")
  )
}
