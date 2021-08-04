#' Get multi-dimensions stats
#'
#' @description `multi_dims_stats()` creates a new data frame. It will have one or more
#' rows for each combination of grouping dimensions. If there are no grouping dimensions,
#' the output will have a single row summarising all samples in the input data frame.
#' Despite the dimensions columns, it will contain other 3 or 6 columns depending on
#' whether there is *drug* dimension in the dimension combination.
#'
#' If *drug* dimension is in the dimension combination, it will report:
#'
#'   * `sample_size`: is the size of the sample set under specified dimensions.
#'   * `detection_rate`: is the detection rate of this specified sample set.
#'   * `qualification_rate`: is the qualification rate of this specified sample set.
#'
#' If *drug* dimension is **not** in the dimension combination, it will report:
#'
#'   * `sample_size`: is the size of the sample set under specified dimensions.
#'   * `qualification_rate`: is the qualification rate of this specified sample set.
#'   * `multi_detection_rate`: is the multi-residue detection rate of this specified
#'   sample set.
#'   * `max_defective_num`: is the max multi-residue detection number of this
#'   specified sample set.
#'   * `multi_defective_rate`: is the multi-residue defective rate of this specified sample set.
#'   * `max_defective_num`: is the multi-residue defective number of this
#'   specified sample set.
#'
#' @param data a data frame with dimensions labels and drug monitoring data divided by MRL.
#' @param ... dimension names parameters. **NOT Character**. The supported dimensions are
#' depending on `data`. As for now, only 6 dimensions are supported:
#'
#'   * year
#'   * quarter
#'   * province
#'   * category
#'   * product
#'   * drug (depending on param `drug_colname`)
#'
#' If you want to use (and you must use) this param, you should replace `...`
#' with e.g. `quarter, province, drug`.
#' @param drug_colname the name of column containing drug names, and is the dimension name.
#' @param st_drug_name the name of the first drug residual column.
#' @param ed_drug_name the name of the last drug residual column. *Notice*: the column
#' between `st_drug_name` and `ed_drug_name` should all be drug residual column.
#'
#' @return
#' a data frame containing essential stats.
#' @export
#'
#' @examples
#' ## without `drug`
#' # multi_dims_stats(treated_data, year, province)
#' # multi_dims_stats(treated_data)
#' ## with `drug`
#' # multi_dims_stats(treated_data, quarter, drug)
#' @importFrom magrittr %>%
#' @importFrom dplyr .data
multi_dims_stats <- function(
  data, ...,
  drug_colname = "drug",
  st_drug_name = "\u7532\u80fa\u78f7", ed_drug_name = "\u4e8c\u7532\u620a\u7075"
) {
  arguments <- dplyr::quos(...)
  args_names <- unlist(lapply(arguments, dplyr::quo_name))
  values_colname <- paste0(drug_colname, "_residual")
  if (drug_colname %in% args_names) {
    data <- data %>%
      dplyr::select(-c("multi_detection_num", "multi_defective_num", "is_qualified")) %>%
      tidyr::pivot_longer(
        cols = {{st_drug_name}}:{{ed_drug_name}},
        names_to = drug_colname,
        values_to = values_colname
      ) %>%
      dplyr::mutate(
        "is_detected" = .data[[values_colname]] > 0,
        "is_qualified" = .data[[values_colname]] <= 1,
      )
  }
  md_stats <- data %>%
    dplyr::group_by(...)
  if (drug_colname %in% args_names) {
    md_stats <- md_stats %>%
      dplyr::summarise(
        "sample_size" = dplyr::n(),
        "detection_rate" = sum(.data[["is_detected"]]) / dplyr::n(),
        "qualification_rate" = sum(.data[["is_qualified"]]) / dplyr::n()
      )
  } else {
    md_stats <- md_stats %>%
      dplyr::summarise(
        "sample_size" = dplyr::n(),
        "qualification_rate" = sum(.data[["is_qualified"]]) / dplyr::n(),
        "multi_detection_rate" = sum(.data[["multi_detection_num"]] >= 2) / dplyr::n(),
        "max_detection_num" = max(.data[["multi_detection_num"]]),
        "multi_defective_rate" = sum(.data[["multi_defective_num"]] >= 2) / dplyr::n(),
        "max_defective_num" = max(.data[["multi_defective_num"]])
      )
  }
  return(md_stats)
}

#' Get multi-dimensions' stats using data.table
#'
#' @description rewrite `multi_dims_stats()` using data.table to speed up
#' the calculation. The usage of this function is the same as `multi_dims_stats()` but
#' the efficiency is at least 6 times higher.
#'
#' @param data a **data table** with dimensions labels and drug monitoring data
#' divided by MRL.
#' @param ... dimension names parameters. **NOT Character**. The supported dimensions are
#' depending on `data`. As for now, only 6 dimensions are supported:
#'
#'   * year
#'   * quarter
#'   * province
#'   * category
#'   * product
#'   * drug (depending on param `drug_colname`)
#'
#' If you want to use (and you must use) this param, you should replace `...`
#' with e.g. `quarter, province, drug`.
#' @param drug_colname the name of column containing drug names, and is the dimension name.
#' @param st_drug_name the name of the first drug residual column.
#' @param ed_drug_name the name of the last drug residual column. *Notice*: the column
#' between `st_drug_name` and `ed_drug_name` should all be drug residual column.
#'
#' @return
#' a data frame containing essential stats.
#' @export
#'
#' @examples
#' ## without `drug`
#' # multi_dims_stats_dt(treated_data, year, province)
#' # multi_dims_stats_dt(treated_data)
#' ## with `drug`
#' # multi_dims_stats_dt(treated_data, quarter, drug)
#' @importFrom data.table .N .SD :=
multi_dims_stats_dt <- function(
  data, ...,
  drug_colname = "drug",
  st_drug_name = "\u7532\u80fa\u78f7", ed_drug_name = "\u4e8c\u7532\u620a\u7075"
) {
  arguments <- dplyr::quos(...)
  args_names <- unlist(lapply(arguments, dplyr::quo_name))
  values_colname <- paste0(drug_colname, "_residual")
  if (!data.table::is.data.table(data)) data <- data.table::as.data.table(data)
  if (drug_colname %in% args_names) {
    data <- data[,!c("multi_detection_num", "multi_defective_num", "is_qualified")]
    drug_col_ids <- which(colnames(data) == st_drug_name):which(colnames(data) == ed_drug_name)
    label_col_ids <- setdiff(1:ncol(data), drug_col_ids)
    data <- data.table::melt(
      data,
      id.vars = label_col_ids,
      measure.vars = drug_col_ids,
      variable.name = drug_colname,
      value.name = values_colname
    )
    data[,"is_detected" := lapply(.SD, function(x) {x > 0}), .SD = c(values_colname)]
    data[,"is_qualified" := lapply(.SD, function(x) {x <= 0}), .SD = c(values_colname)]
  }
  if (drug_colname %in% args_names) {
    md_stats <- data[
      ,
      .(
        sample_size = .N,
        detection_rate = sum(is_detected) / .N,
        qualification_rate = sum(is_qualified) / .N
      ),
      by = args_names
    ]
  } else {
    md_stats <- data[
      ,
      .(
        sample_size = .N,
        qualification_rate = sum(is_qualified) / .N,
        multi_detection_rate = sum(multi_detection_num >= 2) / .N,
        max_detection_num = max(multi_detection_num),
        multi_defective_rate = sum(multi_defective_num >= 2) / .N,
        max_defective_num = max(multi_defective_num)
      ),
      by = args_names
    ]
  }
  return(as.data.frame(md_stats))
}

#' Generate stats for all combinations of dims to be analysis
#'
#' @description `gen_dims_comb_stats()` returns a list contains $2^length(all_dims)$
#' data frame. It will calculate the specified statistical indicators for each combination
#' of dimensions to be analysis.
#'
#' @param data data.table if `fast` is `TRUE` where data.frame if `fast` is FALSE. It is
#' the data divided by MRL so you should evaluate function `calc_treated_data()` before
#' using it.
#' @param all_dims character vector of all dimensions.
#' @param fast logic, default is `TRUE`. Determine whether to use a more efficient function
#' rewrite with data.table.
#'
#' @return
#' a list contains $2^length(all_dims)$ data.frame. Each data.frame is named with
#' dimensions' names connected with underline and contains dimensions columns and
#' statistic columns.
#' @export
#'
#' @examples
#' # treated_data <- calc_treated_data(raw_data, mrl_data, "\u7532\u80fa\u78f7", "\u4e8c\u7532\u620a\u7075")
#' # dims <- c(names(RawColumnTrans), "product", "drug")
#' ## fast mode
#' # gen_dims_comb_stats(data, dims)
#' ## slow mode
#' # gen_dims_comb_stats(treated_data, dims, fast = FALSE)
#' @importFrom rlang !!!
gen_dims_comb_stats <- function(data, all_dims, fast = TRUE) {
  dims_comb_data <- list()
  total_num <- 2^length(all_dims)
  num <- 0
  for (i in 0:length(all_dims)) {
    dims_comb <- utils::combn(all_dims, i)
    for (j in 1:ncol(dims_comb)) {
      if ("category" %in% dims_comb[,j] && "product" %in% dims_comb[,j]) {}
      else {
        sp_dims <- rlang::parse_exprs(dims_comb[,j])
        var_name <- paste0(dims_comb[,j], collapse = '_')
        if (var_name == "") var_name <- "full_sample"

        data <- data.table::as.data.table(data)
        if (fast) {
          dims_comb_data[[rlang::parse_expr(var_name)]] <- multi_dims_stats_dt(data, !!!sp_dims)
        } else {
          dims_comb_data[[{var_name}]] <- multi_dims_stats(data, !!!sp_dims)
        }
      }
      num <- num + 1
      print(sprintf("Complete %s    %i/%i", var_name, num, total_num))
    }
  }
  return(dims_comb_data)
}


# # export as .csv file for each dimensions combination
# for (dims_comb_name in names(dims_comb_data)) {
#   write.csv(
#     dims_comb_data[[dims_comb_name]],
#     file = paste0("./Data/dims_combination_data/", dims_comb_name, ".csv"),
#     row.names = FALSE
#   )
# }


# # lapply vectorizing, slower compared with for loop
# calc_multi_dims_stats <- function(treated_data, dims_comb_vector) {
#   message(data.table::address(treated_data))
#   sp_dims <- rlang::parse_exprs(dims_comb_vector)
#   var_name <- paste0(dims_comb_vector, collapse = '_')
#   if (var_name == "") var_name <- "full_sample"
#   dims_comb_data <- multi_dims_stats_dt(treated_data, !!!sp_dims)
#   print(sprintf("Complete %s", var_name))
#   return(dims_comb_data)
# }
#
# dims_comb_data <- lapply(0:length(dims), function(x) {
#   dims_comb <- combn(dims, x);
#   apply(dims_comb, 2, calc_multi_dims_stats, treated_data = data)
# })
