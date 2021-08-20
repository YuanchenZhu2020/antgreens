#' Arrange Rows by Multiple Columns
#'
#' @description Arrange rows in a data frame according by the columns provided. The order of
#' columns will determine the order of arrangement.
#'
#' @param data a data frame
#' @param ... quoted column names
#'
#' @return a data frame with arranged rows
#' @export
#'
#' @examples
#' df <- data.frame(a = c(4, 6, 2), b = c('a', 's', 'd'))
#' df <- arrange_rows(df, a, b)
#' @importFrom magrittr %>%
arrange_rows <- function(data, ...) {
  data <- data %>%
    dplyr::arrange(...)
  return(data)
}

#' Get Lower/Upper Bounds for Odds to Display
#'
#' @param data a data frame.
#' @param column_name character. Name of target column which value is between [0, 1] or [0, 100].
#' @param lower logical, default TRUE. If set TRUE then will return the lower bound, otherwise
#' will return the upper bound.
#' @param increment numeric, default is 5. It is the increment of bound.
#'
#' @return numeric
#' @export
#'
#' @examples
#' df <- data.frame(a = c(11, 23, 1), b = c(0.2, 0.3, 0.1))
#' get_odds_bounds(df, 'a', lower = FALSE)
#' get_odds_bounds(df, 'b', increment = 0.2, lower = FALSE)
get_odds_bounds <- function(data, column_name, lower = TRUE, increment = 5) {
  bounds <- seq(0, ifelse(increment <= 1, 1, 100), increment)
  if (lower) bound <- bounds[max(1, which(min(data[[column_name]]) <= bounds)[1] - 1)]
  else bound <- bounds[which(max(data[[column_name]]) <= bounds)[1]]
  return(bound)
}

#' Get Lower/Upper Bounds of Numeric/Integer Sequence
#'
#' @param data a data frame with numeric or integer sequence.
#' @param column_name character of column name.
#' @param increment numeric, default 5. It is the increment of bounds.
#' @param upper_shrink locical, default FALSE. If set TRUE and the \code{shrink_prop}
#' of upper bound is larger than the maximum of target column, the actual upper column
#' will use its maximum value.
#' @param shrink_prop numeric, default is 0.66666.... This is the proportion of shrink threshold.
#'
#' @return numeric vector of length 2. The first is lower bound and the second is upper bound.
#' @export
#'
#' @examples
#' df <- data.frame(a = c(11, 23, 1), b = c(0.2, 0.3, 0.1))
#' get_bounds(df, 'a')
#' get_bounds(df, 'b', increment = 0.2)
#' get_bounds(df, 'b', increment = 0.4, upper_shrink = TRUE)
get_bounds <- function(data, column_name, increment = 5, upper_shrink = FALSE, shrink_prop = 2/3) {
  if (increment <= 0) stop("The increment of bounds should large than 0.")
  data_seq <- data[[column_name]]
  if (!inherits(data_seq, c("numeric", "integer"))) {
    stop(sprintf("Column %s should be numeric or integer", column_name))
  }
  lower_bound <- increment * floor(min(data_seq) / increment)
  upper_bound <- increment * ceiling(max(data_seq) / increment)
  if (upper_shrink) {
    if (max(data_seq) < shrink_prop * upper_bound) upper_bound <- max(data_seq)
  }
  return(c(lower_bound, upper_bound))
}

#' Combine the Year and Quarter Column into '2021-Q1' Form
#'
#' @description This function will combine the numeric or factor column of year and quarter into
#' the form of "2021-Q1" whcih reffers to the year 2021 and the first quarter.
#'
#' @param data a data frame.
#' @param year_name character, default 'year'. It is the column name representing the year.
#' @param quarter_name character, default 'quarter'. It is the column name representing the quarter.
#' @param simple logical, default \code{TRUE}. If set `TRUE` then use number as quarter name.
#' @param quarter_trans list containing 4 characters and the names of each element is 1, 2, 3, 4,
#' which represents the four quarters.
#'
#' @return a data frame with column \code{timeline} and without column \code{year_name} and
#' \code{quarter_name}.
#' @export
#'
#' @examples
#' df <- data.frame(a = 2017:2021, b = c(1,2,2,4,3))
#' combine_year_quarter(df, year_name = 'a', quarter_name = 'b')
#' @importFrom magrittr %>%
combine_year_quarter <- function(
  data, year_name = "year", quarter_name = "quarter",
  simple = TRUE, quarter_trans = quarter_readable
){
  data <- data %>%
    dplyr::rename("quarter" = .data[[quarter_name]])
  if (simple) {
    data <- data %>%
      dplyr::mutate(timeline = paste0(.data[[year_name]], "-Q", "quarter"))
  }
  else {
    data <- data %>%
      factor_quarter(readable = TRUE, quarter_trans = quarter_trans) %>%
      dplyr::mutate(timeline = paste0(.data[[year_name]], "\u5e74", .data[["quarter"]]))
  }
  data <- data %>%
    dplyr::select(-c(dplyr::all_of(year_name), "quarter"))
  return(data)
}

#' Change Qulification Rate (Percent) to Fractration Defective (Percent)
#'
#' @description \code{change_to_defective(df)} will change the \code{qualification_rate} column in
#' \code{df} to \code{defective_rate} using \eqn{1 - qualification_rate}, or change the
#' \code{qualification_rate_percent} column in \code{df} to \code{defective_rate_percent} column
#' using \eqn{100 - qualification_rate_percent}.
#'
#' @param data data frame with \code{qualification_rate(_percent)} column
#' @param use_percent logical, default TRUE. Whether to use "percent" suffix.
#' @param digits numeric, default 2. The number of dicimal reserved when using minus to change
#' to defective.
#'
#' @return a data frame with \code{fractration_rate(_percent)} column.
#' @export
#'
#' @examples
#' df <- data.frame(qualification_rate = runif(3, 0, 1), qualification_rate_percent = runif(3, 0, 100))
#' change_to_defective(df, use_percent = FALSE)
#' change_to_defective(df)
#' @importFrom magrittr %>%
change_to_defective <- function(data, use_percent = TRUE, digits = 2) {
  defective_name <- rlang::sym(
    ifelse(use_percent, "rate_percent", "rate")
  )
  qualification_name <- ifelse(use_percent, "qualification_rate_percent", "qualification_rate")
  full <- ifelse(use_percent, 100, 1)
  data <- data %>%
    dplyr::mutate("defective_{{defective_name}}" := round(full - .data[[qualification_name]], digits)) %>%
    dplyr::select(-.data[[qualification_name]])
  return(data)
}

#' Change Integer Column to Numeric in Data Frame
#'
#' @param data data frame with integer column
#' @param column_name character. The column name of integer column.
#'
#' @return data frame with numerci column
#' @export
#'
#' @examples
#' df <- data.frame(a = as.integer(1:3))
#' int_to_numeric(df, 'a')
int_to_numeric <- function(data, column_name) {
  if (!inherits(data[[column_name]], "integer")) {
    stop(column_name, " must be integer")
  }
  data[[column_name]] <- as.numeric(data[[column_name]])
  return(data)
}

#' Shorten Province Names
#'
#' @param data data frame with column named "province"
#'
#' @return a data frame which province column contains short names of province
#' @export
shorten_province_name <- function(data) {
  provinces_trans <- getOption("report.options")[["provinceTrans"]]
  data[["province"]] <- sapply(as.character(data[["province"]]), \(x) {provinces_trans[[x]]})
  return(data)
}

#' Get Corresponding Threshold Value
#'
#' @param threshold_df data frame contains dimension labels encoded with 0/1 and
#' threshold values.
#' @param dims character vector of all dimension variable names.
#' @param arg_dims character vector of selected dimension variable names.
#'
#' @return nunmeric value
#' @export
get_threshold <- function(threshold_df, dims, arg_dims) {
  threshold_line <- threshold_df %>%
    filter(!!!rlang::parse_exprs(c(
      paste0(setdiff(dims, arg_dims), " == 0"),
      paste0(arg_dims, " == 1")
    )))
  return(threshold_line[["threshold"]])
}

#' Specify Data Set to be Used
#'
#' @details \code{spec_dataset()} is closely related with the key names in
#' the return list of \code{gen_dims_comb_stats()}. This function use the same method
#' of variable name construction with \code{gen_dims_comb_stats()} and this method is
#' hard-encoding hence can not be changed.
#'
#' @param data_list list of multi-dimensions combinations statistics.
#' @param dims character vector. All dimension variable names.
#' @param ... dimension variable names to bed selected.
#' @param calc_type character. The type of statistics to be selected which should
#' be in \code{c("product", "product_drug", "multi_residue")}.
#' @param ana_threshold data frame of analysis threshold. This function will find
#' the corresponding threshold value in it.
#' @param digits numeric, default 2. Do not keep dicimal places if set NA.
#'
#' @return data frame with dimension labels and statistics.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename
spec_dataset <- function(
  data_list, dims, ana_threshold,
  calc_type = c("product", "product_drug", "multi_residue"), ...,
  digits = 2
) {
  # parse ... and get names of args
  arguments <- dplyr::quos(...)
  args_names <- unlist(lapply(arguments, dplyr::quo_name))
  # must be `intersect(dims, args_names)` since the return value is in `dims`'s order.
  # In this case, this function can correctly construct variable's name in `data_list`.
  var_name <- paste0(intersect(dims, args_names), collapse = '_')
  if (var_name == '') var_name <- "full_sample"
  data <- data_list[[var_name]]

  # determin threshold of sample size for this combination of dimensions
  threshold <- 0
  if (!missing(ana_threshold) && var_name != "full_sample") {
    threshold <- (ana_threshold %>%
                    filter(!!!rlang::parse_exprs(c(
                      paste0(setdiff(dims, args_names), " == 0"),
                      paste0(args_names, " == 1")
                    ))))[["threshold"]]
  }

  calc_type <- calc_type[1]
  if (calc_type == "product") {
    data <- data %>%
      filter(.data[["sample_size"]] >= threshold) %>%
      select(..., "sample_size", "qualification_rate") %>%
      mutate("qualification_rate" = 100 * .data[["qualification_rate"]]) %>%
      rename("qualification_rate_percent" = "qualification_rate")
    if (!is.na(digits)) {
      data <- data %>%
        mutate("qualification_rate_percent" = round(.data[["qualification_rate_percent"]], digits))
    }
  }
  else if (calc_type == "product_drug") {
    data <- data %>%
      filter(.data[["sample_size"]] >= threshold) %>%
      select(..., "sample_size", "detection_rate", "qualification_rate") %>%
      mutate(
        "detection_rate"     = 100 * .data[["detection_rate"]],
        "qualification_rate" = 100 * .data[["qualification_rate"]]
      ) %>%
      rename(
        "detection_rate_percent"     = "detection_rate",
        "qualification_rate_percent" = "qualification_rate"
      )
    if (!is.na(digits)) {
      data <- data %>%
        mutate(
          "detection_rate_percent"     = round(.data[["detection_rate_percent"]], digits),
          "qualification_rate_percent" = round(.data[["qualification_rate_percent"]], digits)
        )
    }
  }
  else if (calc_type == "multi_residue") {
    data <- data %>%
      filter(.data[["sample_size"]] >= threshold) %>%
      select(
        ..., "sample_size",
        "multi_detection_rate", "max_detection_num",
        "multi_defective_rate", "max_defective_num"
      ) %>%
      mutate(
        "multi_detection_rate" = 100 * .data[["multi_detection_rate"]],
        "multi_defective_rate" = 100 * .data[["multi_defective_rate"]]
      ) %>%
      rename(
        "multi_detection_rate_percent" = "multi_detection_rate",
        "multi_defective_rate_percent" = "multi_defective_rate"
      )
    if (!is.na(digits)) {
      data <- data %>%
        mutate(
          "multi_detection_rate_percent" = round(.data[["multi_detection_rate_percent"]], digits),
          "multi_defective_rate_percent" = round(.data[["multi_defective_rate_percent"]], digits)
        )
    }
  }
  else {
    stop("Wrong type of calculation. Choose from \"product\", \"product_drug\" and \"multi_residue\".")
  }
  return(data)
}
