#' Make Columns As Factor
#'
#' @param dataset data frame with readable "quarter" column
#' or "year"/"province"/"category"/"product"/"drug" column.
#' @param dim_names character vector.
#' @param funcs named list of customed factor function
#' @param quarter_readable logical, default TRUE.
#'
#' @return data frame with factor column. And the order of quarter is
#' depends on the order of \code{quarter_readable}; the order of year is depends on
#' the number of year; the order of province, category product and drug are depends
#' on the dictionary order of character.
#' @export
factor_dims <- function(dataset, dim_names, funcs = list(), quarter_readable = TRUE) {
  funcs <- utils::modifyList(
    funcs,
    list(
      year = factor_year,
      quarter = ifelse(quarter_readable, readable_factor_quarter, factor_quarter),
      province = factor_province,
      category = factor_category,
      product = factor_product,
      drug = factor_drug
    )
  )
  for (dn in dimnames) {
    dataset <- funcs[[dn]](dataset)
  }
  return(dataset)
}

#'
#' @export
factor_year <- function(dataset) {
  year_levels <- sort(unique(dataset[['year']]))
  dataset[['year']] <- factor(
    dataset[['year']],
    levels = year_levels
  )
  return(dataset)
}

#'
#' @export
factor_quarter <- function(dataset) {
  quarter_levels <- sapply(1:4, \(x) {quarter_readable[[x]]})
  dataset[['quarter']] <- factor(dataset[['quarter']], levels = quarter_levels)
  return(dataset)
}

#'
#' @export
factor_province <- function(dataset) {
  province_levels <- sort(unique(dataset[['province']]))
  dataset[['province']] <- factor(
    dataset[['province']],
    levels = province_levels
  )
  return(dataset)
}

#'
#' @export
factor_category <- function(dataset) {
  category_levels <- sort(unique(dataset[['category']]))
  dataset[['category']] <- factor(
    dataset[['category']],
    levels = category_levels
  )
  return(dataset)
}

#'
#' @export
factor_product <- function(dataset) {
  product_levels <- sort(unique(dataset[['product']]))
  dataset[['product']] <- factor(
    dataset[['product']],
    levels = product_levels
  )
  return(dataset)
}

#'
#' @export
factor_drug <- function(dataset) {
  drug_levels <- sort(unique(dataset[['drug']]))
  dataset[['drug']] <- factor(
    dataset[['drug']],
    levels = drug_levels
  )
  return(dataset)
}

#'
#' @export
#' @importFrom magrittr %>%
readable_factor_quarter <- function(dataset) {
  dataset <- dataset %>%
    make_quarter_readable() %>%
    factor_quarter()
  return(dataset)
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
  threshold_line <- ana_threshold %>%
    filter(!!!rlang::parse_exprs(c(
      paste0(setdiff(dims, arg_dims), " == 0"),
      paste0(args_names, " == 1")
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
      filter(sample_size >= threshold) %>%
      select(..., sample_size, qualification_rate) %>%
      mutate(qualification_rate = 100 * qualification_rate) %>%
      rename(qualification_rate_percent = qualification_rate)
    if (!is.na(digits)) {
      data <- data %>%
        mutate(qualification_rate_percent = round(qualification_rate_percent, digits))
    }
  }
  else if (calc_type == "product_drug") {
    data <- data %>%
      filter(sample_size >= threshold) %>%
      select(..., sample_size, detection_rate, qualification_rate) %>%
      mutate(
        detection_rate     = 100 * detection_rate,
        qualification_rate = 100 * qualification_rate
      ) %>%
      rename(
        detection_rate_percent    = detection_rate,
        qualification_rate_percent = qualification_rate
      )
    if (!is.na(digits)) {
      data <- data %>%
        mutate(
          detection_rate_percent = round(detection_rate_percent, digits),
          qualification_rate_percent = round(qualification_rate_percent, digits)
        )
    }
  }
  else if (calc_type == "multi_residue") {
    data <- data %>%
      filter(sample_size >= threshold) %>%
      select(
        ..., sample_size,
        multi_detection_rate, max_detection_num,
        multi_defective_rate, max_defective_num
      ) %>%
      mutate(
        multi_detection_rate = 100 * multi_detection_rate,
        multi_defective_rate = 100 * multi_defective_rate
      ) %>%
      rename(
        multi_detection_rate_percenet = multi_detection_rate,
        multi_defective_rate_percenet = multi_defective_rate
      )
    if (!is.na(digits)) {
      data <- data %>%
        mutate(
          multi_detection_rate_percenet = round(multi_detection_rate_percenet, digits),
          multi_defective_rate_percenet = round(multi_defective_rate_percenet, digits)
        )
    }
  }
  else {
    stop("Wrong type of calculation. Choose from \"product\", \"product_drug\" and \"multi_residue\".")
  }
  return(data)
}
