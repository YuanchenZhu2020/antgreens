#' Generate Range of Numeric Column in Text Form
#'
#' @description \code{text_range()} generate range of numeric column in data frame
#' in the form of \code{<number><symbol> ~ <number><symbol>}. If the column name has
#' "percent" string, then the \code{symbol} will be \code{'\%'}, otherwise is a void string.
#'
#' @param data a data frame
#' @param col character. The name of a numeric column.
#'
#' @return character in the form of \code{<number><symbol> ~ <number><symbol>}
#' @export
#'
#' @examples
#' df <- data.frame(a = runif(10, 10, 100))
#' text_range(df, 'a')
text_range <- function(data, col) {
  add_sym <- ifelse(grepl("percent", col), "%", "")
  str <- paste0(
    min(data[[col]]), add_sym,
    " ~ ",
    max(data[[col]]), add_sym
  )
  return(str)
}

#' Generate Text Describing Maximum Two-Items
#'
#' @param data a data frame
#' @param rate_text character. The character used in the text to represent the rate name.
#' @param item_name_1 character. The name of column in \code{data} storing the name of first item.
#' @param item_name_2 character. The name of column in \code{data} storing the name of second item.
#' @param rate_name character. The name of column in \code{data} storing the value of rate.
#' @param conjunction character. It is the conjunction between the first
#'   item and the second item to make the sentence more fluent.
#'
#' @return character of a sentence.
#' @export
#'
#' @examples
#' df <- data.frame(rate = runif(4), item1 = c("a",'b','c','d'), item2 = c("ab",'bc','cd','de'))
#' text_two_max(df, "\u6d4b\u8bd5\u7387", "item1", "item2", "rate")
#' text_two_max(df, "\u6d4b\u8bd5\u7387", "item1", "item2", "rate", conjunction = " | ")
#' @importFrom magrittr %>%
text_two_max <- function(data, rate_text, item_name_1, item_name_2, rate_name, conjunction = "") {
  data_desc <- data %>% dplyr::arrange(dplyr::desc(.data[[rate_name]]))
  str <- paste0(
    data_desc[1,][[item_name_1]], conjunction, data_desc[1,][[item_name_2]],
    rate_text, "\u6700\u9ad8\u4e3a ", data_desc[1,][[rate_name]], "%"
  )
  return(str)
}

#' Generate Text Describing Maximum Detection and Defective Rate for Special Item
#'
#' @param data a data frame.
#' @param colname character. The name of the item column.
#' @param specify character. The specified value for \code{colname}.
#' @param change logical, default TRUE. Whether to change qualification rate to fraction defective.
#'
#' @return character of a sentence.
#' @export
#'
#' @examples
#' df <- data.frame(item = c('a','b','a','a'), detection_rate_percent = runif(4,0,100), qualification_rate_percent = runif(4,0,100))
#' text_drug_detdef_max(df, "item", 'a')
text_drug_detdef_max <- function(data, colname, specify, change = TRUE) {
  if (change) data <- data %>% change_to_defective()
  data_det_desc <- data %>%
    dplyr::filter(.data[[colname]] == specify) %>%
    dplyr::arrange(dplyr::desc(.data[["detection_rate_percent"]]))
  data_def_desc <- data %>%
    dplyr::filter(.data[[colname]] == specify) %>%
    dplyr::arrange(dplyr::desc(.data[["defective_rate_percent"]]))
  str <- paste0(
    specify, "\u5728",
    data_det_desc[["drug"]][1], "\u4e0a\u7684\u68c0\u51fa\u7387\u6700\u9ad8\u4e3a ",
    data_det_desc[["detection_rate_percent"]][1], "%\uff0c\u5728",
    data_def_desc[["drug"]][1], "\u4e0a\u7684\u8d85\u6807\u7387\u6700\u9ad8\u4e3a ",
    data_def_desc[["defective_rate_percent"]][1], "%"
  )
  return(str)
}

#' Generate Text Describing the Maximum and Minimum Items
#'
#' @param data a data frame
#' @param rate_text character. The character used in the text to represent the rate name.
#' @param item_name character. The name of column in \code{data} storing the name of item.
#' @param rate_name character. The name of column in \code{data} storing the value of rate.
#'
#' @return character of a sentence.
#' @export
#'
#' @examples
#' df <- data.frame(rate = runif(4), item = c("a",'b','c','d'))
#' text_minmax(df, "\u6d4b\u8bd5\u7387", "item", "rate")
#' @importFrom magrittr %>%
text_minmax <- function(data, rate_text, item_name, rate_name) {
  data_desc <- data %>% dplyr::arrange(dplyr::desc(.data[[rate_name]]))
  data_asc <- data %>% dplyr::arrange(data, .data[[rate_name]])
  str <- paste0(
    data_desc[1,][[item_name]], "\u7684",
    rate_text, "\u6700\u9ad8\u4e3a ", data_desc[1,][[rate_name]], "%\uff0c",
    data_asc[1,][[item_name]], "\u7684",
    rate_text, "\u6700\u4f4e\u4e3a ", data_asc[1,][[rate_name]], "%"
  )
  return(str)
}

#' Generate Text Describing Three Maximum Items
#'
#' @inheritParams text_minmax
#'
#' @return character of a sentence.
#' @export
#'
#' @examples
#' df <- data.frame(rate = runif(4), item = c("a",'b','c','d'))
#' text_max_3(df, "\u6d4b\u8bd5\u7387", "item", "rate")
#' @importFrom magrittr %>%
text_max_3 <- function(data, rate_text, item_name, rate_name) {
  data_desc <- data %>% dplyr::arrange(dplyr::desc(.data[[rate_name]]))
  str <- paste0(
    data_desc[1,][[item_name]], "\u7684",
    rate_text, "\u6700\u9ad8\u4e3a ", data_desc[1,][[rate_name]], "%\uff0c",
    "\u5176\u6b21\u662f",
    data_desc[2,][[item_name]], "\u548c", data_desc[3,][[item_name]],
    "\uff0c\u5176", rate_text, "\u4f9d\u6b21\u4e3a ",
    data_desc[2,][[rate_name]], "% \u548c ", data_desc[3,][[rate_name]], "%"
  )
  return(str)
}

#' Generate Text Describing Three Maximum Two-Items
#'
#' @inheritParams text_two_max
#'
#' @return character of a sentence.
#' @export
#'
#' @examples
#' df <- data.frame(rate = runif(4), item1 = c("a",'b','c','d'), item2 = c("ab",'bc','cd','de'))
#' text_two_max_3(df, "\u6d4b\u8bd5\u7387", "item1", "item2", "rate")
#' text_two_max_3(df, "\u6d4b\u8bd5\u7387", "item1", "item2", "rate", conjunction = "")
#' @importFrom magrittr %>%
text_two_max_3 <- function(
  data, rate_text, item_name_1, item_name_2, rate_name, conjunction
) {
  # It can be the default value of 'conjunction', but the PDF manual
  # cannot be generated if do so.
  if (missing(conjunction)) conjunction <- "\u4e2d\u7684"

  data_desc <- data %>% dplyr::arrange(dplyr::desc(.data[[rate_name]]))
  str <- paste0(
    data_desc[1,][[item_name_1]], conjunction, data_desc[1,][[item_name_2]], "\u7684",
    rate_text, "\u6700\u9ad8\u4e3a ", data_desc[1,][[rate_name]], "%\uff0c",
    "\u5176\u6b21\u662f",
    data_desc[2,][[item_name_1]], conjunction, data_desc[2,][[item_name_2]], "\u548c",
    data_desc[3,][[item_name_1]], conjunction, data_desc[3,][[item_name_2]],
    "\uff0c\u5176", rate_text, "\u4f9d\u6b21\u4e3a ",
    data_desc[2,][[rate_name]], "% \u548c ",
    data_desc[3,][[rate_name]], "%"
  )
  return(str)
}


#' Generate Text Describing Minimum and Maximum Rate Item By Year
#'
#' @description Generate text describing minimum and maximum rate item for each year.
#'
#' @inheritParams text_minmax
#'
#' @param year_range numeric vector represents the year.
#'
#' @return a character of sentence.
#' @export
#'
#' @importFrom magrittr %>%
text_year_minmax <- function(data, year_range, rate_text, item_name, rate_name) {
  str <- paste(
    sapply(
      year_range[1]:year_range[2],
      function(x){
        paste0(
          x, " \u5e74",
          text_minmax(data %>% dplyr::filter(.data[["year"]] == x), rate_text, item_name, rate_name)
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}

#' Generate Text Describing Minimum and Maximum Rate Item By Quarter
#'
#' @description Generate text describing minimum and maximum rate item for each quarter.
#'
#' @inheritParams text_minmax
#'
#' @return a character of sentence.
#' @export
#' @importFrom magrittr %>%
text_quarter_minmax <- function(data, rate_text, item_name, rate_name) {
  str <- paste(
    sapply(
      c("\u7b2c\u4e00\u5b63\u5ea6", "\u7b2c\u4e8c\u5b63\u5ea6", "\u7b2c\u4e09\u5b63\u5ea6", "\u7b2c\u56db\u5b63\u5ea6"),
      function(x){
        paste0(
          x,
          text_minmax(data %>% filter(.data[["quarter"]] == x), rate_text, item_name, rate_name)
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}

#' Generate Text Describing Minimum and Maximum Rate Item By Year-Quarter
#'
#' @description Generate text describing minimum and maximum rate item for each year-quarter.
#'
#' @inheritParams text_year_minmax
#'
#' @return a character of sentence.
#' @export
#' @importFrom magrittr %>%
text_timeline_minmax <- function(data, year_range, rate_text, item_name, rate_name) {
  str <- paste(
    sapply(
      year_range[1]:year_range[2],
      function(x){
        sapply(
          c("\u7b2c\u4e00\u5b63\u5ea6", "\u7b2c\u4e8c\u5b63\u5ea6", "\u7b2c\u4e09\u5b63\u5ea6", "\u7b2c\u56db\u5b63\u5ea6"),
          function(y) {
            paste0(
              x, " \u5e74", y,
              text_minmax(
                data %>% filter(.data[["year"]] == x, .data[["quarter"]] == y),
                rate_text, item_name, rate_name
              )
            )
          }
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}

#' Generate Text Describing Minimum and Maximum Rate Item By Category
#'
#' @inheritParams text_minmax
#'
#' @return a character of sentence.
#' @export
#' @importFrom magrittr %>%
text_category_minmax <- function(data, rate_text, item_name, rate_name) {
  category_names <- unique(data[["category"]])
  str <- paste(
    sapply(
      category_names,
      function(x){
        paste0(
          x, "\u5728",
          text_minmax(data %>% filter(.data[["category"]] == x), rate_text, item_name, rate_name)
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}


#' Generate Text Describing Three Maximum Two-Items By Year-Quarter
#'
#' @inheritParams text_year_two_max_3
#'
#' @return a character of sentence.
#' @export
text_timeline_two_max <- function(
  data, year_range, rate_text, item_name_1, item_name_2, rate_name
) {
  str <- paste(
    sapply(
      year_range[1]:year_range[2],
      function(x){
        sapply(
          c("\u7b2c\u4e00\u5b63\u5ea6", "\u7b2c\u4e8c\u5b63\u5ea6", "\u7b2c\u4e09\u5b63\u5ea6", "\u7b2c\u56db\u5b63\u5ea6"),
          function(y) {
            paste0(
              x, " \u5e74", y,
              text_two_max(
                data %>% filter(.data[["year"]] == x, .data[["quarter"]] == y),
                rate_text, item_name_1, item_name_2, rate_name
              )
            )
          }
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}


#' Generate Text Describing Three Maximum Items By Year
#'
#' @inheritParams text_year_minmax
#'
#' @return a character of sentence.
#' @export
#' @importFrom magrittr %>%
text_year_max_3 <- function(data, year_range, rate_text, item_name, rate_name) {
  str <- paste(
    sapply(
      year_range[1]:year_range[2],
      function(x){
        paste0(
          x, " \u5e74",
          text_max_3(data %>% filter(.data[["year"]] == x), rate_text, item_name, rate_name)
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}

#' Generate Text Describing Three Maximum Items By Quarter
#'
#' @inheritParams text_max_3
#'
#' @return a character of sentence.
#' @export
#' @importFrom magrittr %>%
text_quarter_max_3 <- function(data, rate_text, item_name, rate_name) {
  str <- paste(
    sapply(
      c("\u7b2c\u4e00\u5b63\u5ea6", "\u7b2c\u4e8c\u5b63\u5ea6", "\u7b2c\u4e09\u5b63\u5ea6", "\u7b2c\u56db\u5b63\u5ea6"),
      function(x){
        paste0(
          x,
          text_max_3(data %>% filter(.data[["quarter"]] == x), rate_text, item_name, rate_name)
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}

#' Generate Text Describing Three Maximum Items By Year-Quarter
#'
#' @inheritParams text_year_max_3
#'
#' @return a character of sentence.
#' @export
#' @importFrom magrittr %>%
text_timeline_max_3 <- function(data, year_range, rate_text, item_name, rate_name) {
  str <- paste(
    sapply(
      year_range[1]:year_range[2],
      function(x){
        sapply(
          c("\u7b2c\u4e00\u5b63\u5ea6", "\u7b2c\u4e8c\u5b63\u5ea6", "\u7b2c\u4e09\u5b63\u5ea6", "\u7b2c\u56db\u5b63\u5ea6"),
          function(y) {
            paste0(
              x, " \u5e74", y,
              text_max_3(
                data %>% filter(.data[["year"]] == x, .data[["quarter"]] == y),
                rate_text, item_name, rate_name
              )
            )
          }
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}

#' Generate Text Describing Three Maximum Two-Items By Category
#'
#' @inheritParams text_max_3
#'
#' @return a character of sentence.
#' @export
text_category_max_3 <- function(data, rate_text, item_name, rate_name) {
  category_names <- unique(data[["category"]])
  str <- paste(
    sapply(
      category_names,
      function(x){
        paste0(
          x, "\u5728",
          text_max_3(data %>% filter(.data[["category"]] == x), rate_text, item_name, rate_name)
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}


#' Generate Text Describing Three Maximum Two-Items By Year
#'
#' @inheritParams text_two_max_3
#'
#' @param year_range numeric vector represents the year.
#'
#' @return a character of sentence.
#' @export
text_year_two_max_3 <- function(
  data, year_range, rate_text, item_name_1, item_name_2, rate_name, conjunction
) {
  # It can be the default value of 'conjunction', but the PDF manual
  # cannot be generated if do so.
  if (missing(conjunction)) conjunction <- "\u7684"

  str <- paste(
    sapply(
      year_range[1]:year_range[2],
      function(x){
        paste0(
          x, " \u5e74",
          text_two_max_3(data %>% filter(.data[["year"]] == x), rate_text, item_name_1, item_name_2, rate_name, conjunction)
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}


#' Generate Text Describing the Threshold
#'
#' @description This function
#'
#' @param threshold_data a data frame with threshold information.
#' @param dims character vector. All dimensions.
#' @param ana_dims character vector. The dimensions to be analysed in this case.
#'
#' @return character of paragraph that describes the threshold in the case.
#' @export
#'
#' @examples
#' df <- data.frame(year=c(1,0,0,1),quarter=c(0,1,0,0),province=c(0,0,1,1),threshold = runif(4))
#' text_threshold(df, c('year','quarter','province'), c('year', 'province'))
text_threshold <- function(threshold_data, dims, ana_dims) {
  colname_map <- getOption("report.options")$colnameMap
  cn_dims <- unname(
    sapply(
      ana_dims, function(x) {ifelse(!is.null(colname_map[[x]]), colname_map[[x]], x)}
    )
  )
  actual_dims <- paste0(
    c('', cn_dims), collapse = "\u67d0"
  )
  threshold <- get_threshold(threshold_data, dims, ana_dims)
  str <- paste0(
    "&emsp;&emsp;", actual_dims,
    "\u62bd\u68c0\u6837\u672c\u91cf\u5927\u4e8e\u7b49\u4e8e ", threshold,
    " \u4f8b\u624d\u80fd\u53c2\u4e0e\u5206\u6790\u3002"
  )
  return(str)
}
