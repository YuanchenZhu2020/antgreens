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

#' Generate Text Describing the Largest and Least Items
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

#' Generate Text Describing Three Largest Items
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

#' Generate Text Describing Three Largest Two-Items
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
#' text_two_max_3(df, "\u6d4b\u8bd5\u7387", "item1", "item2", "rate")
#' text_two_max_3(df, "\u6d4b\u8bd5\u7387", "item1", "item2", "rate", conjunction = "")
#' @importFrom magrittr %>%
text_two_max_3 <- function(
  data, rate_text, item_name_1, item_name_2, rate_name, conjunction = "\u4e2d\u7684"
) {
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

#' Generate Text for Minimum and Maximum Rate Description by Year
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
          text_minmax(data %>% dplyr::filter(year == x), rate_text, item_name, rate_name)
        )
      }
    ),
    collapse = '\uff1b'
  )
  return(str)
}

#' #' Title
#' #'
#' #' @param data
#' #' @param rate_text
#' #' @param item_name
#' #' @param rate_name
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' text_quarter_minmax <- function(data, rate_text, item_name, rate_name) {
#'   str <- paste(
#'     sapply(
#'       c("第一季度", "第二季度", "第三季度", "第四季度"),
#'       function(x){
#'         paste0(
#'           x,
#'           arrange(data %>% filter(quarter == x), desc(.data[[rate_name]]))[1,][[item_name]],
#'           rate_text, "最高为 ",
#'           arrange(data %>% filter(quarter == x), desc(.data[[rate_name]]))[1,][[rate_name]], "%，",
#'           arrange(data %>% filter(quarter == x), .data[[rate_name]])[1,][[item_name]],
#'           rate_text, "最低为 ",
#'           arrange(data %>% filter(quarter == x), .data[[rate_name]])[1,][[rate_name]], "%"
#'         )
#'       }
#'     ),
#'     collapse = '；')
#'   return(str)
#' }
#'
#' #' Title
#' #'
#' #' @param data
#' #' @param year_range
#' #' @param rate_text
#' #' @param item_name
#' #' @param rate_name
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' text_timeline_minmax <- function(data, year_range, rate_text, item_name, rate_name) {
#'   str <- paste(
#'     sapply(
#'       year_range[1]:year_range[2],
#'       function(x){
#'         sapply(
#'           c("第一季度", "第二季度", "第三季度", "第四季度"),
#'           function(y) {
#'             paste0(
#'               x, " 年", y,
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[1,][[item_name]],
#'               rate_text, "最高为 ",
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[1,][[rate_name]], "%，",
#'               arrange(data %>% filter(year == x, quarter == y), .data[[rate_name]])[1,][[item_name]],
#'               rate_text, "最低为 ",
#'               arrange(data %>% filter(year == x, quarter == y), .data[[rate_name]])[1,][[rate_name]], "%"
#'             )
#'           }
#'         )
#'       }
#'     ),
#'     collapse = '；'
#'   )
#'   return(str)
#' }
#'
#' text_year_max_3 <- function(data, year_range, rate_text, item_name, rate_name) {
#'   str <- paste(
#'     sapply(
#'       year_range[1]:year_range[2],
#'       function(x){
#'         paste0(
#'           x, " 年",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[1,][[item_name]],
#'           rate_text, "最高为 ",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[1,][[rate_name]], "%，",
#'           "其次是",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[2,][[item_name]], "和",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[3,][[item_name]],
#'           "，其", rate_text, "依次为 ",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[2,][[rate_name]], "% 和 ",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[3,][[rate_name]], "%"
#'         )
#'       }
#'     ),
#'     collapse = '；'
#'   )
#'   return(str)
#' }
#'
#' text_quarter_max_3 <- function(data, rate_text, item_name, rate_name) {
#'   str <- paste(
#'     sapply(
#'       c("第一季度", "第二季度", "第三季度", "第四季度"),
#'       function(x){
#'         paste0(
#'           x,
#'           arrange(data %>% filter(quarter == x), desc(.data[[rate_name]]))[1,][[item_name]],
#'           rate_text, "最高为 ",
#'           arrange(data %>% filter(quarter == x), desc(.data[[rate_name]]))[1,][[rate_name]], "%，",
#'           "其次是",
#'           arrange(data %>% filter(quarter == x), desc(.data[[rate_name]]))[2,][[item_name]], "和",
#'           arrange(data %>% filter(quarter == x), desc(.data[[rate_name]]))[3,][[item_name]],
#'           "，其", rate_text, "依次为 ",
#'           arrange(data %>% filter(quarter == x), desc(.data[[rate_name]]))[2,][[rate_name]], "% 和 ",
#'           arrange(data %>% filter(quarter == x), desc(.data[[rate_name]]))[3,][[rate_name]], "%"
#'         )
#'       }
#'     ),
#'     collapse = '；'
#'   )
#'   return(str)
#' }
#'
#' text_timeline_max_3 <- function(data, year_range, rate_text, item_name, rate_name) {
#'   str <- paste(
#'     sapply(
#'       year_range[1]:year_range[2],
#'       function(x){
#'         sapply(
#'           c("第一季度", "第二季度", "第三季度", "第四季度"),
#'           function(y) {
#'             paste0(
#'               x, " 年", y,
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[1,][[item_name]],
#'               rate_text, "最高为 ",
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[1,][[rate_name]], "%，",
#'               "其次是",
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[2,][[item_name]], "和",
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[3,][[item_name]],
#'               "，其", rate_text, "依次为 ",
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[2,][[rate_name]], "% 和 ",
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[3,][[rate_name]], "%"
#'             )
#'           }
#'         )
#'       }
#'     ),
#'     collapse = '；'
#'   )
#'   return(str)
#' }
#'
#'
#' #' Title
#' #'
#' #' @param data
#' #' @param year_range
#' #' @param rate_text
#' #' @param item_name_1
#' #' @param item_name_2
#' #' @param rate_name
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' text_timeline_two_max <- function(
#'   data, year_range, rate_text, item_name_1, item_name_2, rate_name
#' ) {
#'   str <- paste(
#'     sapply(
#'       year_range[1]:year_range[2],
#'       function(x){
#'         sapply(
#'           c("第一季度", "第二季度", "第三季度", "第四季度"),
#'           function(y) {
#'             paste0(
#'               x, " 年", y,
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[1,][[item_name_1]],
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[1,][[item_name_2]],
#'               rate_text, "最高为 ",
#'               arrange(data %>% filter(year == x, quarter == y), desc(.data[[rate_name]]))[1,][[rate_name]], "%"
#'             )
#'           }
#'         )
#'       }
#'     ),
#'     collapse = '；'
#'   )
#'   return(str)
#' }
#'
#' text_year_two_max_3 <- function(data, year_range, rate_text, item_name_1, item_name_2, rate_name) {
#'   str <- paste(
#'     sapply(
#'       year_range[1]:year_range[2],
#'       function(x){
#'         paste0(
#'           x, " 年",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[1,][[item_name_1]], "的",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[1,][[item_name_2]],
#'           rate_text, "最高为 ",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[1,][[rate_name]], "%，",
#'           "其次是",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[2,][[item_name_1]], "的",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[2,][[item_name_2]],"和",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[3,][[item_name_1]], "的",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[3,][[item_name_2]],
#'           "，其", rate_text, "依次为 ",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[2,][[rate_name]], "% 和 ",
#'           arrange(data %>% filter(year == x), desc(.data[[rate_name]]))[3,][[rate_name]], "%"
#'         )
#'       }
#'     ),
#'     collapse = '；'
#'   )
#'   return(str)
#' }
#'
#' text_category_max_3 <- function(data, rate_text, item_name, rate_name) {
#'   category_names <- unique(data[["category"]])
#'   str <- paste(
#'     sapply(
#'       category_names,
#'       function(x){
#'         paste0(
#'           x, "在",
#'           arrange(data %>% filter(category == x), desc(.data[[rate_name]]))[1,][[item_name]], "的",
#'           rate_text, "最高为 ",
#'           arrange(data %>% filter(category == x), desc(.data[[rate_name]]))[1,][[rate_name]], "%，",
#'           "其次是",
#'           arrange(data %>% filter(category == x), desc(.data[[rate_name]]))[2,][[item_name]], "和",
#'           arrange(data %>% filter(category == x), desc(.data[[rate_name]]))[3,][[item_name]],
#'           "，其", rate_text, "依次为 ",
#'           arrange(data %>% filter(category == x), desc(.data[[rate_name]]))[2,][[rate_name]], "% 和 ",
#'           arrange(data %>% filter(category == x), desc(.data[[rate_name]]))[3,][[rate_name]], "%"
#'         )
#'       }
#'     ),
#'     collapse = '；'
#'   )
#'   return(str)
#' }
#'
#'
#' #' Title
#' #'
#' #' @param data
#' #' @param rate_text
#' #' @param item_name
#' #' @param rate_name
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' text_category_minmax <- function(data, rate_text, item_name, rate_name) {
#'   category_names <- unique(data[["category"]])
#'   str <- paste(
#'     sapply(
#'       category_names,
#'       function(x){
#'         paste0(
#'           x, "在",
#'           arrange(data %>% filter(category == x), desc(.data[[rate_name]]))[1,][[item_name]],
#'           rate_text, "最高为 ",
#'           arrange(data %>% filter(category == x), desc(.data[[rate_name]]))[1,][[rate_name]], "%，",
#'           arrange(data %>% filter(category == x), .data[[rate_name]])[1,][[item_name]],
#'           rate_text, "最低为 ",
#'           arrange(data %>% filter(category == x), .data[[rate_name]])[1,][[rate_name]], "%"
#'         )
#'       }
#'     ),
#'     collapse = '；')
#'   return(str)
#' }
#'
#' #' Title
#' #'
#' #' @param data
#' #' @param colname
#' #' @param specify
#' #' @param change
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' text_drug_detdef_max <- function(data, colname, specify, change = TRUE) {
#'   if (change) data <- data %>% change_to_defective()
#'   str <- paste0(
#'     specify, "在",
#'     arrange(data %>% filter(.data[[colname]] == specify), desc(detection_rate_percent))[["drug"]][1],
#'     "上的检出率最高为 ",
#'     arrange(data %>% filter(.data[[colname]] == specify), desc(detection_rate_percent))[["detection_rate_percent"]][1],
#'     "%，在",
#'     arrange(data %>% filter(.data[[colname]] == specify), desc(defective_rate_percent))[["drug"]][1],
#'     "上的超标率最高为 ",
#'     arrange(data %>% filter(.data[[colname]] == specify), desc(defective_rate_percent))[["defective_rate_percent"]][1],
#'     "%"
#'   )
#'   return(str)
#' }

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
