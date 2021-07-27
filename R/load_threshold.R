#' Load Threshold Data in Excel Format
#'
#' @param path character of .xls(x) file which has threshold data.
#'
#' @return a data frame contains dimensions combination encoded with 0/1 and threshold
#' data for each combination.
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
load_threshold <- function(path) {
  dims_names <- get_option("DimsNamesTrans")

  ana_threshold <- readxl::read_excel(path) %>%
    dplyr::rename(!!!dims_names)

  colnames(ana_threshold) <- sapply(
    colnames(ana_threshold),
    function(x) {
      if (grepl(get_option("ThresholdName"), x, fixed = TRUE)) "threshold"
      else  x
    }
  )

  ana_threshold <- ana_threshold %>%
    dplyr::select(c(names(dims_names), "threshold"))
  return(ana_threshold)
}
