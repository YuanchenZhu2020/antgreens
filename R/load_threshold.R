#' Load Threshold Data in Excel Format
#'
#' @param path character of .xls(x) file which has threshold data.
#' @param dims_en2cn list where the keys are English name of dimensions and the values
#' are column names of raw data (which is usually Chinese). The default value is the
#' list of \code{\link{RawColnameTrans}}, \code{\link{ProductLabelName}}
#' and the first item of \code{\link{DrugNames}}.
#' @param threshold_key character. The keyword of threshold column name. The default
#' value is \code{\link{ThresholdName}}.
#'
#' @return a data frame contains dimensions combination encoded with 0/1 and threshold
#' data for each combination.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
load_threshold <- function(
  path, dims_en2cn = utils::modifyList(
    RawColnameTrans, list(product = ProductLabelName, drug = DrugNames[1])
  ), threshold_key = ThresholdName
) {
  # dims_en2cn <- get_option("DimsNamesTrans")

  ana_threshold <- readxl::read_excel(path) %>%
    dplyr::rename(!!!dims_en2cn)

  colnames(ana_threshold) <- sapply(
    colnames(ana_threshold),
    function(x) {
      # if (grepl(get_option("ThresholdName"), x, fixed = TRUE)) "threshold"
      if (grepl(threshold_key, x, fixed = TRUE)) "threshold"
      else  x
    }
  )

  ana_threshold <- ana_threshold %>%
    dplyr::select(c(names(dims_en2cn), "threshold"))
  return(ana_threshold)
}
