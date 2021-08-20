#' Check Sample Size of combinations of dimensions
#'
#' @param data data frame of raw data or renamed data.
#' @param dimensions character vector of dimensions to be analysis in \code{data}.
#' @param path character of \code{.xls(x)} file path.
#' @param drug_names dimension name for \code{drug}. The default value
#' is in \code{\link{DrugNames}}. If \code{dimensions} contains \code{drug}, then should
#' specify this parameters.
#'
#' @return a data frame of dimension combination which are encoded with 0/1 and
#' maximum sample size (\code{max_ss}), median sample size (\code{med_ss}) and minimum
#' sample size (\code{min_ss}).
#' @export
#'
#' @examples
#' data <- data.frame(a=c(1,2,1,2,1,2,1,2), b=c(1,1,2,2,3,3,1,1), c=c(1,1,1,1,1,1,2,2))
#' check_sample_size(data, dimensions = c('a', 'b', 'c'))
check_sample_size <- function(
  data, dimensions, drug_names = DrugNames, path
) {
  col_names <- c(dimensions, "min_ss", "med_ss", "max_ss")
  rows <- 2^length(dimensions)
  cols <- length(col_names)
  sample_size_stats <- matrix(0, rows, cols, dimnames = list(NULL, col_names))

  rowid <- 1
  for (i in 0:length(dimensions)) {
    dims_comb <- utils::combn(dimensions, i)
    for (j in 1:ncol(dims_comb)) {
      dims <- dims_comb[,j]
      for (c in dims) sample_size_stats[,c][rowid] <- 1

      # dims <- setdiff(dims, get_option("DrugNames"))
      dims <- base::setdiff(dims, drug_names)
      tmp <- get_sample_size_stats(
        data, draw_figure = FALSE, !!!rlang::parse_exprs(dims)
      )
      sample_size_stats[,"min_ss"][rowid] <- tmp$min_ss
      sample_size_stats[,"med_ss"][rowid] <- tmp$med_ss
      sample_size_stats[,"max_ss"][rowid] <- tmp$max_ss
      rowid <- rowid + 1
    }
  }

  sample_size_stats <- as.data.frame(sample_size_stats)
  if (!missing(path)) {
    openxlsx::write.xlsx(sample_size_stats, path, overwrite = TRUE)
    message("\nExcel file is write to: \n", normalize_path(path))
  }

  return(sample_size_stats)
}

#' Get Sample Size Statistics for Given Dimension Combnation
#'
#' @param dataset data frame of raw data or renamed data.
#' @param draw_figure logical, default \code{TRUE}. If \code{TRUE} this function will
#' draw histogram of sample size of given dimension combination.
#' @param ... dimensions
#'
#' @return a list with sample size data frame, minimum, median and maximum sample
#' size.
#' @export
#'
#' @examples
#' # data <- data.frame(a=c(1,2,1,2,1,2,1,2), b=c(1,1,2,2,3,3,1,1), c=c(1,1,1,1,1,1,2,2))
#' # get_sample_size_stats(data, draw_figure = FALSE, a, b)
#' @importFrom magrittr %>%
get_sample_size_stats <- function(dataset, draw_figure = TRUE, ...) {
  sample_size_df <- dataset %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(sample_size = dplyr::n())
  max_ss <- max(sample_size_df[["sample_size"]])
  min_ss <- min(sample_size_df[["sample_size"]])
  med_ss <- stats::median(sample_size_df[["sample_size"]])
  if (draw_figure) {
    graphics::hist(sample_size_df[["sample_size"]])
    graphics::rug(jitter(sample_size_df[["sample_size"]]))
  }
  return(list(
    "df" = sample_size_df,
    "max_ss" = max_ss,
    "min_ss" = min_ss,
    "med_ss" = med_ss
  ))
}
