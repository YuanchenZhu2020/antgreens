#' Get the year span of the analysis
#'
#' @param csv_data_file a character representing the UTF-8 `.csv` data file.
#' @param year_colname a character. It is the name of column that represents the year.
#'
#' @return a numeric vector of length 2, representing min and max year separately.
#' @export
get_year_range <- function(csv_data_file, year_colname = "\u5e74\u4efd") {
  data <- data.table::fread(csv_data_file, encoding = "UTF-8")
  year_range <- c(
    min(data[, year_colname, with = FALSE]),
    max(data[, year_colname, with = FALSE])
  )
  return(year_range)
}

#' Check the required packages and install them
#'
#' @param pkgs_vec a character vector contains names of required packages.
#'
#' @return None (invisible `NULL`)
#'
#' @export
#'
#' @examples
#' # library(antgreens)
#' # pkgs <- c('data.table', 'tidyverse', 'Rcpp')
#' # check_install_pkgs(pkgs)
check_install_pkgs <- function(pkgs_vec = c("data.table", "tidyverse", "bookdown")) {
  void <- lapply(
    pkgs_vec,
    function(pkg) {
      if (system.file(package = pkg) == '') utils::install.packages(pkg)
    }
  )
  message("Finished checking and installing required packages.")
}

#' Generate Environment for Book Rendering
#'
#' @param data_path character. Path of csv or excel raw data file
#' @param skip_conv logical, default FALSE. If set TRUE then the \code{data_path}
#' refers to the path of CSV raw data file, otherwise refers to the path of .xls(x)
#' raw data file.
#' @param year_range numeric vector length 2, default NA.
#' @param year_colname character of column name for dimension \code{year}.
#'
#' @return environment object contains necessary objects.
#' @export
book_env <- function(
  data_path, skip_conv = FALSE, year_range = NA, year_colname = "\u5e74\u4efd"
) {
  # if choose to skip the convert of data file, then `data_path` represent the path
  # of the `.csv` file. Otherwise do converting.
  if (skip_conv) {
    csv_path <- data_path
  } else {
    csv_path <- excel_to_utf8bom_csv(data_path)
  }

  # get year range of report
  if (all(is.na(year_range))) {
    year_range <- get_year_range(csv_path, year_colname = year_colname)
    message("The year range of this report is: ", year_range[1], " to ", year_range[2])
  } else {
    message("The year range of this report is: ", year_range[1], " to ", year_range[2])
  }
  return(environment())
}

#' Automatically Render Report
#'
#' @param input_dir character, default '.'. Root directory for a bookdown project
#' (contains \code{index.Rmd} file).
#' @param skip_conv void
#' @param ... void
#' @param data_path void
#'
#' @return path of HTML book.
#' @export
auto_report <- function(input_dir = ".", data_path, skip_conv = FALSE, ...) {
  check_install_pkgs()
  # if (all(is.na(params))) {
  #   message("You must set params for rendering reports.")
  #   return(NA)
  # }
  data_path <- normalizePath(data_path)
  bookdown::render_book(input = input_dir, envir = book_env(data_path, skip_conv), ...)
}
