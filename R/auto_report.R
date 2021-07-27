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
#' @examples
#' pkgs <- c('data.table', 'tidyverse', 'Rcpp')
#' check_isntall_pkgs(pkgs)
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
#' @param data_path
#' @param skip_conv
#' @param year_colname
#' @param year_range
#'
#' @return
#' @export
book_env <- function(
  data_path, skip_conv = FALSE, year_colname = "\u5e74\u4efd", year_range = NA
) {
  # if choose to skip the convert of data file, then `data_path` represent the path
  # of the `.csv` file. Otherwise do converting.
  if (skip_conv) {
    csv_path <- data_path
  } else {
    csv_path <- excel_to_utf8bom_csv(data_path)
  }
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
#' @param input_dir
#' @param raw_data_path
#' @param skip_conv
#' @param ...
#'
#' @return
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
