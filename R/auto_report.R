#' Get the year span of the analysis
#'
#' @param csv_data_file a character representing the UTF-8 `.csv` data file.
#' @param year_colname a character. It is the name of column that represents the year.
#'
#' @return a numeric vector of length 2, representing min and max year separately.
#' @export
get_year_range <- function(csv_data_file, year_colname) {
  # It can be the default value of 'year_colname', but the PDF manual
  # cannot be generated if do so.
  if (missing(year_colname)) year_colname <- "\u5e74\u4efd"

  data <- data.table::fread(csv_data_file, encoding = "UTF-8")
  year_range <- c(
    min(data[, year_colname, with = FALSE]),
    max(data[, year_colname, with = FALSE])
  )
  return(year_range)
}

#' Generate Environment for Book Rendering
#'
#' @param year_range numeric vector length 2, default NA.
#' @param year_colname character of column name for dimension \code{year}.
#' @param params list. The parameters of report.
#'
#' @return environment object contains necessary objects.
#' @export
book_env <- function(params = list(), year_colname, year_range = NA) {
  # It can be the default value of 'year_colname', but the PDF manual
  # cannot be generated if do so.
  if (missing(year_colname)) year_colname <- "\u5e74\u4efd"

  # if raw data file specified in params is CSV, then `data_path` represent the path
  # of the `.csv` file. Otherwise do converting.
  tmp_fn <- unlist(strsplit(basename(params[["raw_data"]]), '.', fixed = TRUE))
  use_csv <- any(tmp_fn[length(tmp_fn)] == c("csv", "CSV"))
  if (use_csv) {
    csv_path <- params[["raw_data"]]
  } else {
    csv_path <- excel_to_utf8bom_csv(params[["raw_data"]])
    params[["raw_data"]] <- csv_path
  }
  # specified year_range of report
  if (all(is.na(year_range))) {
    year_range <- get_year_range(csv_path, year_colname = "\u5e74\u4efd")
    message("The year range of this report is: ", year_range[1], " to ", year_range[2])
  } else {
    message("The year range of this report is: ", year_range[1], " to ", year_range[2])
  }
  return(environment())
}

#' Update Compile Parameters
#'
#' @description Use given compile parameters to update the default parameters.
#'
#' @param new_params_list list. The given parameters.
#'
#' @return list.
#' @export 
update_params <- function(new_params_list = list()) {
  default_params <- list(
    cran_mirror = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/",
    raw_data = "../Data/raw_data/vegetable_58_2016-2020.xlsx",
    mrl_data = "../Data/raw_data/MRL_2020.xlsx",
    products = "../Data/raw_data/products.xlsx",
    pesticides = "../Data/raw_data/pesticides.xlsx",
    province = "../Data/raw_data/province.xlsx",
    ana_threshold = "../Data/multi_dim_check.xlsx",
    codes_dir = "../Codes",
    ana_dims_part = list(
      year = "\u5e74\u4efd", quarter = "\u5b63\u5ea6",
      province = "\u7701\u4efd", category = "\u4ea7\u54c1\u7c7b\u522b"
    ),
    ana_dims_product = "\u4ea7\u54c1\u540d\u79f0",
    ana_dims_drug = "\u836f\u7269",
    threshold_kw = "\u9608\u503c",
    st_drug = "\u7532\u80fa\u78f7",
    ed_drug = "\u4e8c\u7532\u620a\u7075",
    product_env = TRUE,
    digits = 2,
    colname_map = list(category = "\u852c\u83dc\u7c7b\u522b", drug = "\u519c\u836f")
  )
  params <- utils::modifyList(default_params, new_params_list)
  return(params)
}

#' Automatically Render Report
#'
#' @description This function is a wrapper of \code{bookdown::render_book} and is designed
#' to render report automatically using given parameters and do not need to edit source
#' report files.
#'
#' @param input_dir character, default \code{'./Report'}. Root directory for a bookdown project.
#' @param params_list list. The compile parameters to update the default parameters.
#' @param output_dir character, default \code{'_book'}. It is the directory name of output report.
#'
#' @export
auto_report <- function(input_dir = "./Report", params_list = list(), output_dir = "_book") {
  check_required_pkgs()
  params <- update_params(params_list)
  bookdown::render_book(
    input = input_dir,
    envir = book_env(params = params),
    output_dir = output_dir
  )
}
