#' Convert Xls(x) file to CSV file in UTF-8
#'
#' @description Convert `.xls(x)` file to UTF-8 with byte order mark (can open in
#' Windows with Excel in Chinese locale). In this case, you can edit the raw data
#' file using Excel in Windows of any encoding format, convert it to `.csv` file
#' with UTF-8 BOM, and open it still in Windows with readable characters to do
#' anything you want and keep the UTF-8 encoding to simplify the data processing.
#'
#' @param raw_path character. Path of raw data file.
#' @param csv_path Character or `NA`, default `NA`. It can be the path of processed
#' data file or `NA`. If it set `NA` then the function will use the origin name of
#' raw data file and add `.UTF-8` after it.
#'
#' @return
#' a character representing the **absolute** path of the CSV file.
#' @export
#' @importFrom magrittr %>%
excel_to_utf8bom_csv <- function(raw_path, csv_path = NA) {
  if (is.na(csv_path)) {
    file_name <- paste0(
      strsplit(basename(raw_path), '.', fixed = TRUE)[[1]][1],
      ".UTF-8.csv"
    )
    csv_path <- file.path(dirname(raw_path), file_name)
  }

  # read data in the form of `.xls(x)` and write in `.csv` with UTF-8 Byte order mark
  data <- readxl::read_excel(raw_path) %>%
    readr::write_excel_csv(csv_path)
  csv_path <- normalize_path(csv_path)

  return(csv_path)
}
