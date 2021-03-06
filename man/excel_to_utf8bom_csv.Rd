% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/excel_to_ust8bom_csv.R
\name{excel_to_utf8bom_csv}
\alias{excel_to_utf8bom_csv}
\title{Convert Xls(x) file to CSV file in UTF-8}
\usage{
excel_to_utf8bom_csv(raw_path, csv_path = NA)
}
\arguments{
\item{raw_path}{character. Path of raw data file.}

\item{csv_path}{Character or `NA`, default `NA`. It can be the path of processed
data file or `NA`. If it set `NA` then the function will use the origin name of
raw data file and add `.UTF-8` after it.}
}
\value{
a character representing the **absolute** path of the CSV file.
}
\description{
Convert `.xls(x)` file to UTF-8 with byte order mark (can open in
Windows with Excel in Chinese locale). In this case, you can edit the raw data
file using Excel in Windows of any encoding format, convert it to `.csv` file
with UTF-8 BOM, and open it still in Windows with readable characters to do
anything you want and keep the UTF-8 encoding to simplify the data processing.
}
