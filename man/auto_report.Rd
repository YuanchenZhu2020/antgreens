% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auto_report.R
\name{auto_report}
\alias{auto_report}
\title{Automatically Render Report}
\usage{
auto_report(input_dir = "./Report", params_list = list(), output_dir = "_book")
}
\arguments{
\item{input_dir}{character, default \code{'./Report'}. Root directory for a bookdown project.}

\item{params_list}{list. The compile parameters to update the default parameters.}

\item{output_dir}{character, default \code{'_book'}. It is the directory name of output report.}
}
\description{
This function is a wrapper of \code{bookdown::render_book} and is designed
to render report automatically using given parameters and do not need to edit source
report files.
}
