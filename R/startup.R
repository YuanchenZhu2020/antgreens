#' Report Template
#'
#' @description This is a analysis report template of vegetables pesticide residue. You can
#' use this template as the beginning and develop your specified report quickly.
#'
#' @format a folder with three subdirectories:
#' \describe{
#'   \item{Codes}{contains \code{*.R} files which will be used in .Rmd report.}
#'   \item{Data}{contains raw data and processed data}
#'     \item{raw_data}{contains raw data files which are msotly \code{.xls(x)} files.}
#'   \item{Report}{bookdown project of report}
#'     \item{css}{forlder contains css files}
#'     \item{latex}{folder contains tex files}
#'     \item{*.Rmd}{Rmarkdown files of report}
#'     \item{...}{Other files bookdown needed}
#' }
#'
#' @source \url{http://formlesslab.top/}
TemplateHelp <- paste0(
  "Report template in antgreens package is written by author and is specified for vegetables.",
  "You can quickly develop own analysis report based on this template, ",
  "since it has set many details and provides many functions, ",
  "covering the output of interactive table and figure using `DT` and `echarts4r` and ",
  "static figure using `ggplot2`.",
  "To startup, using `use_template('~/dir/to/report', 'report_name')` to copy template ",
  "to specified directory and then use `AutoHelp` to see how to render the report."
)

#' Copy Report Template to Directory
#'
#' @param proj_dir path of report directory.
#' @param proj_name name of directory which will be the report project.
#'
#' @return None
#' @export
use_template <- function(proj_dir, proj_name = "vegetables_residue_report") {
  if (!dir.exists(proj_dir)) {
    dir.create(proj_dir)
  }
  proj_path <- normalize_path(file.path(proj_dir, proj_name))
  if (!dir.exists(proj_path)) {
    dir.create(proj_path)
  } else {
    stop(sprintf("Directory %s already exists.", proj_path))
  }

  template_dir <- system.file("extdata", "report_template", package = "antgreens")
  dirs_files <- dir(template_dir, include.dirs = TRUE, recursive = TRUE)
  void <- sapply(
    dirs_files,
    \(x) {
      if (utils::file_test('-d', file.path(template_dir, x))) {
        dir.create(file.path(proj_path, x))
      }
      else {
        file.copy(file.path(template_dir, x), file.path(proj_path, x))
      }
    }
  )
  # file.copy(template_dir, proj_dir, recursive = TRUE, copy.mode = FALSE)
  # tmp_path <- normalize_path(file.path(proj_dir, "report_template"))
  # file.rename(tmp_path, proj_path)
  setwd(proj_path)
}
