#' Normalize paths
#'
#' @description a wrapper function of \code{\link{normalizePath}()} with different
#' default parameters.
#'
#' @param path character or character vector of file paths.
#' @param winslash the separator to be used on Windows. Default is
#'  \code{\link{.Platform$file.sep}}. Must be one of \code{c('/', '\\')}.
#' @param must_work logical, default FALSE.
#'
#' @export
#'
#' @examples library(antgreens)
#' normalize_path(c('~', NA, "file/path"))
normalize_path <- function(path, winslash = .Platform$file.sep, must_work = FALSE) {
  res <- normalizePath(path, winslash = winslash, mustWork = must_work)
  return(res)
}
