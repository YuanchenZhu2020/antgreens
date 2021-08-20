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
#' # check_required_pkgs(pkgs)
check_required_pkgs <- function(pkgs_vec = c("data.table", "readxl", "readr", "bookdown")) {
  void <- lapply(
    pkgs_vec,
    function(pkg) {
      if (system.file(package = pkg) == '') utils::install.packages(pkg)
    }
  )
  message("Finished checking and installing required packages.")
}
