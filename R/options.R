#' Variable Name of Antgreens Options List
#' @export
antgreens_opt_name <- "antgreens_options"

#' Load Initial Options for antgreens
#'
#' @return None
#' @export
#'
#' @examples
#' # load_antgreens_options()
#' @importFrom rlang !!!
load_antgreens_options <- function() {
  product_drug <- list(ProductLabelName, names(DrugNames)[1])
  names(product_drug) <- c("product", DrugNames[[1]])
  assign(
    antgreens_opt_name,
    list(
      RawColnameTrans = RawColnameTrans,
      ProductLabelName = ProductLabelName,
      DrugNames = DrugNames,
      DimsNamesTrans = utils::modifyList(RawColnameTrans, product_drug),
      ThresholdName = ThresholdName,
      DataTableI18n = DataTableI18n,
      ColnamesI18n = ColnamesI18n
    ),
    1
  )
}

#' Get \code{antgreens_options} in \code{.GlobalEnv}
#'
#' @param var_name character. The key of variable \code{antgreens_opt_name}.
#'
#' @export
#'
#' @examples
#' # library(antgreens)
#' # get_option("ProductLabelName")
get_option <- function(var_name) {
  if (!exists(antgreens_opt_name, 1)) {
    warning("There is no antgreens options in Global Environment.")
    warning("Please reload optioins using `load_antgreen_options()` and reset your options.")
    stop("Stop to reload antgreens options")
  }
  rlang::eval_tidy(rlang::call2("[[", rlang::sym(antgreens_opt_name), var_name))
}

#' Set Antgreens Option
#'
#' @param var_name character. The key of variable \code{antgreens_opt_name}.
#' @param value New value for this option.
#'
#' @return None
#' @export
#'
#' @examples
#' # library(antgreens)
#' # var_name <- "ProductLabelName"
#' # value <- ""
#' # set_option(var_name, value)
set_option <- function(var_name, value) {
  if (!exists(antgreens_opt_name, 1)) {
    warning("There is no antgreens options in Global Environment.")
    warning("Please reload optioins using `load_antgreen_options()` and reset your options.")
    stop("Stop to reload antgreens options")
  }
  rlang::eval_bare(
    rlang::call2(
      "<-", rlang::call2("[[", rlang::sym(antgreens_opt_name), var_name), value
    ),
    globalenv()
  )
  rlang::eval_bare(
    rlang::call2(
      "<-",
      rlang::call2("[[", rlang::sym(antgreens_opt_name), "DimsNamesTrans"),
      utils::modifyList(
        get_option("RawColnameTrans"),
        list(product = get_option("ProductLabelName"))
      )
    ),
    globalenv()
  )
}
