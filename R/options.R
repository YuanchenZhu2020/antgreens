#' Variable Name of Antgreens Options List
antgreens_opt_name <- "antgreens_options"

#' Load Initial Options for antgreens
#'
#' @return None
load_antgreens_options <- function() {
  product_drug <- list(product = ProductLabelName, drug = DrugNames[1])
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
    )
  )
}

#' Clear Current Options and Reload
#'
#' @return None
reload_options <- function() {
  load_antgreens_options()
}

#' Get \code{antgreens_options} in \code{.GlobalEnv}
#'
#' @param var_name character. The key of variable \code{antgreens_opt_name}.
get_option <- function(var_name) {
  if (!exists(antgreens_opt_name)) {
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


##### Current Version #####

#' Options for DT Package
#'
#' @export
DTOptions <- list(
  # paginate
  pageLength = 10,
  lengthMenu = c(10, 15, 20, 25),
  # hard-encoding of cell width
  autoWidth = TRUE,
  # internationalization
  language = DataTableI18n$chinese,
  # align
  columnDefs = list(
    list(className = 'dt-center', targets = "_all")
  ),
  # search settings
  search = list(regex = TRUE, caseInsensitive = FALSE),
  # searchHighlight = TRUE,
  # enable this will cause disappearence of 2rd data table
  # and the developer have not fix it.
  # Extension: Buttons
  dom = 'lBfrtip',
  buttons = c('copy', 'csv', 'excel', 'print')
)
