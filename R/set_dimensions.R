#' Set Dimensions to be Analysis After Import antgreens
#'
#' @param product_label_name character, default NA. It is the column name both in
#' raw data and MRL data. If set NA this parameter will use the default value (See
#' use \code{get_option("ProductLabelName")})
#' @param other_labels It mey be a list or a character which is one item of antgreens options.
#' @param drug_label_name character vector, default NA. It is the name of pesticides column
#' which exists in long table. If set NA this parameter will use the default value (See
#' use \code{get_option("DrugNames")})
#'
#' @return None
set_dimensions <- function(product_label_name = NA, drug_label_name = NA, other_labels = NA) {
  if (!is.na(product_label_name)) {
    if (is.character(product_label_name) && length(product_label_name) == 1) {
      set_option("ProductLabelName", product_label_name)
    }
    else {
      stop("The product label name shoule be a character such as 'product'.")
    }
  }

  if (!is.na(drug_label_name)) {
    if (is.character(drug_label_name)) {
      set_option("DrugNames", drug_label_name)
    }
    else {
      stop("The drug label name shoule be a character or character vector.")
    }
  }

  if (any(is.na(other_labels))) {
    if (is.list(other_labels) && !("" %in% names(other_labels))) {
      set_option("RawColnameTrans", other_labels)
    }
    else {
      stop("The other labels options should be named list such as `list(year = '\u5e74\u4efd')`.")
    }
  }
}
