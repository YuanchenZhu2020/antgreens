#' Make Quarter Readable in Table
#'
#' @description use readable name of quarters to replace number of quarters.
#'
#' @param df data frame contains column named with "quarter"
#'
#' @return data frame with readable "quarter" column
#' @export
#'
#' @examples
#' df <- data.frame(quarter = c(1,2,3,4))
#' df <- make_quarter_readable(df)
make_quarter_readable <- function(df) {
  df[["quarter"]][df[["quarter"]] == 1] <- quarter_readable$`1`
  df[["quarter"]][df[["quarter"]] == 2] <- quarter_readable$`2`
  df[["quarter"]][df[["quarter"]] == 3] <- quarter_readable$`3`
  df[["quarter"]][df[["quarter"]] == 4] <- quarter_readable$`4`
  return(df)
}

#' Make Columns As Factor
#'
#' @param dataset data frame with readable "quarter" column
#' or "year"/"province"/"category"/"product"/"drug" column.
#' @param dim_names character vector.
#' @param funcs named list of customed factor function
#' @param quarter_readable logical, default TRUE.
#'
#' @return data frame with factor column. And the order of quarter is
#' depends on the order of \code{quarter_readable}; the order of year is depends on
#' the number of year; the order of province, category product and drug are depends
#' on the dictionary order of character.
#' @export
factor_dims <- function(dataset, dim_names, funcs = list(), quarter_readable = TRUE) {
  funcs <- utils::modifyList(
    funcs,
    list(
      year = factor_year,
      quarter = ifelse(quarter_readable, readable_factor_quarter, factor_quarter),
      province = factor_province,
      category = factor_category,
      product = factor_product,
      drug = factor_drug
    )
  )
  for (dn in dimnames) {
    dataset <- funcs[[dn]](dataset)
  }
  return(dataset)
}

#' @export
factor_year <- function(dataset) {
  year_levels <- sort(unique(dataset[['year']]))
  dataset[['year']] <- factor(
    dataset[['year']],
    levels = year_levels
  )
  return(dataset)
}

#' @export
factor_quarter <- function(dataset) {
  quarter_levels <- sapply(1:4, \(x) {quarter_readable[[x]]})
  dataset[['quarter']] <- factor(dataset[['quarter']], levels = quarter_levels)
  return(dataset)
}

#' @export
factor_province <- function(dataset) {
  province_levels <- sort(unique(dataset[['province']]))
  dataset[['province']] <- factor(
    dataset[['province']],
    levels = province_levels
  )
  return(dataset)
}

#' @export
factor_category <- function(dataset) {
  category_levels <- sort(unique(dataset[['category']]))
  dataset[['category']] <- factor(
    dataset[['category']],
    levels = category_levels
  )
  return(dataset)
}

#' @export
factor_product <- function(dataset) {
  product_levels <- sort(unique(dataset[['product']]))
  dataset[['product']] <- factor(
    dataset[['product']],
    levels = product_levels
  )
  return(dataset)
}

#' @export
factor_drug <- function(dataset) {
  drug_levels <- sort(unique(dataset[['drug']]))
  dataset[['drug']] <- factor(
    dataset[['drug']],
    levels = drug_levels
  )
  return(dataset)
}

#' @export
#' @importFrom magrittr %>%
readable_factor_quarter <- function(dataset) {
  dataset <- dataset %>%
    make_quarter_readable() %>%
    factor_quarter()
  return(dataset)
}
