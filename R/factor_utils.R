#' Make Quarter Readable in Table
#'
#' @description use readable name of quarters to replace number of quarters.
#'
#' @param df data frame contains column named with "quarter"
#' @param quarter_trans list containing 4 characters and the names of each element is 1, 2, 3, 4,
#' which represents the four quarters.
#'
#' @return data frame with readable "quarter" column
#' @export
#'
#' @examples
#' df <- data.frame(quarter = c(1,2,3,4))
#' df <- make_quarter_readable(df)
make_quarter_readable <- function(df, quarter_trans = quarter_readable) {
  df[["quarter"]][df[["quarter"]] == 1] <- quarter_trans$`1`
  df[["quarter"]][df[["quarter"]] == 2] <- quarter_trans$`2`
  df[["quarter"]][df[["quarter"]] == 3] <- quarter_trans$`3`
  df[["quarter"]][df[["quarter"]] == 4] <- quarter_trans$`4`
  return(df)
}

#' Make Columns As Factor
#'
#' @param dataset data frame with readable \code{quarter} column
#' or \code{year}/\code{province}/\code{category}/\code{product}/\code{drug}/\code{timeline} column.
#' @param ... dimensions to be made as factor.
#' @param funcs named list of custome factor function
#' @param quarter_readable logical, default \code{TRUE}. If set \code{TRUE} then make
#' quarter column readable and as a factor.
#' @param quarter_trans list containing 4 characters and the names of each element is 1, 2, 3, 4,
#' which represents the four quarters.
#'
#' @return data frame with factor column. And the order of quarter is
#' depends on the order of \code{quarter_readable}; the order of year is depends on
#' the number of year; the order of province, category product and drug are depends
#' on the dictionary order of character.
#' @export
#' @examples
#' data <- data.frame(year = 2016:2019, quarter = c(1,2,3,4))
#' factor_dims(data, year, quarter, quarter_trans = list(`1`='a',`2`='b',`3`='c',`4`='d'))
factor_dims <- function(
  dataset, ..., funcs = list(), quarter_readable = TRUE, quarter_trans = quarter_readable
) {
  dims <- dplyr::quos(...)
  dim_names <-  unlist(lapply(dims, dplyr::quo_name))
  funcs <- utils::modifyList(
    list(
      year = factor_year,
      province = factor_province,
      category = factor_category,
      product = factor_product,
      drug = factor_drug,
      timeline = factor_timeline
    ),
    funcs
  )
  for (dn in dim_names) {
    if (dn == "quarter") {
      dataset <- factor_quarter(
        dataset, readable = quarter_readable, quarter_trans = quarter_trans
      )
    }
    else dataset <- funcs[[dn]](dataset)
  }
  return(dataset)
}

#' Make Column As Factor
#'
#' @description The subject variants of `factor_*` has:
#'   * `year`: factor numeric year variable
#'   * `quarter`: (make quarter readable and) factor quarter character
#'   * `province`: factor province character
#'   * `category`: factor category character
#'   * `product`: factor product character
#'   * `drug`: factor drug character
#'   * `timeline`: factor timeline character
#'
#' @param dataset data frame
#'
#' @param readable logical, default \code{TRUE}. If set \code{TRUE} then make
#' quarter column readable and as a factor.
#' @param quarter_trans list containing 4 characters and the names of each element is 1, 2, 3, 4,
#' which represents the four quarters.
#'
#' @export
#' @examples
#' data <- data.frame(
#' year = 2016:2019, quarter = c(1,2,3,4), province = c('ab','bc','cd','de'), category = c('ab','bc','cd','de'), product = c('ab','bc','cd','de'), drug = c('ab','bc','cd','de'))
#' quarter_trans <- list(`1`='a',`2`='b',`3`='c',`4`='d')
#' # factor year
#' factor_year(data)
#' # factor quarter
#' factor_quarter(data)
#' factor_quarter(data, readable = TRUE, quarter_trans = quarter_trans)
#' # factor province
#' factor_province(data)
#' # factor category
#' factor_category(data)
#' # factor product
#' factor_product(data)
#' # factor drug
#' factor_drug(data)
#' # factor timeline
#' factor_timeline(combine_year_quarter(data))
#' @importFrom magrittr %>%
factor_year <- function(dataset) {
  year_levels <- sort(unique(dataset[['year']]))
  dataset[['year']] <- factor(dataset[['year']], levels = year_levels)
  return(dataset)
}
#' @rdname factor_year
#' @export
factor_quarter <- function(dataset, readable = FALSE, quarter_trans = quarter_readable) {
  if (readable) {
    quarter_levels <- sapply(1:4, \(x) {quarter_trans[[x]]})
    dataset <- dataset %>%
      make_quarter_readable(quarter_trans = quarter_trans)
  }
  else quarter_levels <- 1:4
  dataset[['quarter']] <- factor(dataset[['quarter']], levels = quarter_levels)
  return(dataset)
}
#' @rdname factor_year
#' @export
factor_province <- function(dataset) {
  province_levels <- sort(unique(dataset[['province']]))
  dataset[['province']] <- factor(dataset[['province']], levels = province_levels)
  return(dataset)
}
#' @rdname factor_year
#' @export
factor_category <- function(dataset) {
  category_levels <- sort(unique(dataset[['category']]))
  dataset[['category']] <- factor(dataset[['category']], levels = category_levels)
  return(dataset)
}
#' @rdname factor_year
#' @export
factor_product <- function(dataset) {
  product_levels <- sort(unique(dataset[['product']]))
  dataset[['product']] <- factor(dataset[['product']], levels = product_levels)
  return(dataset)
}
#' @rdname factor_year
#' @export
factor_drug <- function(dataset) {
  drug_levels <- sort(unique(dataset[['drug']]))
  dataset[['drug']] <- factor(dataset[['drug']], levels = drug_levels)
  return(dataset)
}
#' @rdname factor_year
#' @export
factor_timeline <- function(dataset) {
  timeline_levels <- sort(unique(dataset[['timeline']]))
  dataset[['timeline']] <- factor(dataset[['timeline']], levels = timeline_levels)
  return(dataset)
}
