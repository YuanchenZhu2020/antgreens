#' Calculate treated data using raw data and MRL
#'
#' @description `calc_treated_data()` construct a new data frame which construction
#' is like raw_data that has multiple columns as labels and the renaming columns are
#' detection values of each drug on each sample.
#'
#' @param raw_data a data frame. It is raw detection data of each sample on each drug.
#'   It has multiple columns as labels of each sample and the remaining columns are
#'   raw detection values.
#' @param mrl_data a data frame. It has one column named "\\u4ea7\\u54c1\\u540d\\u79f0"
#'   and the other columns named with name of drugs. Every row is MRL value of one
#'   product on each drugs.
#' @param st_drug a character. Name of the first column of drug.
#' @param ed_drug a character. Name of the last column of drug.
#' @param raw_col_en2cn list, default \code{\link{RawColnameTrans}}. It is the
#'   English to Chinese names of dimensions except product.
#' @param product_name character. It is the product dimension name.
#'
#' @return
#' a data frame with 3 extra columns compared with `raw_data`.
#' @export
#'
#' @examples
#' # library(antgreens)
#' # set_dimensions("name", list(t1="t1", t2="t2"))
#' # df <- data.frame(name = c('a', 'b', 'a', 'c', 'c'),t1 = c(1,2,3,4,5),t2 = c(0,9,8,7,6))
#' # mrl <- data.frame(name = c('a','b','c'),t1 = c(2,2,4),t2 = c(4,10,6))
#' # mrl_map <- mapping_mrl(as.matrix(mrl[,2:3]), mrl$product)
#' # nvm <- calc_treated_data(as.matrix(df[,2:3]), mrl_map, "t1", "t2")
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @importFrom data.table := as.data.table
#' @importFrom dplyr rename
calc_treated_data <- function(
  raw_data, mrl_data, st_drug, ed_drug,
  raw_col_en2cn = RawColnameTrans, product_name = ProductLabelName
) {
  # get label and drug columns' index
  raw_colnames <- colnames(raw_data)
  drug_name_inds <- which(st_drug == raw_colnames):which(ed_drug == raw_colnames)
  label_name_inds <- base::setdiff(1:length(raw_colnames), drug_name_inds)

  # rename columns and convert data to data table
  raw_data <- raw_data %>%
    as.data.frame() %>%
    rename(!!!raw_col_en2cn, "product" = product_name) %>%
    as.data.table()
  mrl_data <- mrl_data %>%
    as.data.frame() %>%
    rename("product" = product_name) %>%
    as.data.table()
  # store treated data select columns which are labels
  treated_data_labels <- raw_data[, label_name_inds, with = FALSE]

  # generate MRL mapping list
  MRL <- mapping_mrl(as.matrix(mrl_data[,-"product"]), mrl_data[["product"]])
  # calculate treated matrix
  treated_matrix <- calc_residue_index(
    as.matrix(raw_data[, drug_name_inds, with = FALSE]),
    treated_data_labels[["product"]],
    MRL
  )

  # reset colnames (drugs' name)
  colnames(treated_matrix) <- raw_colnames[drug_name_inds]
  # add labels columns
  treated_data <- cbind(treated_data_labels, treated_matrix)

  # calculate multi-residue detection number
  treated_data[,
    "multi_detection_num" := apply(.SD, 1, function(x){sum(x > 0)}),
    .SDcols = drug_name_inds
  ]

  # calculate multi-residue defective number
  treated_data[,
    "multi_defective_num" := apply(.SD, 1, function(x){sum(x > 1)}),
    .SDcols = drug_name_inds
  ]

  # add label that indicates whether this sample is qulified
  treated_data[,
    "is_qualified" := apply(.SD, 1, function(x) {all(x <= 1)}),
    .SDcols = drug_name_inds
  ]

  return(treated_data)
}
