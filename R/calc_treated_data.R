#' Calculate multiple detection number
#'
#' @description This function calculate the multiple
#' detection number of one sample. The value of multiple
#' detection number is the number of components greater
#' than 0 in a vector.
#'
#' @param res_ind_vector It can be a numeric vector or
#' data.frame with one numeric row. And its significance should
#' be the ratio of the detection value of samples on each
#' pesticide to MRL(maximum residue limit).
#'
#' @return
#' An integer number that is multiple detection number.
#' @export
#'
#' @examples
#' # when res_ind_vector is a numeric vector:
#' x <- c(0.1, 0.2, 0, 0, 0.3)
#' get_multi_detection_num(x)
#'
#' # when want to apply this function on a data.frame:
#' library(dplyr)
#' library(magrittr)
#' df <- data.frame(a=c(0.1,0,0.3), b=c(0,0,0), c=c(0,0.2,0.3))
#' df %>%
#'     rowwise() %>%
#'     mutate(mdn = get_multi_detection_num(c_across(a:c)))
#' @importFrom magrittr %>%
get_multi_detection_num <- function(res_ind_vector) {
  .Deprecated("This function will be removed in next version.")
  if (is.data.frame(res_ind_vector)) {
    res_ind_vector <- as.numeric(res_ind_vector)
  }
  multi_dn <- ifelse(res_ind_vector > 0, 1, 0) %>%
    sum()
  return(multi_dn)
}

#' Get multiple defective number
#'
#' @description This function calculate the multiple
#' defective number of one sample. The value of multiple
#' defective number is the number of components greater
#' than 1 in a vector.
#'
#' @param res_ind_vector It can be a numeric vector or
#' data.frame with one numeric row. And its significance should
#' be the ratio of the detection value of samples on each
#' pesticide to MRL(maximum residue limit).
#'
#' @return
#' An integer number that is multiple defective number.
#' @export
#'
#' @examples
#' # when res_ind_vector is a numeric vector:
#' x <- c(0.1, 0.2, 0, 0, 0.3)
#' get_multi_defective_num(x)
#'
#' # when want to apply this function on a data.frame:
#' library(dplyr)
#' library(magrittr)
#' df <- data.frame(a=c(0.1,0,0.3), b=c(0,0,0), c=c(0,0.2,0.3))
#' df %>%
#'     rowwise() %>%
#'     mutate(mdn = get_multi_defective_num(c_across(a:c)))
#' @importFrom magrittr %>%
get_multi_defective_num <- function(res_ind_vector) {
  .Deprecated("This function will be removed in next version.")
  if (is.data.frame(res_ind_vector)) {
    res_ind_vector <- as.numeric(res_ind_vector)
  }
  multi_dn <- ifelse(res_ind_vector > 1, 1, 0) %>%
    sum()
  return(multi_dn)
}

#' Get qualified label
#'
#' @description This function check whether there exists at least
#' one component that greater than 1 in a vector. If all components are
#' less than or equal to 1, this sample is qualified, otherwise is not qualified.
#'
#' @param res_ind_vector It can be a numeric vector or
#' data.frame with one numeric row. And its significance should
#' be the ratio of the detection value of samples on each
#' pesticide to MRL(maximum residue limit).
#'
#' @return
#' If a sample is calculating to be qualified, return 1. Otherwise return 0.
#' @export
#'
#' @examples
#' # qualified sample (vector)
#' q <- c(0,0,0,0,0.01,0,0,0.2)
#' is_qualified(q)
#'
#' # unqualified sample (vector)
#' uq <- c(0,0,0,0,0.01,0,0,1.2)
#' is_qualified(uq)
#'
#' # samples (data.frame)
#' library(dplyr)
#' library(magrittr)
#' df <- data.frame(a=c(0.1,0,0.3), b=c(0,1,0), c=c(0,0.2,0.3))
#' df %>%
#'     rowwise() %>%
#'     mutate(is_qualified = is_qualified(c_across(a:c)))
#' @importFrom magrittr %>%
is_qualified <- function(res_ind_vector) {
  .Deprecated("This function will be removed in next version.")
  if (is.data.frame(res_ind_vector)) {
    res_ind_vector <- as.numeric(res_ind_vector)
  }
  qualified <- ifelse(res_ind_vector <= 1, TRUE, FALSE) %>%
    all() %>%
    as.numeric()
  return(qualified)
}

#' Calculate treated data using raw data and MRL
#'
#' @description `calc_treated_data()` construct a new data frame which construction
#' is like raw_data that has multiple columns as labels and the remaning columns are
#' detection values of each drug on each sample.
#'
#' @param raw_data a data frame. It is raw detection data of each sample on each drug.
#' It has multiple columns as labels of each sample and the remaining columns are
#' raw detection values.
#' @param mrl_data a data frame. It has one column named `产品名称` and the other columns
#' named with name of drugs. Every row is MRL value of one product on each drugs.
#' @param st_drug a character. Name of the first column of drug.
#' @param ed_drug a character. Name of the last column of drug.
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
calc_treated_data <- function(raw_data, mrl_data, st_drug, ed_drug) {
  # get label and drug columns' index
  raw_colnames <- colnames(raw_data)
  drug_name_inds <- which(st_drug == raw_colnames):which(ed_drug == raw_colnames)
  label_name_inds <- base::setdiff(1:length(raw_colnames), drug_name_inds)

  # rename columns and convert data to data table
  raw_data <- raw_data %>%
    as.data.frame() %>%
    rename(!!!get_option("RawColnameTrans"), product = get_option("ProductLabelName")) %>%
    as.data.table()
  mrl_data <- mrl_data %>%
    as.data.frame() %>%
    rename(product = ProductLabelName) %>%
    as.data.table()
  # store treated data select columns which are labels
  treated_data_labels <- raw_data[, label_name_inds, with = FALSE]

  # generate MRL mapping list
  MRL <- mapping_mrl(as.matrix(mrl_data[,-"product"]), mrl_data[,product])
  # calculate treated matrix
  treated_matrix <- calc_residue_index(
    as.matrix(raw_data[, drug_name_inds, with = FALSE]),
    treated_data_labels[,product],
    MRL
  )

  # reset colnames (drugs' name)
  colnames(treated_matrix) <- raw_colnames[drug_name_inds]
  # add labels columns
  treated_data <- cbind(treated_data_labels, treated_matrix)

  # calculate multi-residue detection number
  treated_data[,
    multi_detection_num := apply(.SD, 1, function(x){sum(x > 0)}),
    .SDcols = drug_name_inds
  ]

  # calculate multi-residue defective number
  treated_data[,
    multi_defective_num := apply(.SD, 1, function(x){sum(x > 1)}),
    .SDcols = drug_name_inds
  ]

  # add label that indicates whether this sample is qulified
  treated_data[,
    is_qualified := apply(.SD, 1, function(x) {all(x <= 1)}),
    .SDcols = drug_name_inds
  ]

  return(treated_data)
}
