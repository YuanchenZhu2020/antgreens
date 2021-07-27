#' Translate Chinese Labels in Raw Data to English
#'
#' @description Note that \code{ProductLabelName} is renamed with "product"
#' which is hard encoded.
RawColnameTrans <- list(
  year = "\u5e74\u4efd", quarter = "\u5b63\u5ea6", province = "\u7701\u4efd",
  category = "\u4ea7\u54c1\u7c7b\u522b"
)

#' Label Name of Product in Raw and MRL Data
ProductLabelName <- "\u4ea7\u54c1\u540d\u79f0"

#' Names of "Drug"
#'
#' @description \code{DrugName} is a character vector with some names of "drug"
#' using in vegetables.
DrugNames <- list("\u836f\u7269" = "drug", "\u519c\u836f" = "pesticide")

#' Name of Threshold Column
ThresholdName <- "\u9608\u503c"

#' I18N Settings for DataTable
#'
#' @export
DataTableI18n <- list(
  chinese = list(
    processing = '\u5904\u7406\u4e2d...',
    lengthMenu = '\u663e\u793a _MENU_ \u9879\u7ed3\u679c',
    zeroRecords = '\u6ca1\u6709\u5339\u914d\u7ed3\u679c',
    info = '\u663e\u793a\u7b2c _START_ \u81f3\ _END_ \u9879\u7ed3\u679c\uff0c\u5171 _TOTAL_ \u9879',
    infoEmpty = '\u663e\u793a\u7b2c 0 \u81f3 0 \u9879\u7ed3\u679c\uff0c\u5171 0 \u9879',
    infoFiltered = '(\u7531 _MAX_ \u9879\u7ed3\u679c\u8fc7\u6ee4)',
    infoPostfix = '',
    search = '\u641c\u7d22:',
    url = '',
    emptyTable = '\u8868\u4e2d\u6570\u636e\u4e3a\u7a7a',
    loadingRecords = '\u8f7d\u5165\u4e2d...',
    infoThousands = ',',
    paginate = list(
      first = '\u9996\u9875', previous = '\u4e0a\u9875',
      `next` = '\u4e0b\u9875', last = '\u672b\u9875'
    ),
    aria = list(
      sortAscending = ": \u4ee5\u5347\u5e8f\u6392\u5217\u6b64\u5217",
      sortDescending = ": \u4ee5\u964d\u5e8f\u6392\u5217\u6b64\u5217"
    )
  )
)

#' I18N Settings for Displaying Column Names
#'
#' @export
ColnamesI18n <- list(
  chinese = list(
    year = "\u5e74\u4efd", quarter = "\u5b63\u5ea6", province = "\u7701\u4efd",
    category = "\u7c7b\u522b", product = "\u54c1\u79cd", drug = "\u836f\u7269",
    # hard-encoding
    sample_size = "\u6837\u672c\u91cf",
    detection_rate = "\u68c0\u51fa\u7387",
    qualification_rate = "\u5408\u683c\u7387",
    detection_rate_percent = "\u68c0\u51fa\u7387\uff08\u0025\uff09",
    qualification_rate_percent = "\u5408\u683c\u7387\uff08\u0025\uff09",
    multi_detection_rate = "\u591a\u6b8b\u7559\u68c0\u51fa\u7387",
    multi_defective_rate = "\u591a\u6b8b\u7559\u8d85\u6807\u7387",
    multi_detection_rate_percenet = "\u591a\u6b8b\u7559\u68c0\u51fa\u7387\uff08\u0025\uff09",
    multi_defective_rate_percenet = "\u591a\u6b8b\u7559\u8d85\u6807\u7387\uff08\u0025\uff09",
    max_detection_num = "\u6700\u5927\u591a\u6b8b\u7559\u68c0\u51fa\u6570",
    max_defective_num = "\u6700\u5927\u591a\u6b8b\u7559\u8d85\u6807\u6570"
  )
)
