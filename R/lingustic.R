#' Translate Chinese Labels in Raw Data to English
#'
#' @description Note that \code{ProductLabelName} is renamed with "product"
#' which is hard encoded.
#' @export
RawColnameTrans <- list(
  year = "\u5e74\u4efd", quarter = "\u5b63\u5ea6", province = "\u7701\u4efd",
  category = "\u4ea7\u54c1\u7c7b\u522b"
)

#' Label Name of Product in Raw and MRL Data
#' @export
ProductLabelName <- "\u4ea7\u54c1\u540d\u79f0"

#' Names of "Drug"
#'
#' @description \code{DrugName} is a character vector with some names of "drug"
#' using in vegetables.
#' @export
DrugNames <- c("\u836f\u7269", "\u519c\u836f")

#' Name of Threshold Column
#' @export
ThresholdName <- "\u9608\u503c"

#' Quarters Readable Name
#' @export
quarter_readable <- list(
  `1` = "\u7b2c\u4e00\u5b63\u5ea6",
  `2` = "\u7b2c\u4e8c\u5b63\u5ea6",
  `3` = "\u7b2c\u4e09\u5b63\u5ea6",
  `4` = "\u7b2c\u56db\u5b63\u5ea6"
)

#' Standard Names of Provinces in China
#' @export
ProvinceStandard <- c(
  "\u5b89\u5fbd\u7701", "\u5317\u4eac\u5e02", "\u798f\u5efa\u7701", "\u7518\u8083\u7701",
  "\u5e7f\u4e1c\u7701", "\u5e7f\u897f\u58ee\u65cf\u81ea\u6cbb\u533a", "\u8d35\u5dde\u7701", "\u6d77\u5357\u7701",
  "\u6cb3\u5317\u7701", "\u6cb3\u5357\u7701", "\u9ed1\u9f99\u6c5f\u7701", "\u6e56\u5317\u7701",
  "\u6e56\u5357\u7701", "\u5409\u6797\u7701", "\u6c5f\u82cf\u7701", "\u6c5f\u897f\u7701",
  "\u8fbd\u5b81\u7701", "\u5185\u8499\u53e4\u81ea\u6cbb\u533a", "\u5b81\u590f\u56de\u65cf\u81ea\u6cbb\u533a", "\u9752\u6d77\u7701",
  "\u5c71\u4e1c\u7701", "\u5c71\u897f\u7701", "\u9655\u897f\u7701", "\u4e0a\u6d77\u5e02",
  "\u56db\u5ddd\u7701", "\u5929\u6d25\u5e02", "\u897f\u85cf\u81ea\u6cbb\u533a", "\u65b0\u7586\u7ef4\u543e\u5c14\u81ea\u6cbb\u533a",
  "\u4e91\u5357\u7701", "\u6d59\u6c5f\u7701", "\u91cd\u5e86\u5e02", "\u53f0\u6e7e\u7701",
  "\u9999\u6e2f\u7279\u522b\u884c\u653f\u533a", "\u6fb3\u95e8\u7279\u522b\u884c\u653f\u533a"
)

#' Short Names of Provinces in China
#' @export
ProvinceShort <- c(
  "\u5b89\u5fbd", "\u5317\u4eac", "\u798f\u5efa", "\u7518\u8083",
  "\u5e7f\u4e1c", "\u5e7f\u897f", "\u8d35\u5dde", "\u6d77\u5357",
  "\u6cb3\u5317", "\u6cb3\u5357", "\u9ed1\u9f99\u6c5f", "\u6e56\u5317",
  "\u6e56\u5357", "\u5409\u6797", "\u6c5f\u82cf", "\u6c5f\u897f",
  "\u8fbd\u5b81", "\u5185\u8499\u53e4", "\u5b81\u590f", "\u9752\u6d77",
  "\u5c71\u4e1c", "\u5c71\u897f", "\u9655\u897f", "\u4e0a\u6d77",
  "\u56db\u5ddd", "\u5929\u6d25", "u897f\u85cf", "\u65b0\u7586",
  "\u4e91\u5357", "\u6d59\u6c5f", "\u91cd\u5e86", "\u53f0\u6e7e",
  "\u9999\u6e2f", "\u6fb3\u95e8"
)

#' PinYin of Provinces in China
#' @export
ProvincePinyin <- c(
  "anhui", "beijing", "fujian", "gansu", "guangdong", "guangxi", "guizhou", "hainan",
  "hebei", "henan", "heilongjiang", "hubei", "hunan", "jiling", "jiangsu", "jiangxi",
  "liaoning", "neimenggu", "ningxia", "qinghai", "shandong", "shan1xi", "shan3xi",
  "shanghai", "sichuan", "tianjin", "xizang", "xinjiang", "yunnan", "zhejiang",
  "chognqing", "taiwan", "xianggang", "aomen"
)

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
