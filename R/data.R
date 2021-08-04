#' China's provincial administrative boundary data in 2015
#'
#' A geo_list containing China's provincial administrative boundary data in 2015
#'
#' @format A nested list with 3 main elements:
#' \describe{
#'   \item{type}{"FeatureCollection"}
#'   \item{name}{"file48a4630a7acd"}
#'   \item{features}{nested list with 536 lists}
#'     \item{type}{"Feature"}
#'     \item{properties}{list with 9 elements}
#'     \item{geometry}{nested list with 2 elements}
#'       \item{type}{"Polygon"}
#'       \item{coordinates}{list with many numeric vertor of length 2, which is the coordinates of polygon}
#' }
#' @source \url{https://www.resdc.cn/data.aspx?DATAID=200}
"cn_province_map_json_2015_origin"

#' Modified Version of China's provincial administrative boundary data in 2015
#'
#' A geo_list containing China's provincial administrative boundary data in 2015 and has one thing
#' different with \code{\link{cn_province_map_json_2015_origin}}. The three counties between Beijing
#' and Tianjin belongs to Heibei province and is not labeled with "Heibei". They are on the same
#' polygon and marked with "Heibei" considering the integrity of administrative divisions.
#'
#' @format A nested list with 3 main elements:
#' \describe{
#'   \item{type}{"FeatureCollection"}
#'   \item{name}{"file48a4630a7acd"}
#'   \item{features}{nested list with 536 lists}
#'     \item{type}{"Feature"}
#'     \item{properties}{list with 9 elements}
#'     \item{geometry}{nested list with 2 elements}
#'       \item{type}{"Polygon"}
#'       \item{coordinates}{list with many numeric vertor of length 2, which is the coordinates of polygon}
#' }
#' @source \url{https://www.resdc.cn/data.aspx?DATAID=200}
"cn_province_map_json_2015"
