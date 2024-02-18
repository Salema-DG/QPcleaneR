
#' @title Minimum Wage in Portugal
#'
#' @description
#' https://www.pordata.pt/Portugal/Sal%c3%a1rio+m%c3%adnimo+nacional-74-7895
#'
#'
#' @format A data frame with 48 rows and 2 variables:
#' \describe{
#'   \item{\code{year}}{Year of reference.}
#'   \item{\code{nominal_mw}}{Minimum Wage in nominal euros.}
#'}
#'
#' @source {
#' Pordata
#' Code used to alter it:
#'
#' # load data
#'
#' minimum_wage %<>%
#' dplyr::rename(nominal_mw = "sm",
#'               year ="anos")
#'
#'    }
#' @examples
#' data(minimum_wage) # lazy loading. The RAM will not be immediately occupied.
"minimum_wage "
