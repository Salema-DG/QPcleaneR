
#' @title Prince level
#'
#' @description
#' Price level of each month since 1948.
#' Quadro extraído em 02 de Julho de 2023 (13:49:50)
#'
#'
#' @format A data frame with 905 rows and 3 variables:
#' \describe{
#'   \item{\code{cpi}}{Prince level.}
#'   \item{\code{year}}{Year of reference.}
#'   \item{\code{month}}{Month of reference.}
#'}
#'
#' @source {
#' INE.
#' Code used to alter it:
#'
#' # load data
#'
#' #set the data format to yyyy-mm-dd
#' INE_month_IHPC %<>% mutate(date = date %>% stringr::str_remove(" de"))
#'
#' INE_month_IHPC %<>% mutate(date = date %>% stringr::str_replace("Janeiro", "January"),
#'                            date = date %>% stringr::str_replace("Fevereiro", "February"),
#'                            date = date %>% stringr::str_replace("Março", "March"),
#'                            date = date %>% stringr::str_replace("Abril", "April"),
#'                            date = date %>% stringr::str_replace("Maio", "May"),
#'                            date = date %>% stringr::str_replace("Junho", "June"),
#'                            date = date %>% stringr::str_replace("Julho", "July"),
#'                            date = date %>% stringr::str_replace("Agosto", "August"),
#'                            date = date %>% stringr::str_replace("Setembro", "September"),
#'                            date = date %>% stringr::str_replace("Outubro", "October"),
#'                            date = date %>% stringr::str_replace("Novembro", "November"),
#'                            date = date %>% stringr::str_replace("Dezembro", "December"))
#'
#' INE_month_IHPC %<>%
#'   mutate(date = date %>% lubridate::my()) %>%
#'   mutate(year = date %>% lubridate::year(),
#'          month = date %>% lubridate::month())
#'
#' INE_month_IHPC %<>%
#'   rename(cpi = ipc) %>%
#'   select(cpi, year, month)
#'
#' INE_month_IHPC %<>%
#'   drop_na()
#'
#'    }
#' @examples
#' data(INE_month_IHPC) # lazy loading. The RAM will not be immediately occupied.
"INE_month_IHPC "


