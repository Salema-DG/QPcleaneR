#' @title Join Firms to Establishments
#'
#' @description
#' Connect firms to their establishments, year by year, maintaining the list format.
#' From 1985 to 2009, firms and establishments are joined using
#' "nuemp" (and NPC_FIC remains in the data). From 2010 onwards,
#' I use NPC_FIC for the join.
#'
#' @param data_firm A list with a dataset for each year; containing firm-level info.
#' @param data_est A list with a dataset for each year; containing establishment-level info.
#'
#' @return A list with joined firm and establishment data.
#'

join_FirmEst <- function(data_firm,
                         data_est) {

  data_firm_est <- data_firm %>%
    purrr::map2(data_est,
                ~{

                  year_ <- .x[1] %>% unique()

                  if(year_ <= 2009){

                    .x %>% dplyr::left_join(.y,
                                     by = c("nuemp", "year"))

                  } else if(year_ >= 2010){

                    .x %>% dplyr::left_join(.y %>%
                                              dplyr::rename(nuemp_est = "nuemp",
                                              emp_id_est = "emp_id"),
                                     by = c("firm", "year"))
                  }

                })

  return(data_firm_est)
}


