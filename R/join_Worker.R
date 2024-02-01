#' @title Join Worker Level Info
#'
#' @description
#' Connect workers to their establishments and firms, year by year, maintaining the list format.
#' This assumes that there is already a list with joined firm and establishment info.
#' From 1985 to 2009, workers are joined to firms using "nuemp" and to
#' establishments using "nuest". From 2010 onwards,
#' I use "NPC_FIC" and "estab_id", respectively.
#'
#' @param data_firm_est A list with a dataset for each year; containing firm-establishment-level info.
#' @param data_worker A list with a dataset for each year; containing worker-level info.
#'
#' @return A list with joined worker, firm and establishment data.
#'

join_Worker <- function(data_firm_est,
                        data_worker) {

  data_firm_est %<>%
    purrr::map2(data_worker,
         ~{
           year_ <- .x[1] %>% unique()

           if(year_ <= 2009){

             .x %>% dplyr::left_join(.y,
                              by = c("nuemp",
                                     "nuest",
                                     "year"))

           } else if(year_ >= 2010){

             .x %>% dplyr::left_join(.y %>%
                                #this rename is to avoid repeating the columns, and to double check
                                dplyr::rename(nuemp_worker = "nuemp",
                                       emp_id_worker = "emp_id",
                                       nuest_worker = "nuest"),
                              by = c("firm",
                                     "estab_id",
                                     "year"))
           }

         })

  return(data_firm_est)
}


