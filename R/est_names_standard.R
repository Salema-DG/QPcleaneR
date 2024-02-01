
#' @title Rename and Reorder Establishment Info
#'
#' @description
#' This function selects and renames the variables of establishment-level data.
#' There are inconsistencies in the years. Same variables, different names.
#' But also new variables.
#'
#' @param data A list with datasets. Each element of the list is a dataset.
#'
#' @return A list with establishment level data with consistent columns.
#'
est_names_standard <- function(data) {

  data %<>%
    furrr::future_map(~{ # join all tibbles in a list of tibbles

      year_ <- .x[1] %>% unique()

      if(year_ < 2010){
        .x %<>%
          dplyr::select(tidyselect::starts_with("ano_") |
                 tidyselect::starts_with("nuemp_") |
                 tidyselect::starts_with("nuest_") |
                 tidyselect::starts_with("n2est_") |
                 tidyselect::starts_with("caes2_") |
                 tidyselect::starts_with("caest2_") | #changed in 2007
                 tidyselect::starts_with("pest_"))

        if(year_ == 2007){ #in 2007 both CAES were reported
          .x %<>%
            dplyr::select(-caes2_06)
        }

        names(.x) <- c("year",
                       "nuemp",
                       "nuest",
                       "nut_2_est",
                       "est_size",
                       "cae_3_est")
      }

      else if(year_ >= 2010){
        .x %<>%
          dplyr::select(ANO,
                 NPC_FIC,
                 NUEMP,
                 EMP_ID,
                 NUEST,
                 ESTAB_ID,
                 sede,
                 nut2_est,
                 pest,
                 any_of(c("CAE2_est", #for 2020
                          "CAE2")) #from 10 to 19
          )

        names(.x) <- c("year",
                       "firm",
                       "nuemp",
                       "emp_id",
                       "nuest",
                       "estab_id",
                       "sede",
                       "nut_2_est",
                       "est_size",
                       "cae_3_est")
      }

      #return .x
      .x
    })

  return(data)
}







