
#' @title Rename and Reorder Firm Info
#'
#' @description
#' This function selects and renames the variables of firm-level data.
#' There are inconsistencies in the years. Same variables, different names.
#' But also new variables.
#'
#' @param data A list with datasets. Each element of the list is a dataset.
#'
#' @return A list with firm level data with consistent columns.
#'
firm_names_standard <- function(data) {

  data %<>%
    furrr::future_map(~{

      year_ <- .x[1] %>% unique()

      if(year_ < 1990){

        #variables to keep
        .x %<>%
          dplyr::mutate(emp_id = NA) %>%
          dplyr::select(tidyselect::starts_with("ano_") |
                   NPC_FIC |
                   tidyselect::starts_with("nuemp_") |
                   emp_id |
                   tidyselect::starts_with("n2emp_") |
                   tidyselect::starts_with("natju_") |
                   tidyselect::starts_with("nest_") |
                   tidyselect::starts_with("pemp_") |
                   tidyselect::starts_with("caem3_") |
                   tidyselect::starts_with("csoc_") |
                   tidyselect::starts_with("cspri_") |
                   tidyselect::starts_with("cspub_") |
                   tidyselect::starts_with("csest_") |
                   tidyselect::starts_with("ancon_"))

      } else if(year_ >= 1990 & year_ <= 1994){

        #variables to keep
        .x %<>%
          dplyr::mutate(emp_id = NA) %>%
          dplyr::select(tidyselect::starts_with("ano_") |
                   NPC_FIC |
                   NUEMP |
                   emp_id |
                   tidyselect::starts_with("n2emp_") |
                   tidyselect::starts_with("natju_") |
                   tidyselect::starts_with("nest_") |
                   tidyselect::starts_with("pemp_") |
                   tidyselect::starts_with("caem3_") |
                   tidyselect::starts_with("csoc_") |
                   tidyselect::starts_with("cspri_") |
                   tidyselect::starts_with("cspub_") |
                   tidyselect::starts_with("csest_") |
                   tidyselect::starts_with("ancon_"))

      } else if(year_ >= 1995 & year_ <= 2008){

        #variables to keep
        .x %<>%
          dplyr::mutate(emp_id = NA) %>%
          dplyr::select(tidyselect::starts_with("ano_") |
                   NPC_FIC |
                   NUEMP |
                   emp_id |
                   tidyselect::starts_with("n2emp_") |
                   tidyselect::starts_with("natju_") |
                   tidyselect::starts_with("nest_") |
                   tidyselect::starts_with("pemp_") |
                   tidyselect::starts_with("caem3_") |
                   tidyselect::starts_with("csoc_") |
                   tidyselect::starts_with("cspri_") |
                   tidyselect::starts_with("cspub_") |
                   tidyselect::starts_with("csest_") |
                   tidyselect::starts_with("ancon_"))
        #remove, for 2005 some years the escalao
        if(year_ == 2005){
          .x %<>%
            dplyr::select(-csoc_esc)
        }


      } else if(year_ == 2009){

        #variables to keep
        .x %<>%
          dplyr::mutate(emp_id = NA) %>%
          dplyr::select(tidyselect::starts_with("ano_") |
                   NPC_FIC |
                     tidyselect::starts_with("nuemp_") |
                   emp_id |
                   tidyselect::starts_with("n2emp_") |
                   tidyselect::starts_with("natju_") |
                   tidyselect::starts_with("nest_") |
                   tidyselect::starts_with("pemp_") |
                   tidyselect::starts_with("caem3_") |
                   tidyselect::starts_with("csoc_") |
                   tidyselect::starts_with("cspri_") |
                   tidyselect::starts_with("cspub_") |
                   tidyselect::starts_with("csest_") |
                   tidyselect::starts_with("ancon_"))

      } else if (year_ >= 2010 & year_ <= 2016){

        #variables to keep
        .x %<>%
          dplyr::select(ANO,
                 NPC_FIC,
                 NUEMP,
                 EMP_ID,
                 nut2_emp,
                 natju,
                 nest,
                 pemp,
                 CAE4_COD,
                 csoc,
                 cspri,
                 cspub,
                 csest,
                 ancon)

      } else if (year_ >= 2017){

        #variables to keep
        .x %<>%
          dplyr::select(ANO,
                 NPC_FIC,
                 NUEMP,
                 EMP_ID,
                 nut2_emp,
                 natju,
                 nest,
                 pemp,
                 CAE4_COD,
                 csoc,
                 cspri,
                 cspub,
                 csest,
                 ANO_CONSTITUICAO)
      }
      #rename those variables
      names(.x) <-
        c("year",
          "firm",
          "nuemp",
          "emp_id",
          "nut_2_firm",
          "nat_ju",
          "n_est",
          "pemp_firm",
          "cae_3",
          "cs",
          "cs_pri",
          "cs_pub",
          "cs_foreign",
          "year_funded")

      .x
    })

  return(data)
}






