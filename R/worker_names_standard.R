#' @title Rename and Reorder Worker Info
#'
#' @description
#' This function selects and renames the variables of worker-level data.
#' There are inconsistencies in the years. Same variables, different names.
#' But also new variables.
#'
#' @param data A list with datasets. Each element of the list is a dataset.
#'
#' @return A list with worker level data with consistent columns.
#'
worker_names_standard <- function(data) {

  # an internal function that returns a vector of names given the year
  naming_func <- function(data_){

    year_ <- data_[1] %>% unique()

    names_85_99 <- c("year",
                     "worker",
                     "nuemp",
                     "nuest",
                     "irct_fic",
                     "male",
                     "age",
                     "occup_4d",
                     "habil",
                     "qualif_1d",
                     "qualif_2d",
                     "antig",
                     "adm_date",
                     "data_last_promotion",
                     "prof_category",
                     "prof_situation",
                     "remuneration_control",
                     "normal_hours",
                     "extra_hours",
                     "base_wage",
                     "regular_benefits",
                     "irregular_benefits",
                     "overtime_payment",
                     "tenure_payment")

    names_00_02 <- c(names_85_99,
                     "nationality",
                     "contract_type",
                     "part_time")

    names_03_06 <- c(names_00_02,
                     "irct_cae")

    names_07 <- c(names_03_06,
                  "irct_type")

    names_08_09 <- dplyr::setdiff(names_07,
                                  "tenure_payment")

    names_10_ <-
      dplyr::setdiff(names_07,
                     c("qualif_2d",
                       "tenure_payment",
                       "irct_cae")) %>%
      c("firm", #and add this ones
        "emp_id",
        "estab_id",
        "irct_base_contract_hours")

    if(year_ <= 1999){
      names(data_) <- names_85_99

    } else if(year_ >= 2000 & year_ <= 2002){
      names(data_) <- names_00_02

    } else if(year_ >= 2003 & year_ <= 2006){
      names(data_) <- names_03_06

    } else if(year_ == 2007){
      names(data_) <- names_07

    } else if(year_ >= 2008 & year_ <= 2009){
      names(data_) <- names_08_09

    } else if(year_ >= 2010){
      names(data_) <- names_10_

    }

    return(data_)

  }



  # select the order of the columns
  data %<>%
    furrr::future_map(~{

      year_ <- .x[1] %>% unique()

      if(year_ <= 2009){

        #-----------------------------
        # 1st step: variables to keep
        #-----------------------------

        .x %<>%
          dplyr::select(tidyselect::starts_with("ano_") |
                        tidyselect::starts_with("ntrab_") |
                        tidyselect::starts_with("nuemp_") |
                        tidyselect::starts_with("nuest_") |
                        tidyselect::contains("FICTICIO") |
                        tidyselect::starts_with("sexo_") |
                        tidyselect::starts_with("idade_") |
                        tidyselect::starts_with("prof_4d") |
                        tidyselect::starts_with("habil_") |
                        tidyselect::starts_with("nqua1_") |
                        tidyselect::starts_with("nqual_") |
                        tidyselect::starts_with("ant") |
                        tidyselect::starts_with("dtadm_") |
                        tidyselect::starts_with("dtulp_") |
                        tidyselect::starts_with("ctpro_") |
                        tidyselect::starts_with("stpro_") |
                        tidyselect::starts_with("ctrem_") |
                        tidyselect::starts_with("nhnor_") | # normal_hours
                        tidyselect::starts_with("nhext_") | # extra_hours
                        tidyselect::starts_with("rbase_") | # base_wage
                        tidyselect::starts_with("rprg_") | # regular_benefits
                        tidyselect::starts_with("rpirg_") | # irregular_benefits
                        tidyselect::starts_with("rextr_") | # overtime_payment
                        tidyselect::starts_with("diut_") | # tenure_payment
                        # Here some vars start existing in some earlier years:
                        tidyselect::starts_with("nacio_") |
                        tidyselect::starts_with("ctcont_") |
                        tidyselect::starts_with("crtrab_") |
                        tidyselect::starts_with("irccae_") |
                        tidyselect::starts_with("tipo_irc")
          )

      } else if (year_ >= 2010){

        #variables to keep
        .x %<>%
          dplyr::select(ANO |
                          ntrab |
                          NUEMP |
                          NUEST |
                          IRCT_FICTICIO |
                          sexo |
                          idade_Cod |
                          prof_4d |
                          habil |
                          nqual1 |
                          antig |
                          contains("adm") |
                          contains("prom") |
                          ctpro |
                          sitpro |
                          ctrem |
                          hnormais |
                          hextra |
                          rbase |
                          prest_reg |
                          prest_irreg |
                          rextra |
                          nacio |
                          tipo_contr |
                          reg_dur |
                          IRCT_LABEL |
                          #new ones in the series of 2010
                          NPC_FIC |
                          EMP_ID |
                          ESTAB_ID |
                          pnt
          )

      }

      # 2nd step: Apply the function of the names using rlang
      .x %<>% naming_func()

      #return the tibble
      .x
    })

  return(data)
}




