#'
#' @title Varaibles Consistency
#'
#' @description
#' Things that are done, include the following the changes to the variables:
#' (1) worker, delete NAs and put all NA's in 1985;
#' (2) cae_3;
#' (3) year founded;
#' (4) nut_2_firm;
#' (5) nut_2_est;
#' (6) firm;
#'
#' The function is commented with each of the problems.
#'
#' @param data A raw QP dataset (a tibble).
#'
#' @return description
#'
#' @export
#'

qp_crosswalks <- function(data) {

  # Fill all the blank spaces with NAs
  data[data==""] <- NA

  # drop all NAs in the workers identifier
  data %<>%
    dplyr::filter(!is.na(worker))

  # Because worker ID of 1985 is not compatible, turn them into NAs
  data %<>%
    dplyr::mutate(worker = dplyr::case_when(
      year == 1985 ~ NA,
      TRUE ~ worker
    ))

  #worker ID should be an integer
  data$worker %<>% as.integer()

  # Only after 2010, we have the CAE to the 4th digit.
  # Keep the first 3 digits only, for consistency.
  data %<>%
    dplyr::mutate(cae_3 = cae_3 %>% as.integer() %>% tidyr::na_if(0)) %>%
    dplyr::mutate(cae_3 = dplyr::case_when(
      cae_3 > 1000  ~ (cae_3 %/% 10),
      TRUE ~ cae_3
    ))

  # year founded has the month for some observations (in the format yyyymm).
  # keep only the year
  data %<>%
    dplyr::mutate(year_funded = year_funded %>% as.integer() %>% tidyr::na_if(0)) %>%
    dplyr::mutate(year_founded = dplyr::case_when(
      year_funded < 300000 & year_funded > 100000 ~ (year_funded%/%100),
      TRUE ~ year_funded
    )) %>%
    dplyr::select(-year_funded) # it was poorly written

  # Collapse 2 levels of the NUTS
  data %<>%
    dplyr::mutate(dplyr::across(c(nut_2_firm, nut_2_est), ~{
      .x %>% as.factor() %>% forcats::fct_collapse("90" = c("90", "99"))
    }))

  #-------------------#
  # Firm ID crosswalk #
  #-------------------#

  # keep the original ID of the firm, NPC_FIC
  data %<>%
    dplyr::mutate(NPC_FIC = firm)

  # apply the firm crosswalk
  data %<>%
    crosswalk_firm()


  # return ####
  return(data)
}











