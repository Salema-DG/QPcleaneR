
#' @title Crosswalk of the Firm ID, NPC_FIC
#'
#' @description
#' There are several problems with the firm ID that this function deals with.
#' The `firm` identifier comes from the variable `NPC_FIC`. And there are 2 problems:
#' (1) GEP allows for `NPC_FIC` to be recycled after 2010. If a firm had closed before 2010, its NPC_FIC could be reused;
#' (2) `NPC_FIC` is not valid/existent for some observations.
#' To solve it, I select either `nuemp` or `emp_id` for this firms, which are older firm IDs.
#' A valid firm ID (`NPC_FIC`) has 9 digits and starts with a 5.
#' To solve Problem 1, I build an algorithm that:
#' 1. Get all the firms that exist from 2011 to 2020;
#' 2. Keep only the ones that did not exist in 2010;
#' 3. Keep only the ones that existed before 2010;
#' 4. Give a new ID to this firms, which is NPC_FIC * 100 (adding 2 0's).
#'
#' @param data A tibble with firm inconsistencies.
#'
#' @return A tibble with the variable firm cleaned.
#'
#'
crosswalk_firm <- function(data) {


  #-----------#
  # Problem 2 #
  #-----------#

  # A valid firm ID (`NPC_FIC`) has 9 digits and starts with a 5. There are no invalid firm IDs in 2010 and after. But there are some before.
  # This code will create a variable `firm_id_valid`, which will identify if the id is:
  #
  # 1. `correct`, for valid IDs;
  # 2. `na`, for NAs;
  # 3. `zero`, for 0's in the firm ID;
  # 4. `other`, for miscellanous problems in the ID.

  # detect valid firm ID
  data %<>% dplyr::mutate(firm_id_valid = dplyr::case_when(
    year >= 2010 ~ "correct",
    year < 2010 & firm < 600000000 & firm >= 500000000 ~ "correct",
    year < 2010 & is.na(firm) ~ "NA",
    year < 2010 & firm == 0 ~ "zero",
    TRUE ~ "other"
  ))

  # Substitute:
  # In case `NPC_FIC` is not valid, first use `nuemp` and only later use `emp_id`
  data %<>% dplyr::mutate(firm = dplyr::case_when(
    firm_id_valid != "correct" & !is.na(nuemp) ~ nuemp,
    firm_id_valid != "correct" & is.na(nuemp) & !is.na(emp_id) ~ emp_id,
    firm_id_valid != "correct" & is.na(nuemp) & is.na(emp_id) ~ NA,
    TRUE ~ firm
  )) %>%
    dplyr::drop_na(firm) # However, there are no NAs left to delete


  #-----------#
  # Problem 1 #
  #-----------#

  # The algorithm
  # 1. Get all the firms that exist from 2011 to 2020;
  # 2. Keep only the ones that did not exist in 2010;
  # 3. Keep only the ones that existed before 2010;
  # 4. Give a new ID to this firms, which is $NPC\_FIC \times 100$ (adding 2 0's).

  # Step 1
  vec_firm_aux <- data %>%
    dplyr::filter(year >= 2011) %>%
    dplyr::distinct(firm) %>%
    dplyr::pull(firm)

  # Step 2
  vec_firm_aux %<>%
    dplyr::setdiff(data %>%
                     dplyr::filter(year == 2010) %>%
                     dplyr::distinct(firm) %>%
                     dplyr::pull(firm))

  # Step 3
  vec_firm_aux %<>%
    dplyr::intersect(
      data %>%
        dplyr::filter(year <= 2009) %>%
        dplyr::distinct(firm) %>%
        dplyr::pull(firm)
    )

  # Step 4
  data %<>%
    dplyr::mutate(firm = dplyr::case_when(
      firm %in% vec_firm_aux & year >= 2010 ~ firm*100, # add two 0
      TRUE ~ firm
    ))

  # return ####
  return(data)

}

