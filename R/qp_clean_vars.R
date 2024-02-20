#' @title Clean several variables in QP
#'
#' @description
#' This function cleans the following variables:
#'
#' + male. Because of inconsistencies within workers, apply a mode.
#' + age (see function)
#' + nationality (work in progress)
#' + wages (see function)
#' + Add Inflation (cpi). QP is collected from march to march, until 93. After 93, it has been collected in October. Thus, I must compute the CPI from monthly inflation data. INE has montly series.
#' + Add the minimum wage. Minimum wage for QP is 505 in 2014 (must be changed). QP was collected in October 2014, the change in September of that year of the MW is already binding for that year data. All other years have the MW raised on January 1st. In the first years, the MW is not binding for all (see my thesis). Younger people and people in agriculture must be accounted for.
#' + Tenure (see function)
#' + Firm size from observed size (number of workers.)
#' + Establishment size
#' + Number of establishments
#' + Promotion Variable (see function)
#' + Remove attributes
#' + remove year NAs
#' + Remove IRCTs that INE classifies as invalids
#'
#' @param data A tibble with assembled QP data
#' @param df_cpi A dataset with cpi data (montly).
#' @param df_mw A dataset with the values for yearly minimum wage
#'
#' @export
#'
#' @return A cleaned QP dataset.
#'

qp_clean_vars <- function(data,
                          df_cpi = data("INE_month_IHPC",
                                        envir = environment()),
                          df_mw = data("minimum_wage",
                                       envir = environment())) {

  #----------------------------------------------------------------------------#
  # Male ####

  # Because of inconsistencies within workers, apply a mode

  data %<>%
    dplyr::select(-male) %>%
    dplyr::left_join(data %>%
                       dplyr::group_by(worker) %>%
                       dplyr::summarise(male = male %>%
                                          QPanalyseR::modes(untie = "max")),
              by = "worker")

  # Change the variable to binary (1 if man, 0 if woman)

  data %<>%
    dplyr::mutate(male = dplyr::case_when(
      male == 1 ~ 1,
      male == 2 ~ 0
    ))

  #----------------------------------------------------------------------------#
  # Age ####

  data %<>% clean_age()

  #----------------------------------------------------------------------------#
  # Nationality ####

  # work in progress

  #----------------------------------------------------------------------------#
  # Inflation ####

  # Add the variable CPI

  # rows to keep for QP:
  df_cpi %<>%
    dplyr::filter( (year <= 1993 & month == 3 ) | # when it was collected in march
              (year > 1993 & month == 10) ) #when it was collected in october

  #add cpi to QP:
  data %<>%
    dplyr::left_join(df_cpi %>%
                dplyr::select(year, cpi),
              by = "year")

  #----------------------------------------------------------------------------#
  # Minimum wage

  # Add the column nominal_mw

  # adapt the 2014 data:
  df_mw[41, 2] <- 505

  # add the minimum wage, having age into account:
  data %<>%
    dplyr::left_join(df_mw,
              by = "year") %>%
    dplyr::mutate(nominal_mw = dplyr::case_when(
      year <= 1986 & age %in% c(18, 19) ~ nominal_mw*0.75,
      year <= 1986 & age < 18 ~ nominal_mw*0.5,
      year == 1987 & age == 17 ~ nominal_mw*0.75,
      year >= 1988 & year <= 1997 & age < 18 ~ nominal_mw*0.75,
      TRUE ~ nominal_mw
    ))

  # Also, the agricultural MW was smaller. But this is not relevant for QP,
  # this sector is usually eliminated.

  #----------------------------------------------------------------------------#
  # Wages ####

  data %<>% clean_wages()

  #----------------------------------------------------------------------------#
  # Tenure ####

  data %<>% clean_tenure()

  #----------------------------------------------------------------------------#
  # Firm size ####

  data %<>%
    dplyr::add_count(firm, year, name = "firm_size")

  #----------------------------------------------------------------------------#
  # Establishment size ####

  data %<>%
    dplyr::select(-est_size) %>%
    dplyr::add_count(estab_id, year, name = "est_size")

  #----------------------------------------------------------------------------#
  # Number of Establishments ####

  data %<>%
    dplyr::select(-n_est) %>%
    dplyr::left_join(data %>%
                dplyr::distinct(firm, estab_id, year) %>%
                dplyr::count(firm, year, name = "n_est"),
              by = c("firm", "year"))

  #----------------------------------------------------------------------------#
  # Promotions ####

  data %<>%
    clean_promotions()

  #----------------------------------------------------------------------------#
  # Remove NA in year ####

  data %<>% dplyr::filter(!is.na(year))


  #----------------------------------------------------------------------------#
  # Remove attributes ####

  data <- lapply(data, function(x) { attributes(x) <- NULL; x }) %>%
    tibble::as_tibble()

  #----------------------------------------------------------------------------#
  # IRCT ####

  # list of IRCTs that are not valid, according to INE
  # There are IRCTs that are in a "residual category" in INE. Pedro Portugal deletes them.

  irc_9000f <- c("F00215", "F00747", "F01025", "F01363", "F01571",
                 "F00280", "F00753", "F01086", "F01431", "F01955",
                 "F00354", "F00894", "F01145", "F01440", "F02029",
                 "F00499", "F00937", "F01153", "F01541", "F02180",
                 "F00523", "F01023", "F01190", "F01544", "F02184")

  data %<>% dplyr::filter(!(irct_fic %in% irc_9000f))

  #----------------------------------------------------------------------------#
  # Return ####

  return(data)

}




