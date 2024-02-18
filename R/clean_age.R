
#' @title Clean the variable age
#'
#' @description
#' There are several problems with the age variable that are solved with this function.
#'
#' 1. There are some 0's being reported, along with NA's
#' 2. There are yearly inconsistencies
#' 3. Censoring at high and low ages (<= 17 e >= 68)
#'
#' Solution: Do a max mode for the years of birth and fill in the mistakes.
#' This algorithm must account for the fact that the month of reference was march in 1993 and before;
#' and October in 94 on forward.
#'
#' @param data A tibble
#'
#' @return A tibble with a cleaned variable named age
#'


clean_age <- function(data) {


  #2- discover the year of birth mode by worker
  df_yob <- data %>%
    dplyr::filter(!(age %in% c("<=17", ">=68")) & !is.na(age) & age != "0") %>%
    dplyr::mutate(age = age %>% as.numeric()) %>%
    dplyr::mutate(yob = year - age) %>%
    dplyr::group_by(worker) %>%
    dplyr::summarize(yob_mode = yob %>% QPanalyseR::modes())

  # because after 93 it's collected in October and before it's collected in March
  # I will make a notebook on whether there is more years before or after
  df_most_before <- data %>%
    dplyr::mutate(a = dplyr::case_when(
      year <= 1993 ~ 1,
      TRUE ~ 0
    )) %>%
    dplyr::group_by(worker) %>%
    dplyr::summarise(most_before = a %>% mean() %>% round())

  df_yob %<>%
    dplyr::left_join(df_most_before,
                     by = "worker")

  # In case there is most_before == 0, give tolerancia de +1 nos anos 93 ou menos
  # in most_before == 1, give tolerancia de -1 nos anos 94 ou mais
  data %<>%
    dplyr::left_join(df_yob,
                     by = "worker")

  #sinalize the individuals that will get "tolerancia"
  vec_workers_tol <- data %>%
    dplyr::filter(most_before == 0 & year <= 1993 & (as.numeric(age) + 1 == (year - yob_mode)) |
             most_before == 1 & year >= 1994 & (as.numeric(age) - 1 == (year - yob_mode))) %>%
    dplyr::distinct(worker) %>%
    dplyr::pull(worker)

  #this next lines will:
  # give the mode age for most obs
  # Give a year of tolerance for the cutoff issues
  # if tolerance was given, make it extendable to the censored years

  data %<>%
    dplyr::mutate(age_aux = case_when(
      most_before == 0 & year <= 1993 & (as.numeric(age) + 1 == (year - yob_mode)) ~ as.numeric(age),
      most_before == 1 & year >= 1994 & (as.numeric(age) - 1 == (year - yob_mode)) ~ as.numeric(age),
      age == "<=17" & most_before == 0 & worker %in% vec_workers_tol ~ (year - yob_mode + 1),
      age == ">=68" & most_before == 1 & worker %in% vec_workers_tol ~ (year - yob_mode - 1),
      TRUE ~ (year - yob_mode)
    ))

  data %<>%
    dplyr::select(-age) %>%
    dplyr::mutate(age_aux = age_aux %>% as.integer()) %>%
    dplyr::rename(age = age_aux)

  return(data)
}
