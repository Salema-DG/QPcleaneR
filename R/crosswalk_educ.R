
#' @title Education Crosswalk for QP
#'
#' @description
#' This function applies a crosswalk to the education variable and also a mode.
#' The mode means that we do not allow education to vary within each worker.
#' I apply this mode for several the following reason:
#' Firstly, only for a small fraction does education vary through time.
#' Secondly, from the ones that have chages, there are a lot of decreases in education level.
#' I'm aware that the code of Marta Silva has an option to let the education level
#' increase, but never decrease.
#' However, I think that given the noise of this variable means that
#' an increases are also a reflection of errors.
#'
#' I had the help of Francisco Weinholtz for this function.
#'
#' The levels of the educ variable:
#'
#' |Level | Classification |
#' |--|--|
#' | 1 | Less than primary |
#' | 2 | Primary |
#' | 3 | Basic (6 years) |
#' | 4 | Basic (9 years) |
#' | 5 | High School |
#' | 6 | Polytechnic |
#' | 7 | College |
#' | 9 | Not defined |
#'
#' In the examples there is a code to transform the variable into these names.
#'
#' @param data A tibble
#'
#' @return A varaible "educ", consistent thorugh the years and with a mode applied.
#'
#' @examples
#' # educ_label =
#' #   dplyr::case_when(
#' #     educ == 1 ~ "Less than primary",
#' #     educ == 2 ~ "Primary",
#' #     educ == 3 ~ "Basic (6 years)",
#' #     educ == 4 ~ "Basic (9 years)",
#' #     educ == 5 ~ "High School",
#' #     educ == 6 ~ "Polytechnic",
#' #     educ == 7 ~ "College",
#' #     educ == 9 ~ "Not defined") %>%
#' #   as_factor() %>%
#' #   # to determine the order of the factors
#' #   fct_relevel("Less than primary","Primary","Basic (6 years)",
#' #               "Basic (9 years)","High School","Polytechnic",
#' #               "College", "Not defined"))
#'
crosswalk_educ <- function(data) {

  #-----------#
  # CROSSWALK #
  #-----------#

  # create the variable educ
  data %<>%
    dplyr::mutate(
      educ =
        dplyr::case_when(
          year <= 1993 ~
            dplyr::case_when(
              habil %in% c(110, 120) ~ 1,
              habil %in% c(210) ~ 2,
              habil %in% c(310) ~ 3,
              habil %in% c(410, 420) ~ 4,
              habil %in% c(510, 520) ~ 5,
              habil %in% c(699) ~ 6,
              habil %in% c(799) ~ 7,
              habil %in% c(900, 999) ~ 9),
          year >= 1994 & year <= 1999 ~
            dplyr::case_when(
              habil %in% c(110, 120) ~ 1,
              habil %in% c(210,220) ~ 2,
              habil %in% c(310, 320) ~ 3,
              habil %in% c(410, 420, 430) ~ 4,
              habil %in% c(510, 520, 530, 540) ~ 5,
              habil %in% c(600:699) ~ 6,
              habil %in% c(700:799) ~ 7,
              habil %in% c(999) ~ 9),
          year >= 2000 & year <= 2005 ~
            dplyr::case_when(
              habil %in% c(110, 120) ~ 1,
              habil %in% c(210,220) ~ 2,
              habil %in% c(310, 320) ~ 3,
              habil %in% c(410, 420, 430, 440) ~ 4,
              habil %in% c(510, 520, 530, 540) ~ 5,
              habil %in% c(600:699) ~ 6,
              habil %in% c(714:799) ~ 7,
              habil %in% c(999) ~ 9),
          year >= 2006 ~
            dplyr::case_when(
              habil %in% c(111, 112) ~ 1,
              habil %in% c(211,212) ~ 2,
              habil %in% c(221, 222) ~ 3,
              habil %in% c(231, 232, 233, 234) ~ 4,
              habil %in% c(311, 312, 313, 314,
                           315, 316, 414:499) ~ 5,
              habil %in% c(514:599) ~ 6,
              habil %in% c(614:899) ~ 7,
              habil %in% c(999) ~ 9)
        )
    )


  #------#
  # Mode #
  #------#

  # Do a max mode of educ by worker.
  # If a worker has mode of 9, prefer the 2nd highest.
  # Only keep 9 if there is no other educ observation (9 is a NA)

  isnotnine <- function(x)
  {
    if (x==9) return(FALSE) else return(TRUE)
  }

  # Create an Auxiliar data frame that contains all the education obs of each worker
  df_aux <- data %>%
    dplyr::select(worker, educ) %>%
    dplyr::mutate(
      educ = # Turn NAs into 9's
        dplyr::case_when(
          is.na(educ) ~ 9,
          TRUE ~ educ)) %>%
    tidyr::chop(educ) # every worker gets a vector with all the educ obs.

  # Apply the modes
  educ_vector <- df_aux$educ %>% #gets as a list
    furrr::future_map(~{
      .x %>%
        base::Filter(isnotnine, .) %>% # delete all 9s
        QPanalyseR::modes(untie = "max")
    }) %>%
    unlist() %>%  #unlist is baster than reduce(c)
    dplyr::coalesce(9) #turn NA into 9

  # Diagnostics
  stopifnot(length(educ_vector) == nrow(df_aux),
            length(educ_vector) == dplyr::n_distinct(data$worker))

  # Join
  data %<>%
    dplyr::select(-educ) %>%
    dplyr::left_join(
      df_aux %>%
        dplyr::select(-educ) %>%
        dplyr::mutate(educ = educ_vector),
      by = "worker"
    )

  return(data)

}









