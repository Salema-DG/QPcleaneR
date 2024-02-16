
#' @title Occupation Crosswalk
#'
#' @description
#' Do a crosswalk from the occupation classification of 2010 and 1980 both to
#' the classification of 1994.
#' There are 3 occupation classifications in the pediod of the full QP:
#'
#' |Name | Years |
#' |--|--|
#' |CNP 80 | (1980-1994) |
#' |CNP 94 | (1995-2009) |
#' |CPP 2010 | (2010-today) |
#'
#'
#' @param data A tibble with an assembled QP.
#' @param year_column Not a string. Name of the year column in data.
#' @param original_occup_3d Not a string. Name of the column with occupation 3 digits column in data.
#' @param crosswalk_80_94 Crosswalk from 1980 to 1994. A dataframe.
#' @param crosswalk_10_94 Crosswalk from 2010 to 1994. A dataframe.
#'
#' @return A dataset with the "occup" column, which is crosswalked to the 1994 classification.
#'

crosswalk_occup <- function(data,
                            year_column,
                            original_occup_3d,
                            crosswalk_80_94 = data("occup_crosswalk_80_94"),
                            crosswalk_10_94 = data("occup_crosswalk_10_94")) {

  # Full crosswalk file
  occup_crosswalk <-
    dplyr::bind_rows(crosswalk_80_94 %>%
      dplyr::rename(original_occup = cpp_80) %>%
      dplyr::mutate(start = 1980,
                    end = 1994),
      crosswalk_10_94 %>%
        dplyr::rename(original_occup = cpp_10) %>%
        dplyr::mutate(start = 2010,
                      end = 2030)) %>%
    dplyr::rename(occup = cnp_94) %>%
    dplyr::mutate(original_occup = original_occup %>% as.numeric())

  # DEFUSE (return without evaluate) AND INJECT: {{  }}
  # This is also called data masking

  by <- dplyr::join_by({{ original_occup_3d }} == original_occup,
                       dplyr::between({{ year_column }}, start, end))

  # apply the crosswalk
  data %<>%
    dplyr::left_join(occup_crosswalk,
                     by = by)

  # bring the 94 classification too
  data %<>%
    dplyr::mutate(occup = dplyr::case_when(
      year >= 1995 & year <= 2009 ~ {{ original_occup_3d }},
      TRUE ~ occup
    )) %>%
    dplyr::select(!c(end, start))

  return(data)


}

