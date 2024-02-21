
#' @title Delete Worker Duplicates
#'
#' @description
#' A worker can hold several jobs simultaneously.
#' In that case, workers will have more than one observation per year, reported by different firms.
#'
#'
#' @param data A tibble.
#' @param type A string determining the type of selection. "none" keeps the duplicates. "continuity" uses the algorithm of Salema (2022) to select. "highest" uses the highest wage and hours to select the duplicate. "all" deletes all worker duplicates.
#'
#' @return A tibble with duplicates selected. If type is set to "continuity" or "highest", there is also the column "w_g", indicating if that observation would have been eliminated using "all".
#'
#' @export
#'

delete_worker_duplicates <- function(data,
                                     type = "continuity") {

  if (type == "none") {
    return(data)
  }

  # turn data into a lazy_dt, to use dtplyr
  # lazy_dt_data <- data %>%
  #   dtplyr::lazy_dt()

  # identify the duplicates
  # df_freq <- lazy_dt_data %>%
  #   dplyr::count(worker, year) %>%
  #   dplyr::mutate(w_g= dplyr::case_when(
  #     n > 1 ~ 1,
  #     n == 1 ~ 0
  #   ))

  if (type == "continuity") {

    # get the file 081
    return(data)
  }

  if (type == "highest") {

    data <- data %>%
      dplyr::group_by(worker, year) %>%
      dplyr::slice_max(base_wage,
                n=1) %>%
      dplyr::slice_max(normal_hours,
                n=1) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(worker, year, normal_hours, base_wage, .keep_all=TRUE) %>% #nothing reaches this stage
      tibble::as_tibble()

    return(data)
  }

  if (type == "all") {
    return(data)
  }




}


