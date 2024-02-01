#' @title All Years in One
#'
#' @description
#' This function joins all the years in a full QP. This file creates the RAW QP dataset.
#'
#' @param data A list of dataframes. Each element is a year dataset, containing info on workers, firms and establishment.
#'
#' @return The raw dataset (a tibble).
#'
bind_years <- function(data) {

  df_raw <- data %>%
    #make the data compatible
    furrr::future_map(~{

      #Remove the labels from STATA data, for all variables
      .x %<>%
        purrr::modify(haven::zap_labels)

      # Homogenize the columns' types:
      .x %>%
        # Turn to numeric
        purrr::modify_at(c("year",
                    "emp_id",
                    "age",
                    "contract_type",
                    "part_time",
                    "est_size",
                    "sector_est",
                    "occup_4d",
                    "cae_3",
                    "cae_3_est"),
                  as.character) %>%
        # Turn to character
        purrr::modify_at(c("irct_type",
                    "irct_fic",
                    "adm_date",
                    "data_last_promotion",
                    "nationality"),
                  as.character)

    }) %>%
    #bind all years into a single dataset
    purrr::reduce(dplyr::bind_rows)

  return(df_raw)

}
