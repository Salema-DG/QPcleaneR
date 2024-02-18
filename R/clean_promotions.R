
#' @title Clean the promotions variable
#'
#' @description
#' Add two variables.
#' The first is months_since_promotion. This tells how many months since the last promotion.
#' The second is the dummy "promoted", which tells us if the worker was promoted since last October.
#'
#' @param data A tibble
#'
#' @return A tibble with cleaned QD data.
#'

clean_promotions <- function(data) {

  #-------------------------#
  # Promotion Date reported #
  #-------------------------#
  data %<>%
    mutate(
      # non valid adm_dates will be NA's
      data_last_promotion =
        case_when(
          # valid dates have at least 6 digits and
          str_length(data_last_promotion) >= 6 &
            # should start with one of the following characters
            grepl('^1|^2|^J|^F|^M|^A|^J|^S|^O|^N|^D',data_last_promotion) ~ data_last_promotion ))


  # If there is no data_last_promotion reported, bring the last one reported
  #for that worker
  df_aux <- data %>%
    filter(!is.na(data_last_promotion)) %>%
    select(worker, year, data_last_promotion) %>%
    fast_lag(col = data_last_promotion,
             order = year, #the default
             group = worker,
             lag_or_lead = "lag",
             vec_n = 1,
             new_names = "lag_data_last_promotion")

  data %<>%
    left_join(
      df_aux %>%
        select(worker, year, lag_data_last_promotion),
      by = c("worker", "year")
    )

  data %<>%
    mutate(data_last_promotion = case_when(
      !is.na(data_last_promotion) ~ data_last_promotion,
      !is.na(data_last_promotion) ~ lag_data_last_promotion
    ))


  #---------------------------------------------------------------#
  # Create a variable that yields the months since last promotion #
  #---------------------------------------------------------------#
  data %<>%
    mutate(
      months_since_promotion = case_when(
        year >= 1999 & year <= 2009 &
          str_length(data_last_promotion) == 6 ~
          # (report month is oct)
          (lubridate::my(paste0("10", year)) - lubridate::ym(data_last_promotion)) %>%
          as.numeric(),
        year >= 2010 & year <= 2019 &
          str_length(data_last_promotion) == 8 ~
          # (report month is oct)
          (lubridate::my(paste0("10", year)) -
             as.Date(paste0(data_last_promotion, "/01") ,format='%b %Y/%d')) %>%
          as.numeric(),
        year >= 2020 &
          str_length(data_last_promotion) %in% c(10, 11) ~
          # (report month is oct)
          (lubridate::my(paste0("10", year)) - lubridate::ymd(
            # convert the adm to conventional date format after 2020
            data_last_promotion %>%
              cnvrt_date_sas())) %>%
          as.numeric(),
        TRUE ~ NA
      )) %>%
    #the diff is in days and needs one more month
    mutate(months_since_promotion = (((months_since_promotion/30) %>%
                                        round())+1) %>% as.integer() )
  #assume that the promotion was at the beguining of the month. Bc the report is at the end of october, add a month


  #thus, 1 will mean that the person was promotedin october that year. 2 will mean sep. 12 is nov last year. Thus, from 1 to 12 it's a
  #promotion this year
  data %<>%
    mutate(promoted = case_when(
      months_since_promotion %in% 1:12 ~ 1,
      TRUE ~ 0
    ))


  return(data)



}

