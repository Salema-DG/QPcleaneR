
#' @title Clean tenure variable
#'
#' @description
#' There are two variables from which tenure can be retrived. admission date (`adm_date`) and `antig`.
#' `antig` comes in years, but we want to have it in months.
#' So I create the `tenure` variable,
#' which takes preference from adm_date and goes to
#' antig when there is no adm_date reported. `tenure` is in months.
#'
#' @param data A tibble
#'
#' @return A tibble with the variable tenure.
#'


clean_tenure <- function(data) {


  #-----------------------------------#
  # Create tenure from admission date #
  #-----------------------------------#

  data %<>%
    mutate(
      # non valid adm_dates will be NA's
      adm_date =
        case_when(
          # valid dates have at least 6 digits and
          str_length(adm_date) >= 6 &
            # should start with one of the following characters
            grepl('^1|^2|^J|^F|^M|^A|^J|^S|^O|^N|^D',adm_date) ~ adm_date )) %>%
    mutate(
      tenure = case_when(
        year <= 1993 &
          adm_date %>%
          str_length() == 6 ~ #850540
          # compute the estimated number of days since admission (report month is march)
          (lubridate::my(paste0("03", year)) - lubridate::ym(adm_date)) %>%
          as.numeric(),
        year > 1993 & year <= 2009 &
          str_length(adm_date) == 6 ~
          # (report month is oct)
          (lubridate::my(paste0("10", year)) - lubridate::ym(adm_date)) %>%
          as.numeric(),
        year >= 2010 & year <= 2019 &
          str_length(adm_date) == 8 ~
          # (report month is oct)
          (lubridate::my(paste0("10", year)) -
             as.Date(paste0(adm_date, "/01") ,format='%b %Y/%d')) %>%
          as.numeric(),
        year >= 2020 &
          adm_date %>%
          str_length() == 10 ~
          # (report month is oct)
          (lubridate::my(paste0("10", year)) - lubridate::ymd(
            # convert the adm to conventional date format after 2020
            adm_date %>%
              cnvrt_date_sas())) %>%
          as.numeric(),
        TRUE ~ NA
      )) %>%
    # the diff is in days and needs one more month
    mutate(tenure = ((tenure/30) %>%
                       round())+1 )


  # complement tenure with antig
  data %<>%
    mutate(antig = 12*antig) %>%
    mutate(tenure = case_when(
      # if tenure is negative and antig >= 0, keep antig
      tenure < 0 & !is.na(antig) & antig >= 0 ~ antig,
      # otherwise, keep tenure
      tenure >= 0 ~ tenure
      # leave NA's for tenure that does not fit in with any of this criteria
    )) %>%
    mutate(
      tenure = tenure %>%
        coalesce(antig))

  return(data)

}

