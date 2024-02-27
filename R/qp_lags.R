
#' @title Create Firm and Worker lag/lead variables
#'
#' @description
#' The goal of creating lag and lead variables is to avoid classifying them
#' after the restrictions. That is, if the goal is to know if a worker
#' separated, we should do it before the elimination of maternity,
#' for example.
#'
#' Firm status. Create several variables:
#'
#' - lag_firm: the last year that firm appeared
#' - lead_firm: the following year that firm appeared
#' - firm_open: it's 1 if the firm never appeared before
#' - firm_close: it's 1 if the firm never appeared again
#' - firm_gap_behind: the last year the firm appeared was not last year
#' - firm_gap_forward: the next year the firm appears is not next year
#'
#' Worker status.
#'
#' - lag_worker. Last year the worker was in QP
#' - lead_worker. Next year the worker will be in QP
#' - lag_worker_firm. last firm the worker was in QP.
#' - lead_worker_firm. Next firm the worker is in QP.
#' - separation: worker is not in the current firm next year
#' - hiring: worker was not in the current firm last year
#'
#' @param data A QP assembled sample as a tibble.
#'
#' @return A tibble with lag variables.
#'
#'


qp_lags <- function(data) {

  #----------------------------------------------------------------------------#
  # Firm Status

  df_firm_level <- data %>%
    dplyr::distinct(year, firm)

  # run the fast lag function
  df_firm_level %<>%
    QPanalyseR::fast_lag(col = year,
             order = year, #the default
             group = firm,
             lag_or_lead = "lag",
             vec_n = 1,
             new_names = "lag_firm")

  # run the fast lead function
  df_firm_level %<>%
    QPanalyseR::fast_lag(col = year,
             group = firm,
             lag_or_lead = "lead",
             vec_n = 1,
             new_names = "lead_firm")

  df_firm_level %<>%
    dplyr::mutate(
      # OPEN indicator
      firm_open = dplyr::case_when(
        # case 1: open (not obs before)
        lag_firm %>%
          is.na() ~ 1,
        # case 2: already open (it was already observed before)
        !(lag_firm %>%
            is.na()) ~ 0,
        # NA in years when we cannot say
        year %in%
          c(min(year), 1991, 2002) ~ NA
      ),

      # CLOSE indicator
      firm_close = dplyr::case_when(
        # case 1: close (last observed period)
        lead_firm %>%
          is.na() ~ 1,
        # case 2: not closing
        !(lead_firm %>%
            is.na()) ~ 0,
        # NA in years we cannot say
        year %in%
          c(1989, 2000, max(year)) ~ NA
      )) %>%

    dplyr::mutate(

      # DISCONTINUITY indicator (1 means that there is discountinuity)
      firm_gap_behind = dplyr::case_when(
        (year - lag_firm) >= 2 ~ 1,
        year %in% c(min(year), 1991, 2002) ~ NA,
        TRUE ~ 0),
      firm_gap_forward = dplyr::case_when(
        (lead_firm - year) >= 2 ~ 1,
        year %in% c(min(year), 1989, 2000) ~ NA,
        TRUE ~ 0)#,
      #there os some error here
      # single_year_firm = dplyr::case_when(
      #   is.na(lag_firm) & is.na(lead_firm) >= 2 ~ 1,
      #   year %in% c(min(year), 1989, 1991, 2000, 2002, max(year)) ~ NA,
      #   TRUE ~ 0)
    )

  #add this information to df
  data %<>%
    dplyr::left_join(df_firm_level,
              by = c("firm", "year"))

  #----------------------------------------------------------------------------#
  # Worker Status

  # the last year the worker was in QP
  df %<>% QPanalyseR::fast_lag(col = year,
                   order = year, #the default
                   group = worker,
                   lag_or_lead = "lag",
                   vec_n = 1,
                   new_names = "lag_worker")

  #the next year the worker will be in QP
  df %<>% QPanalyseR::fast_lag(col = year,
                   order = year, #the default
                   group = worker,
                   lag_or_lead = "lead",
                   vec_n = 1,
                   new_names = "lead_worker")

  # the last firm the worker was in QP
  df %<>% QPanalyseR::fast_lag(col = firm,
                   order = year, #the default
                   group = worker,
                   lag_or_lead = "lag",
                   vec_n = 1,
                   new_names = "lag_worker_firm")

  # the next firm the worker will be in QP
  df %<>% QPanalyseR::fast_lag(col = firm,
                   order = year, #the default
                   group = worker,
                   lag_or_lead = "lead",
                   vec_n = 1,
                   new_names = "lead_worker_firm")

  df %<>%
    #classify fluxes as separation and hirings
    dplyr::mutate(separation = dplyr::case_when(
      (year + 1) == lead_worker & firm == lead_worker_firm ~ 0, #worker stays in the following year
      year %in%
        c(1989, 2000, max(year)) ~ NA,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(hiring = dplyr::case_when(
      (year - 1) == lag_worker & firm == lag_worker_firm ~ 0, #worker was in the firm the year before
      year %in%
        c(min(year), 1991, 2002) ~ NA,
      TRUE ~ 1,
    ))





}

