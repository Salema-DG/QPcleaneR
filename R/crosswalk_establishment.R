
#' @title Crosswalk for Establishments in QP
#'
#' @description
#'
#' There are 2 establishment IDs:
#' + estab_id, for 2010 and after. Each establishment as a unique ID;
#' + nuest, for before 2010. Each establishment as a number inside a firm. Thus, the establishment is only identifiable with the firm ID.
#'
#' Besides this, there is not an obvious way to connect them.
#' To solve it, I follow the people inside the establishments, from 2009 to 2010 and from 89 to 91 too.
#' These are the two years with discountinuities.
#'
#' There are some NA's warnings iun the loops
#'
#' @param data A tibble
#'
#' @return The end variable is `estab_id`. Where, after 2010, it's the one that comes in raw. before, it's filled
#'

crosswalk_establishment <- function(data) {


  #-------------------------------#
  # Correct a problem of the NUTS #
  #-------------------------------#

  # same establishment, in the same year, having
  # different nuts: input the most frequent nut for everyone.

  df_estab_key <- data %>%
    dplyr::filter(year <= 2009) %>%
    dplyr::distinct(year, firm, nuest, nut_2_est) %>%
    dplyr::count(year, firm, nuest) %>%
    dplyr::filter(n>1) %>%
    dplyr::select(-n) %>%
    dplyr::semi_join(data, ., by = c("year", "firm", "nuest")) %>%
    dplyr::select(year, worker, firm, nuest, nut_2_est) %>%
    dplyr::count(year, firm, nuest, nut_2_est, name = "n_nut") %>%
    dplyr::group_by(year, firm, nuest) %>%
    dplyr::slice_max(n_nut,
              n = 1,
              with_ties = F) %>%
    dplyr::ungroup() %>%
    dplyr::select(- n_nut) %>%
    dplyr::rename(nut_2_est_sub = "nut_2_est")

  data %<>%
    dplyr::left_join(df_estab_key,
              by = c("year", "firm", "nuest")) %>%
    dplyr::mutate(nut_2_est = dplyr::case_when(
      nut_2_est_sub %>% is.na() ~ nut_2_est,
      TRUE ~ nut_2_est_sub
    )) %>%
    dplyr::select(-nut_2_est_sub)


  ################################
  # Connect Establishments in QP #
  ################################

  # There are 2 discontinuities in the establishment data: 1991 and 2010

  # Including headquarters would not make sense:
  # a firm can change the headquarters

  #__________________
  # From 2009 to 2010
  #------------------

  # The sample for the crosswalk
  df_aux_est <- data %>%
    dplyr::select(year, worker, firm, estab_id, nuest, nut_2_est) %>%
    dplyr::filter(year %in% c(2009, 2010))

  # Let the dataframe have for each establishment a vector of workers
  df_aux_est %<>%
    tidyr::chop(worker) %>%
    dplyr::mutate(worker = worker %>% purrr::map(unique))

  # Create candidates establishments for 2009
  # TO ADD: Locations of the est
  df_aux_est_09 <- df_aux_est %>%
    dplyr::filter(year == 2009) %>%
    dplyr::left_join(
      df_aux_est %>%
        dplyr::filter(year == 2010) %>%
        dplyr::distinct(firm, nut_2_est, estab_id) %>%
        tidyr::chop(estab_id) %>%
        dplyr::rename(candidates = estab_id),
      by = c("firm", "nut_2_est")
    )

  #remove firms non existent in 2010
  df_aux_est_09 %<>% tidyr::drop_na(candidates)

  # if there is only one match assume it.
  # But does it make sense? It's a trade-off:
  ## One one hand, we stop making sure that the foirm didn't close and opened another.
  ## On the other, it's proably more likely that the firm fires some people and maintains the only establishment.

  df_aux_est_09 %<>%
    dplyr::rowwise() %>%
    dplyr::mutate(lc = length(candidates)) %>%
    dplyr::ungroup()

  # Save the ones with only 1 match
  df_aux_est_09_1 <- df_aux_est_09 %>% dplyr::filter(lc == 1)

  df_aux_est_09 %<>% dplyr::filter(lc != 1)

  # give the unique id to the 2009
  df_aux_est_09 %<>%
    #noamlly, purrr inside a mutate is irrelevant (mutate has an embedded loop)
    # so purr is used for non-vectorized function or complicated functions, with several datasets
    # here, because I want purr to iterate inside a vector in each df cell, I must tell it so, with rowwise()
    dplyr::rowwise() %>%
    dplyr::mutate(estab_id_2 = candidates %>% unlist() %>% as.integer() %>% c() %>% purrr::detect(~{

      vec_candidate_temp <- df_aux_est %>% dplyr::filter(year == 2010 & estab_id == .x) %>% dplyr::pull(worker) %>% unlist()

      n_common <- base::intersect((worker %>% unlist()), vec_candidate_temp) %>%
        length()

      n_common > (0.5*((worker %>% unlist()) %>% length()))

    },
    .dir = "forward",
    .default = 0) %>%
      as.numeric()) %>%
    dplyr::ungroup()
  # I have to add some if NA to surpress the warnings


  # criando o crosswalk:
  df_crosswalk_09_10 <- df_aux_est_09 %>%
    dplyr::distinct(firm, nuest, estab_id_2) %>%
    dplyr::select(firm,
           nuest,
           estab_id_2)

  # add the ones with only one match to the crosswalk:
  df_crosswalk_09_10 %<>%
    dplyr::mutate(estab_id_2 = estab_id_2 %>% unlist() %>% as.numeric()) %>% #should have been numeric bc of min integer values
    dplyr::bind_rows(
      df_aux_est_09_1 %>%
        dplyr::rename(estab_id_2 = candidates) %>%
        dplyr::distinct(firm, nuest, estab_id_2) %>%
        dplyr::mutate(estab_id_2 = estab_id_2 %>% unlist() %>% as.numeric())
    )

  df_crosswalk_09_10 %<>% dplyr::filter(estab_id_2 != 0)

  # This gives the dataframe `df_crosswalk_09_10`, which has the connections that should be extended beyond 2009

  data %<>%
    dplyr::left_join(df_crosswalk_09_10,
              by = c("firm", "nuest")) %>%
    dplyr::mutate(estab_id_2 = dplyr::case_when(
      year >= 2010 ~ NA,
      TRUE ~ estab_id_2
    )) %>%
    #if estab_id_2 is not NA, substitute it
    dplyr::mutate(estab_id = dplyr::case_when(
      is.na(estab_id_2) ~ estab_id,
      TRUE ~ estab_id_2
    )) %>%
    dplyr::select(-estab_id_2) %>%
    #keep estan_id NA from 89 backwards
    dplyr::mutate(estab_id = dplyr::case_when(
      year >= 1990 ~ estab_id
    ))

  # For the ones that found no connection, fill the establishments

  #create an estab_id for all years.
  # There are still estab_id that are NA's, because they didn't last until 2010.
  # a unique id for the other nuest
  #Create one

  old_firm_est_level <- data %>%
    dplyr::filter(is.na(estab_id) & year >= 1990) %>%
    dplyr::distinct(firm, nuest)

  #an ID that does not mix with the others
  old_firm_est_level %<>% dplyr::mutate(estab_id_2 = 10000000 + dplyr::row_number())

  data %<>%
    dplyr::left_join(old_firm_est_level,
              by = c("firm", "nuest")) %>%
    dplyr::mutate(estab_id = dplyr::case_when(
      year >= 1990 & !is.na(estab_id) ~ estab_id,
      year >= 1990 & is.na(estab_id) ~ estab_id_2
    )) %>%
    dplyr::select(-estab_id_2)

  #df$estab_id %>% is.na() %>% sum()
  #0


  #__________________
  # From 1989 to 1991
  #------------------

  #already use the new estab_id for this crosswalk
  # the estab_id is already corected, in some cases

  # The sample for the crosswalk
  df_aux_est <- data %>%
    dplyr::select(year, worker, firm, estab_id, nuest, nut_2_est) %>%
    dplyr::filter(year %in% c(1989, 1991))

  #there are no missings here
  #df_aux_est %>% map(~{.x %>% is.na() %>% sum()})

  # this is the number of establishments (it matches the chop)
  #df_aux_est %>% distinct(year, firm, nuest) %>% nrow()

  # Let the dataframe have for each establishment a vector of the workers
  df_aux_est %<>%
    tidyr::chop(worker) %>%
    dplyr::mutate(worker = worker %>% purrr::map(unique))

  # Create candidates establishments for 1989 (in the same firm and location)
  df_aux_est_89 <- df_aux_est %>%
    dplyr::filter(year == 1989) %>%
    dplyr::left_join(
      df_aux_est %>%
        dplyr::filter(year == 1991) %>%
        dplyr::distinct(firm, nut_2_est, estab_id) %>%
        tidyr::chop(estab_id) %>%
        dplyr::rename(candidates = estab_id),
      by = c("firm", "nut_2_est")
    )

  #remove obs with no candidates
  df_aux_est_89 %<>% tidyr::drop_na(candidates)

  # if there is only one match assume it.
  # But does it make sense? It's a trade-off:
  ## One one hand, we stop making sure that the foirm didn't close and opened another.
  ## On the other, it's proably more likely that the firm fires some people and maintains the only establishment.

  df_aux_est_89 %<>%
    dplyr::rowwise() %>%
    dplyr::mutate(lc = length(candidates)) %>%
    dplyr::ungroup()

  # Save the ones with only 1 match
  df_aux_est_89_1 <- df_aux_est_89 %>% dplyr::filter(lc == 1)

  df_aux_est_89 %<>% dplyr::filter(lc != 1)

  # estab_id_2 is having the nuest of 1991 that best matches it
  df_aux_est_89 %<>%
    #noamlly, purrr inside a mutate is irrelevant (mutate has an embedded loop)
    # so purr is used for non-vectorized function or complicated functions, with several datasets
    # here, because I want purr to iterate inside a vector in each df cell, I must tell it so, with rowwise()
    dplyr::rowwise() %>%
    dplyr::mutate(estab_id_2 = candidates %>% unlist() %>% as.integer() %>% c() %>% purrr::detect(~{

      vec_candidate_temp <- df_aux_est %>% dplyr::filter(year == 1991 & estab_id == .x) %>% dplyr::pull(worker) %>% unlist()

      n_common <- base::intersect((worker %>% unlist()), vec_candidate_temp) %>%
        length()

      n_common > (0.5*((worker %>% unlist()) %>% length())) #more than 50% in common

    },
    .dir = "forward",
    .default = 0) %>%
      as.numeric()) %>%
    dplyr::ungroup()
  # I have to add some if NA to suppress the warnings

  # criando o crosswalk:
  df_crosswalk_89_91 <- df_aux_est_89 %>%
    dplyr::distinct(firm, nuest, estab_id_2)

  # add the ones with only one match to the crosswalk:
  df_crosswalk_89_91 %<>%
    dplyr::mutate(estab_id_2 = estab_id_2 %>% unlist() %>% as.numeric()) %>% #should have been numeric bc of min integer values
    dplyr::bind_rows(
      df_aux_est_89_1 %>%
        dplyr::rename(estab_id_2 = candidates) %>%
        dplyr::distinct(firm, nuest, estab_id_2) %>%
        dplyr::mutate(estab_id_2 = estab_id_2 %>% unlist() %>% as.numeric())
    )


  # delete the ones that got no correspondence
  df_crosswalk_89_91 %<>% dplyr::filter(estab_id_2 != 0)


  #----------------------#
  # Apply the Crosswalks #
  #----------------------#

  # I can now apply both crosswlaks top the entire samplethe 2010 crosswalk to the entire sample.
  # First, I must change the nuest of 89 and before
  data %<>%
    dplyr::left_join(df_crosswalk_89_91,
              by = c("firm", "nuest")) %>%
    dplyr::mutate(estab_id_2 = dplyr::case_when(#turn the ones above 1990 into NA
      year >= 1990 ~ NA,
      TRUE ~ estab_id_2
    )) %>%
    #if estab_id_2 is not NA, substitute it
    dplyr::mutate(estab_id = dplyr::case_when(
      is.na(estab_id_2) ~ estab_id,
      TRUE ~ estab_id_2
    )) %>%
    dplyr::select(-estab_id_2)

  #df %>% filter(year >= 1991) %>% pull(estab_id) %>% is.na() %>% sum()
  #df %>% filter(year <= 1989) %>% nrow()
  #df %>% filter(year <= 1989) %>% pull(estab_id) %>% is.na() %>% sum()
  #df %>% filter(year <= 1989) %>% pull(estab_id) %>% is.na() %>%  `!`  %>% sum()
  #fill in the rest

  data %>% split(.$year) %>% purrr::map(~{.x %>% dplyr::pull(estab_id) %>% is.na() %>% sum()})

  old_firm_est_level <- data %>%
    dplyr::filter(is.na(estab_id)) %>%
    dplyr::distinct(firm, nuest)

  #an ID that does not mix with the others
  old_firm_est_level %<>% dplyr::mutate(estab_id_2 = 20000000 + dplyr::row_number())

  data %<>%
    dplyr::left_join(old_firm_est_level,
              by = c("firm", "nuest")) %>%
    dplyr::mutate(estab_id = dplyr::case_when(
      !is.na(estab_id) ~ estab_id,
      is.na(estab_id) ~ estab_id_2
    )) %>%
    dplyr::select(-estab_id_2)

  return(data)

}

