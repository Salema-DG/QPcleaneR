
#' @title QP, Assemble!
#'
#' @description
#' This function takes separated files of QP, with information divided by year and within
#' each year divided by workers, firms and establishments; and bind them all into one dataset.
#' To use the function, make sure all dataframes have coherent names and are in the same directory.
#'
#' @param path_worker_dir Path to the directory with worker-level data. Format of the files: QP_Trab_yyyy
#' @param path_firm_dir Path to the directory with firm-level data. Format of the files: QP_Emp_yyyy
#' @param path_est_dir Path to the directory with establishment-level data. Format of the files: QP_Est_yyyy
#'
#' @return A QP raw dataset, assembled (all years in one panel).
#'
#' @examples
#' # set the parallel processing
#'
#' @export
#'
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
qp_assemble <- function(path_worker_dir,
                        path_firm_dir,
                        path_est_fir) {

  #-------------------------------------------------------------------#
  # WARNING!                                                          #
  # All datasets must have coherent file names in the same directory. #
  #-------------------------------------------------------------------#

  #---------------------#
  # Load Worker QP data #
  #---------------------#

  df_worker_list <-
    list.files(path_worker_dir) %>% # Files to load
    furrr::future_map(~{
      haven::read_sav(paste0(path_worker_dir, "/", .x))
    })

  # name the list objects according to their name on the file.
  # Each dataset will have the title in the form `df_worker_yyyy`.
  names(df_worker_list) <-
    list.files(path_worker_dir) %>%
    #extract the last 4 numbers (the only 4 digits, for the code to work)
    stringr::str_extract("[:digit:]+") %>%
    purrr::map(~{
      paste0("df_worker_", .x)
    })

  #-------------------#
  # Load Firm QP data #
  #-------------------#

  df_firm_list <-
    list.files(path_firm_dir) %>% #the files to load
    furrr::future_map(~{
      haven::read_sav(paste0(path_firm_dir, "/", .x))
    })

  # name the list objects
  names(df_firm_list) <-
    list.files(path_firm_dir) %>%
    stringr::str_extract("[:digit:]+") %>%
    purrr::map(~{
      paste0("df_firm_", .x)
    })
  # Each dataset will have the title in the form `df_firm_yyyy`.


  #----------------------------#
  # Load Establishment QP data #
  #----------------------------#

  df_est_list <-
    list.files(path_est_dir) %>% #the files to load
    furrr::future_map(~{
      haven::read_sav(paste0(path_est_dir, "/", .x))
    })

  # name the list objects
  names(df_est_list) <-
    list.files(path_est_dir) %>%
    stringr::str_extract("[:digit:]+") %>%
    purrr::map(~{
      paste0("df_est_", .x)
    })
  # Each dataset will have the title in the form `df_est_yyyy`.


  #--------------------------#
  # Standardize the names of #
  #--------------------------#

  # Select and rename variables.
  # There are inconsistencies in the years. Same variables, different names.
  # But also new variables.

  # Worker
  df_worker_list %<>%
    worker_names_standard()

  # Firm
  df_firm_list %<>%
    firm_names_standard()

  # Establishment
  df_est_list %<>%
    est_names_standard()

  #----------------------#
  # Join QP year by year #
  #----------------------#

  # First Join: Firm to Establishment
  df_list_join <-
    join_FirmEst(df_firm_list,
                 df_est_list)

  # Second Join: Firm and Establishment to Worker
  df_list_join %<>%
    join_Worker(df_worker_list)

  # free room in the RAM memory
  rm(df_worker_list,
     df_firm_list,
     df_est_list)

  #-----------------------------#
  # Bind all the years together #
  #-----------------------------#

  df_raw <- df_list_join %>%
    bind_years()


  # Return #
  return(df_raw)

}





