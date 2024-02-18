#' @title Convert date from sas to R
#'
#' @description
#' This function converst the numeric sas dates into dates.
#' Sas counts the days since 1582-10-14.
#'
#' @param x A character vector.
#'
#' @return A date in a format %Y-%m-%d
#'
#'
#'
cnvrt_date_sas <-
  function(x) {
    y <- as.numeric(as.character(x))/86400

    y <-
      as.Date(y, origin = "1582-10-14") %>%
      as.character()

    return(y)
  }

