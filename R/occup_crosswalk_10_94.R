#' @title Occupation Crosswalk from 2010 to 94
#'
#' @description
#' A crosswalk that turns the 3 digit occupation code of 2010 and turns it
#' into into the 3 digits code of 94.
#'
#' @format A data frame with 129 rows and 2 variables:
#' \describe{
#'   \item{\code{cnp_94}}{Occupation code of 94.}
#'   \item{\code{cpp_10}}{Occupation code of 2010.}
#'}
#'
#' @source {
#' Data created in-house from Prof. Raposo occupation crosswalks.
#' Code:
#' crosswalk_10_94 <-
#'   read_dta("../1_raw_data/060_original_crosswalks/Crosswalk_CNP2010-CNP19941.dta")
#' crosswalk_10_94 %<>%
#'   rename(cnp_94 = "cnp94_3",
#'          cpp_10 = "profissao20103") %>%
#'   select(cnp_94,
#'          cpp_10)
#'    }
#' @examples
#' data(occup_crosswalk_10_94) # lazy loading. The RAM will not be immediately occupied.
"occup_crosswalk_10_94"
