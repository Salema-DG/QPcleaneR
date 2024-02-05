
#' @title Occupation Crosswalk from 80 to 94
#'
#' @description
#' A crosswalk that turns the 3 digit occupation code of 1980 and turns it
#' into into the 3 digits code of 94.
#'
#' @format A data frame with 279 rows and 2 variables:
#' \describe{
#'   \item{\code{cnp_94}}{Occupation code of 94.}
#'   \item{\code{cpp_80}}{Occupation code of 80.}
#'}
#'
#' @source {
#' Data created in-house from Prof. Raposo occupation crosswalks.
#' Code:
#' crosswalk_80_94 <-
#'  read_dta("../1_raw_data/060_original_crosswalks/Crosswalk_CNP80-CNP941.dta")
#' crosswalk_80_94 %<>%
#'   rename(cnp_94 = "cnp94_1",
#'          cpp_80 = "profissao19803") %>%
#'   select(cnp_94,
#'          cpp_80)
#'    }
#' @examples
#' data(occup_crosswalk_80_94) # lazy loading. The RAM will not be immediately occupied.
"occup_crosswalk_80_94"


