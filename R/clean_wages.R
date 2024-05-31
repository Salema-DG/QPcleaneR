
#' @title Clean the wage variables
#'
#' @description
#' This fucntion does several alterations to the wage variables.
#' A reminder that base wage is paid in 14 months since 1982 (the whole QP sample)
#' https://poligrafo.sapo.pt/fact-check/direito-ao-13-o-mes-em-portugal-teve-origem-numa-greve-geral-em-julho-de-1962
#'
#' + Detects maternities and baixas
#' + Binds diuturnidades in regular_benefits
#' + A total wage that considerest the 14 months.
#' + log real hourly wage that contains all wages
#' + fte is a full time equivalence just with base wages. The goal is to make it comparable with the MW. It is a base wage normalized for the normal hours of that time. Which is 44 until 96 and 40h after.
#'
#' @param data A tibble
#'
#' @return A tibble with cleaned wage variables
#'
#' @export
#'
#'

clean_wages <- function(data) {

  # If the base wage is 0, that means that the wage is being paid by
  # Social Security.
  # It can be a baixa or pregnancy
  # later, for the restrictions
  data %<>% dplyr::mutate(maternity_baixa = dplyr::case_when(
    base_wage == 0 ~ 1,
    TRUE ~ 0))

  # Turn every wage var from NA to 0
  data %<>% dplyr::mutate(
    dplyr::across(
      c("base_wage",
        "regular_benefits",
        "irregular_benefits",
        "overtime_payment",
        "tenure_payment"),
      ~{
        dplyr::case_when(is.na(.x) ~ 0,
                         TRUE ~ .x)
      }))


  # tenure payments (antiguidades are payments are diutirnidades)
  # they are reported separately until 1999.
  # Since then, they are included into `regular_benefits`.
  # The following code binds tenure payments into `regular_benefits` for all years.
  data %<>%
    dplyr::mutate(regular_benefits = tenure_payment + regular_benefits)


  # create total wage
  data %<>%
    mutate(total_wage = base_wage*(14/12) + regular_benefits + irregular_benefits + overtime_payment)

  # turn into real values
  data %<>% dplyr::mutate(total_wage_real = total_wage/cpi*100)

  # log and hourly
  data %<>%
    dplyr::mutate(wage_lh_real = dplyr::case_when(
      (maternity_baixa == 0 & total_wage_real != 0) ~ log(total_wage_real/(normal_hours + extra_hours)),
      TRUE ~ NA
    ))

  # delete extreme values created
  data %<>%
    dplyr::filter(!(wage_lh_real < -100000000)) %>%
    dplyr::filter(!(wage_lh_real > 1000000000))

  # a full time equivalent base wage that has in account that
  # the full time hours changed in Portugal in 1997
  data %<>%
    dplyr::mutate(fte = dplyr::case_when(
      year <= 1996 ~ ((base_wage/normal_hours)*44*52/12),
      year >= 1997 ~ ((base_wage/normal_hours)*40*52/12)
    ))

  return(data)
}



