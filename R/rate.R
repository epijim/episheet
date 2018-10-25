#' Calculate risk ratio and risk difference
#'
#' Calculate risk ratios and risk differences
#'
#' @param data A dataframe
#' @param event Variable with the events as a numeric variable
#' @param denominator Variable giving the amount of time at risk
#' @param exposure Variable giving whether exposed or not
#' @param ci_level A numeric value giving the confidence interval
#'
#'

rate <- function(data, event, denominator, exposure, ci_level = 95){
  exposure <- dplyr::enquo(exposure)
  outcome <- dplyr::enquo(outcome)
  denominator <- dplyr::enquo(event)
  exposure <- dplyr::enquo(exposure)

  assertthat::assert_that(dplyr::between(ci_level, 0.1, 100),
                          msg = "ci_level must be between 0.1 and 100")

  assertthat::assert_that(all(data[,rlang::quo_name(outcome)] %in% c(0,1)) ,
                          msg = "Outcome must be either 1 or 0")

  assertthat::assert_that(all(data[, rlang::quo_name(exposure)] %in% c(0,1)),
                          msg = "Exposure must be either 1 or 0")
}
