#' Calculate risk ratio and risk difference
#'
#' Calculate risk ratios and risk differences using a Poisson distribution for
#' person-time data.
#' Function works on individual level data or aggregated data
#' p244 2nd Edition
#'
#' @param data A dataframe
#' @param outcome Variable with the outcomes as a numeric variable
#' @param denominator Variable giving the amount of time at risk
#' @param exposure Variable giving whether exposed or not
#' @param per_unit Multiplier for rate values, e.g. \code{1000} for n outcomes per 1000 denominator
#' @param ci_level A numeric value giving the confidence interval
#'
#' @export
#'
#' @examples
#'
#' # Using individual level data
#'
#' data(ebola)
#' library(dplyr)
#' ebola %>%
#'  mutate(male = ifelse(sex == "male", 1, 0)) %>%
#'   rate(outcome = died, denominator = days_at_risk, exposure = male,
#'   per_unit = 100)
#'
#' # Using aggregated data
#' # Table 14-2
#' cancer_xray <- data.frame(cases = c(41, 15), pyar = c(28010, 19017),
#' radiation = c(1, 0))
#' cancer_xray
#' cancer_xray %>%
#'   rate(outcome = cases, denominator = pyar, exposure = radiation,
#'     per = 100000)

rate <- function(data, outcome, denominator, exposure, per_unit,
                 ci_level = 95){

  #Fixing undefined global variable issue from CRAN check.
  # hacky though
  base_rate <- rate_diff <- rate_ratio <- NULL

  exposure <- dplyr::enquo(exposure)
  outcome <- dplyr::enquo(outcome)
  denominator <- dplyr::enquo(denominator)

  assertthat::assert_that(dplyr::between(ci_level, 0.1, 99.9),
                          msg = "ci_level must be between 0.1 and 99.9")


    z <- abs(qnorm((100 - 95)/2/100))

    data <- data %>% dplyr::filter(!is.na(!!outcome) & !is.na(!!denominator) &
                            !is.na(!!exposure))

    A0 <- data %>% dplyr::group_by(!!exposure) %>%
      dplyr::summarise(outcome = sum(!!outcome)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!!exposure == 0) %>%
      dplyr::pull()

    A1 <- data %>% dplyr::group_by(!!exposure) %>%
      dplyr::summarise(outcome = sum(!!outcome)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!!exposure == 1) %>%
      dplyr::pull()

    sd_ln_rr <- (1/A0 + 1/A1) ^ (1/2)

    T0 <- data %>% dplyr::group_by(!!exposure) %>%
      dplyr::summarise(outcome = sum(!!denominator, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!!exposure == 0) %>%
      dplyr::pull()

    T1 <- data %>% dplyr::group_by(!!exposure) %>%
      dplyr::summarise(outcome = sum(!!denominator, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!!exposure == 1) %>%
      dplyr::pull()

    sd_ln_rd <- ((A1/(T1)^2 + A0/(T0)^2) ^ (1/2)) * per_unit

  z <- data %>%
    dplyr::group_by(!!exposure) %>%
    dplyr::summarise(outcome = sum(!!outcome),
                     denominator = sum(!!denominator)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rate = (outcome / denominator) * per_unit,
                  base_rate = rate[dplyr::row_number(rate) == 1],
                  rate_ratio = rate / base_rate,
                  rate_ratio_lci = exp(log(rate_ratio) - (z * sd_ln_rr)),
                  rate_ratio_uci = exp(log(rate_ratio) + (z * sd_ln_rr)),
                  rate_diff = rate - base_rate,
                  rate_diff_lci = rate_diff - z * sd_ln_rd,
                  rate_diff_uci = rate_diff + z * sd_ln_rd
                  ) %>%
    dplyr::select(-base_rate)

  return(z)

}
