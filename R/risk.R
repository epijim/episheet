#' Calculate risk ratio and risk difference
#'
#' Calculate risk ratios and risk differences
#'
#' @param data A dataframe
#' @param exposure Variable giving the levels of the outcome
#' @param outcome Variable giving cases (1) or non-cases (0)
#' @param ci_level a string giving the confidence interval
#'
#' @examples
#' # Data from stratum 1 of table 15-1., p260
#' dat <- data.frame(
#'     exposure_var = c(rep(1, 8), rep(0, 5), rep(1, 98), rep(0, 115)),
#'   outcome_var = c(rep(1, 8), rep(1, 5), rep(0, 98), rep(0, 115)),
#'   stringsAsFactors = FALSE
#' )
#' risk(data = dat, exposure = exposure_var, outcome = outcome_var)
#' @return A dataframe with the risk ratio, risk difference and confidence intervals
#' @export

risk <- function(data, exposure, outcome, ci_level = 95){

  exposure <- dplyr::enquo(exposure)
  outcome <- dplyr::enquo(outcome)

  assertthat::assert_that(dplyr::between(ci_level, 0.1, 100),
                          msg = "ci_level must be between 0.1 and 100")

  assertthat::assert_that(all(data[,rlang::quo_name(outcome)] %in% c(0,1)) ,
                          msg = "Outcome must be either 1 or 0")

  assertthat::assert_that(all(data[, rlang::quo_name(exposure)] %in% c(0,1)),
                          msg = "Exposure must be either 1 or 0")

  # Notation:
    # E: Expected nuber of cases
    # V: variance
    # A1: exposed cases, A2 Unexposed cases, B1 exposed non-cases B2 unexposed non-cases
    # N1 total exposed, N2 total unexposed, M1 total cases, M2 total non-cases N total observations

  # p 248 for formulae
  A1 <- data %>% dplyr::filter(!!exposure == 1 & !!outcome == 1) %>%
    dplyr::summarise(n = n()) %>% dplyr::pull()
  A0 <- data %>% dplyr::filter(!!exposure == 0 & !!outcome == 1) %>%
    dplyr::summarise(n = n()) %>% dplyr::pull()
  B1 <- data %>% dplyr::filter(!!exposure == 1 & !!outcome == 0) %>%
    dplyr::summarise(n = max(dplyr::row_number(!!exposure))) %>% dplyr::pull()
  B0 <- data %>% dplyr::filter(!!exposure == 0 & !!outcome == 0) %>%
    dplyr::summarise(n = max(dplyr::row_number(!!exposure))) %>% dplyr::pull()

  # E = (M1 * N1) / N
  E <- (sum(data[,rlang::quo_name(outcome)]) * sum(data[, rlang::quo_name(exposure)])) /
    length(data[,rlang::quo_name(outcome)])
  # V = (E * M0 * N0) / (N * N-1)
  M0 <- length(data[,rlang::quo_name(outcome)]) - sum(data[,rlang::quo_name(outcome)])
  N0 <- length(data[, rlang::quo_name(exposure)]) - sum(data[, rlang::quo_name(exposure)])
  N1 <- data %>% dplyr::summarise(n1 = sum(!!exposure)) %>% dplyr::pull()
  V <- (E * M0 * N0)/(length(data[,rlang::quo_name(outcome)]) *
                        (length(data[,rlang::quo_name(outcome)])-1) )

  # sd_ln_rr = ( 1/A1 - 1/N1 + 1/A0 - 1/N0) ^ (1/2)
  sd_ln_rr <- (1/A1 - 1/sum(data[, rlang::quo_name(exposure)]) + 1/A0 - 1/N0 ) ^ (1/2)
  # sd_ln_rd = ( ((A1*B1)/(N1^2 * (N1 - 1))) + ( (A0*B0)/ (N0 ^ 2 * (N0 - 1) ) ) )^(1/2)
  sd_ln_rd <-( ((A1*B1)/(N1^2 * (N1 - 1))) + ( (A0*B0)/ (N0 ^ 2 * (N0 - 1) ) ) ) ^ (1/2)
  z <- abs(qnorm((100 - ci_level)/100))


  z <- data %>%
    dplyr::group_by(!!exposure) %>%
    dplyr::summarise(n = n(), outcome = sum(!!outcome)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(risk = (outcome / n) * 100,
                  base_risk = risk[dplyr::row_number(risk) == 1],
                  risk_ratio = risk / base_risk,
                  rr_lci = exp(log(risk_ratio) - z * sd_ln_rr),
                  rr_uci = exp(log(risk_ratio) + z * sd_ln_rr),
                  risk_diff = risk - base_risk,
                  rd_lci = exp(log(risk_diff) - z * sd_ln_rd),
                  rd_uci = exp(log(risk_diff) + z * sd_ln_rd)
                  ) %>%
    dplyr::select(-base_risk)
  return(z)
}
