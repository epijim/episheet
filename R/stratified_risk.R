#' Stratified risk
#'
#' Calculate stratified risk estimates as per Chapter 15,
#'
#' @param data A dataframe providing the exposure, outcome and stratifying variable
#' @param exposure binary variable giving the exposure status
#' @param outcome binary variable giving the outcome status
#' @param stratifier stratifying variable
#' @param ci_level variable giving the limits for the confidence interval
#'
#' @return A dataframe giving an MH-adjusted risk ratio
#' @export
#' @examples
#' data(tolbutamide)
#' stratified_risk(tolbutamide, exposure = tolbutamide, outcome = dead,
#'     stratifier = age)

stratified_risk <- function(data, exposure, outcome, stratifier, ci_level = 95){

  . <- NULL

  # Fixing CRAN check undefined global variable warning
  aN0T <-  base_risk <-  bN1T <-  exposed <-  lci <-  m1n1n0 <- rrmh <- NULL
  Tc <- Te <- Tec <- Ts <- Tu <- Tuc <- n <- uci <- var <- NULL
  exposure <- dplyr::enquo(exposure)
  outcome <- dplyr::enquo(outcome)
  stratifier <- dplyr::enquo(stratifier)

  assertthat::assert_that(dplyr::between(ci_level, 0.1, 100),
                          msg = "ci_level must be between 0.1 and 100")

  assertthat::assert_that(all(data[,rlang::quo_name(outcome)] %in% c(0,1)) ,
                          msg = "Outcome must be either 1 or 0")

  assertthat::assert_that(all(data[, rlang::quo_name(exposure)] %in% c(0,1)),
                          msg = "Exposure must be either 1 or 0")

  alpha <- ((100 - ci_level) / 100) / 2
  z <- abs(qnorm(alpha))

  # RRmh in sheet is AH18/AI18
  # AH18 is sum of aN0/T across strata
  #     (aN0/T is L2*M4/N4 - i.e. exposed cases * total unexposed over absolute total
  # AI18 is sum of bN1/T across strata
  #     (bN1/T is M2*L4/N4
  #     m2 is unexposed cases, L4 is total exposed, N4 is absolute total
  # ie, total exposed over total)

  totals <- data %>%
    dplyr::group_by(!!stratifier) %>%
    dplyr::summarise(Ts = dplyr::n(), Tc = sum(!!outcome)) %>%
    dplyr::ungroup()

  exposed_totals <- data %>%
    dplyr::filter(!!exposure == 1) %>%
    dplyr::group_by(!!stratifier) %>%
    dplyr::summarise(Te = dplyr::n(), Tec = sum(!!outcome)) %>%
    dplyr::ungroup()


  unexposed_totals <- data %>%
    dplyr::filter(!!exposure == 0) %>%
    dplyr::group_by(!!stratifier) %>%
    dplyr::summarise(Tu = dplyr::n(), Tuc = sum(!!outcome)) %>%
    dplyr::ungroup()

  aN0 <- totals %>%
    dplyr::left_join(., exposed_totals) %>%
    dplyr::left_join(unexposed_totals) %>%
    dplyr::mutate(aN0T = Tec * Tu / Ts,
                  bN1T = Tuc * Te / Ts,
                  m1n1n0 = (Tc * Te * Tu) / Ts^2 - Tec * Tuc / Ts
    )

  # RRmh in sheet is AH18/AI18

  # Variance of RRMH is AJ18 / ah18 * ai18
  # aj is (n2*l4*m4) / n4^2 - l2 * M2 / N4
  # n2 is Tc, l4 is Te, m4 is Tu, n4 is Ts
  # l2 is Tec, m2 is Tuc, n4 is Ts


  z <- data %>%
    dplyr::group_by(!!stratifier) %>%
    dplyr::summarise(n = dplyr::n(), exposed = sum(!!exposure), outcome = sum(!!outcome)) %>%
    dplyr::left_join(., aN0) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      rrmh = sum(aN0T)/sum(bN1T),
      var = sum(m1n1n0) / (sum(aN0T) * sum(bN1T)),
      # sum_m1n1no = sum(m1n1n0),
      # sum_aN0T = sum(aN0T),
      # sum_bN1T = sum(bN1T),
      lci = exp(log(rrmh) - z * sqrt(var)),
      uci = exp(log(rrmh) + z * sqrt(var)),
    ) %>%
    dplyr::select(!!stratifier, n, exposed, outcome, rrmh, var, lci, uci)
  return(z)
}
