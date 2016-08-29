#' Plot the p-value function
#'
#' Plot the p-value function for one or two confidence interval pairs.
#' See following for example of the use in the literature:
#' Is flutamide effective in patients with bilateral orchiectomy?
#' Rothman, Kenneth J et al.
#' The Lancet , Volume 353 , Issue 9159 , 1184
#'
#' @section Bugs:
#' Code repo: \url{https://github.com/epijim/episheet}
#'
#' @param est1.ll Lower confidence interval of estimate 1
#' @param est1.ul Upper confidence interval of estimate 1
#' @param est2.ll Lower confidence interval of estimate 2 (optional)
#' @param est2.ul Upper confidence interval of estimate 2 (optional)
#' @param label1 If using two estimates, name the 1st
#' @param label2 If using two estimates, name the 2nd
#' @param xlabel The x axis label
#' @param citype Choose between '95\%CI', '90\%CI' or '99\%CI'
#' @keywords R Rothman pvalues episheet
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom graphics "curve"
#' @importFrom stats "qnorm"
#' @examples
#' pvalueplot(
#'   est1.ll = 0.9,
#'   est1.ul = 12,
#'   xlabel = "Relative Risk"
#' )
#'
#' pvalueplot(
#'   est1.ll = 0.8,
#'   est1.ul = 3.8,
#'   est2.ll = 1.2,
#'   est2.ul = 2,
#'   label1 = "Estimate 1",
#'   label2 = "Estimate 2",
#'   xlabel = "Relative Risk",
#'   citype = "95%CI"
#' )

pvalueplot <- function(
  est1.ll,
  est1.ul,
  est2.ll = NA,
  est2.ul = NA,
  label1 = "Estimate 1",
  label2 = "Estimate 2",
  xlabel = "Relative Risk",
  citype = "95%CI"
){
  # fake global variables to pass CMD check
  pvalue <- NULL
  updown <- NULL
  xvalue <- NULL
  zvalue <- NULL

  # which ci type?

  citype.pow <- ifelse(
    citype == "90%CI", 3.29,
    ifelse(citype == "99%CI", 5.16,
           3.84)
  )

  message(paste("Confidence interval type is ",citype))

  # calculate relative risk
    # Gardner M J and Altman D G. Statisitics with confidence.
    # BMJ publications. Reprint 1994 p 51-52
  jb_getrr <- function(ll,ul){
    exp((log(ll) + log(ul))/2)
  }

  # calculate standard errors
  jb_getse <- function(ll,ul,citype.pow){
    (log(ul) - log(ll))/citype.pow
  }

  # calculate curve value
  # up
  jb_getcurveup <- function(rr,se,pvalue){
    exp(log(rr) - pvalue*(se))
  }
  # down
  jb_getcurvedown <- function(rr,se,pvalue){
    exp(log(rr) + pvalue*(se))
  }

  # p-values of interest
  d.frame <- data.frame(
    # pvalues to plot
    pvalue = c(seq(0.01,1,0.01),seq(0.99,0.01,-0.01)),
    # formula changes if going up or down x values
    updown = c(rep("up",100),rep("down",99))
  ) %>%
    dplyr::mutate(
      zvalue = qnorm(1 - pvalue/2)
    )

  # breaks if less than 10 (in plot)
  jb_makebreaks <- function(
    xvalue
  ){
    minvalue <- min(xvalue, na.rm = T)
    maxvalue <- max(xvalue, na.rm = T)

    if (minvalue > 0.5) minvalue <- 0.49

    maxvalue <- ifelse(
      maxvalue < 3,
      3,
      ifelse(
        maxvalue < 5, 5,
        ifelse(
          maxvalue < 10, 10,
          maxvalue
        )
      )
      )

    xlimits <- c(minvalue,maxvalue)

    return(xlimits)
  }

  xbreaks <- c(0,0.5,1,1.5,2,5,10,15)




  # calculate the curves ------------------------------------------------
  ### If one estimate -------------------------------------------------
  if (is.na(est2.ll) | is.na(est2.ul)) {

    plot.data <- d.frame %>%
      dplyr::mutate(
        curve1 = ifelse(
          updown == "up",
          jb_getcurveup(
            jb_getrr(est1.ll,est1.ul),
            jb_getse(est1.ll,est1.ul,citype.pow),
            zvalue),
          jb_getcurvedown(
            jb_getrr(est1.ll,est1.ul),
            jb_getse(est1.ll,est1.ul,citype.pow),
            zvalue)
        )
      )

    # plot it
    xlimits <- jb_makebreaks(plot.data$curve1)

    plotout <- ggplot2::ggplot(plot.data,
                               ggplot2::aes_string('curve1', 'pvalue')
    ) +
      ggplot2::geom_line(size = 2, colour = "#0033cc") +
      ggplot2::theme_bw() +
      ggplot2::ylab("p-value") +
      ggplot2::xlab(xlabel) +
      ggplot2::geom_vline(
        xintercept = 1,
        colour = "red",
        linetype = 2) +
      ggplot2::geom_hline(
        yintercept = 0.05,
        colour = "red",
        linetype = 2) +
      ggplot2::scale_x_log10(
        breaks = xbreaks,
        limits = xlimits
      )
  }

  ### If two estimates ---------------------------------------------------------------
  if (!is.na(est2.ll) | !is.na(est2.ul)) {
    d.frame <- d.frame %>%
      dplyr::mutate(
        curve1 = ifelse(
          updown == "up",
          jb_getcurveup(
            jb_getrr(est1.ll,est1.ul),
            jb_getse(est1.ll,est1.ul, citype.pow),
            zvalue),
          jb_getcurvedown(
            jb_getrr(est1.ll,est1.ul),
            jb_getse(est1.ll,est1.ul, citype.pow),
            zvalue)
        ),
        curve2 = ifelse(
          updown == "up",
          jb_getcurveup(
            jb_getrr(est2.ll,est2.ul),
            jb_getse(est2.ll,est2.ul, citype.pow),
            zvalue),
          jb_getcurvedown(
            jb_getrr(est2.ll,est2.ul),
            jb_getse(est2.ll,est2.ul, citype.pow),
            zvalue)
        )
      )

    # reshape data
    plot.data <- d.frame %>%
      dplyr::select(-c(zvalue,updown)) %>%
      tidyr::gather(curve,xvalue,-pvalue)

    # plot it
    xlimits <- jb_makebreaks(plot.data$xvalue)

    plotout <- ggplot2::ggplot(plot.data,
                          ggplot2::aes_string(
                          'xvalue',
                          'pvalue',
                          colour = 'curve')
    ) +
      ggplot2::geom_line(size = 2) +
      ggplot2::theme_bw() +
      ggplot2::ylab("p-value") +
      ggplot2::xlab(xlabel) +
      ggplot2::geom_vline(
        xintercept = 1,
        colour = "red",
        linetype = 2) +
      ggplot2::geom_hline(
        yintercept = 0.05,
        colour = "red",
        linetype = 2) +
      ggplot2::scale_color_manual(
        name = "Estimate: ",
        labels = c(label1, label2),
        values = c("#0033cc", "#00cc00")
      ) +
      ggplot2::theme(legend.position = "top") +
      ggplot2::scale_x_log10(
        breaks = xbreaks,
        limits = xlimits
      )
  }
  return(plotout)
}
