#' @title Odds Ratios and 95\% Confidence Intervals
#' @description This function calculates the odds ratios and 95\% confidence intervals (CI) from vectors of coefficients and standard errors.
#' @param coef Numeric vector of coefficient estimates
#' @param se Numeric vector of standard errors
#' @param siglevel Significance level for the confidence interval
#' @param roundto Number of decimal places to round to
#' @return A character vector of odds ratios and their 95\% confidence intervals.
#' @author Victoria Truong
#' @examples
#' OR_95CI(c(0.5,-0.3),c(0.1,0.2))
#' @export



OR_95CI <- function(coef, se, siglevel=0.05, roundto=3){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}

