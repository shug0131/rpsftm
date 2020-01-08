#'Generic S3 methods to extract the z-statistic from a set of survival fit objects
#'
#'@title Extracting the chi-squared statistic from a survival object
#'
#'@param x an R object
#'@param ... extendible arguments to the S3 method
#'@return a numeric value, the chi-squared statistic for the independence test of the treatment arms
#'@author Simon Bond
#'@keywords internal




extract_chisq <- function(x, ...) {
  UseMethod("extract_chisq")
}

#' @describeIn extract_z Method for survdiff
#'@param fit a fitted survival object : survdiff, coxph, survreg

extract_chisq.survdiff <- function(fit, ...) {
  if (is.matrix(fit$obs)) {
    otmp <- apply(fit$obs, 1, sum)
    etmp <- apply(fit$exp, 1, sum)
  } else {
    otmp <- fit$obs
    etmp <- fit$exp
  }
  df <- (etmp > 0)
  if (sum(df) < 2)
    z <- 0  else {
      temp2 <- ((otmp - etmp)[df])[-1]
      vv <- (fit$var[df, df])[-1, -1, drop = FALSE]
       chi <- sum(solve(vv, temp2) * temp2)
      #z <- temp2/sqrt(diag(vv))
    }
  
  chi
  #z
}


#' @describeIn extract_z Method for coxph objects
#'@param arm a character vector giving the name of the covariate representing the treatment arm.
#'
extract_chisq.coxph <- function(fit, arm, ...) {
  beta <-fit$coefficients[arm]
  labels <- names(fit$coefficients)
  index <- match(arm, labels)
  var_beta <- fit$var[index,index]
  sum( solve(var_beta, beta)*beta)
  
}


#' @describeIn extract_z Method for survreg objects

extract_chisq.survreg <- function(fit, arm, ...) {
  beta <-fit$coefficients[arm]
  var_beta <- fit$var[arm,arm]
  sum( solve(var_beta, beta)*beta)
  
}