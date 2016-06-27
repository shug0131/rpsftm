#'Generic S3 methods to extract the z-statistic from a set of survival fit objects
#'
#'@title Extracting the z-statistic from a survival object
#'
#'@param x an R object
#'@param ... extendible arguments to the S3 method
#'@return a numeric value, the z statistic for the independence test of the treatment arm
#'@author Simon Bond




extract_z <- function(x, ...) {
  UseMethod("extract_z")
}

#' @describeIn extract_z Method for survdiff
#'@param fit a fitted survival object : survdiff, coxph, survreg

extract_z.survdiff <- function(fit, ...) {
  if (is.matrix(fit$obs)) {
    otmp <- apply(fit$obs, 1, sum)
    etmp <- apply(fit$exp, 1, sum)
  } else {
    otmp <- fit$obs
    etmp <- fit$exp
  }
  df <- (etmp > 0)
  if (sum(df) < 2) 
    z <- 0 else {
      temp2 <- ((otmp - etmp)[df])[-1]
      vv <- (fit$var[df, df])[-1, -1, drop = FALSE]
      # chi <- sum(solve(vv, temp2) * temp2)
      z <- temp2/sqrt(vv)
    }
  
  z
}


#' @describeIn extract_z Method for coxph objects
#'@param arm a character vector giving the name of the covariate representing the treatment arm.
#'
extract_z.coxph <- function(fit, arm, ...) {
  Z <- with(fit, coefficients/sqrt(diag(var)))
  Z[arm]
}


#' @describeIn extract_z Method for survreg objects

extract_z.survreg <- function(fit, arm, ...) {
  coef <- fit$coefficients
  # var includes the scale parameters
  var <- diag(fit$var)[1:length(coef)]
  Z <- coef/sqrt(var)
  Z[arm]
  
}