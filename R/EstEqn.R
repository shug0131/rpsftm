#'Calculates the Estimating Equation to be solved in RPSFTM models
#' 
#' @title Estimating Equations for \code{rpsftm()}
#' @name EstEqn
#' @inheritParams recensor
#' @param data the data set that contains the variables.
#' @param formula a forumula object of covariates to adjust for: \code{~strata(A)+B*C}
#' @param target the value to subtract off from the z-statistic 
#' @param test the survival regression function to calculate the z-statistic: survdiff, coxph, survreg
#' @param \code{...} arguments to supply to the test function.
#' @return A scalar value of the estimating equation: the z-statistics from a test minus a target value
#' @seealso recensor
#' @author Simon Bond



EstEqn <- function(phi,time,censor_time,rx, data, arm, formula, 
                   target=0, test="survdiff", Recensor, Autoswitch, ...){
  Sstar <- recensor( phi,data[,time],data[,censor_time],data[,rx],data[,arm], Recensor,Autoswitch)
  data <- cbind(Sstar, data)
  #build a formula object,
  fit_formula <- update(formula, Sstar~.)
  #allow different methods to test the independence of arm
  #constrained to be from the survival package.
  functionName <- get(test, asNamespace("survival"))
  fit <-   do.call(functionName, list(fit_formula,data,...) )
  # a 'cheat' to enable this to plugged into uniroot as a function that returns a number AND store the fit object.
  .value <- ExtractZ(fit, arm=arm)-target
  attr(.value,"fit") <- fit
  .value
}