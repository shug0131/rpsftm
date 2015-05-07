#' @include Recensor.R
NULL

#'Calculates the Estimating Equation to be solved in RPSFTM models
#' 
#' @title Estimating Equations for \code{rpsftm()}
#' @name EstEqn
#' @inheritParams Recensor
#' @param arm the randomisation that is independent of the recensored survival times
#' @param adjustors a forumula object of covariates to adjust for: \code{~strata(A)+B*C}
#' @param target the value to subtract off from the z-statistic 
#' @param test the survival regression function to calculate the z-statistic: survdiff, coxph, survreg
#' @param \code{...} arguments to supply to the test function.
#' @return A scalar value of the estimating equation: the z-statistics from a test minus a target value
#' @seealso Recensor
#' @author Simon Bond



EstEqn=function(phi,time,censor_time,rx, data, arm, formula, target=0, test=survdiff,...){
  Sstar=recensor( phi,time,censor_time,rx, data=data)
  data=cbind(Sstar, data)
  
  #build a formula object,
  fit_formula=update(formula, Sstar~.)
  #allow different methods to test the independence of arm
  #constrained to be from the survival package.
  functionName=get(test, asNamespace("survival"))
  fit=  do.call(functionName, list(fit_formula,data,...) )
  ExtractZ(fit)-target
  
}