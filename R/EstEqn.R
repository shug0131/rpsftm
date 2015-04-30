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
  
  # do a log-rank test
  # Could generalise this to other tests.
  #build a formula style interface
  Sstar=recensor( phi,time,censor_time,rx, data=data)
  data=cbind(Sstar, data)
  # needs a check that arm is two levels
  if( length(unique(arm))!=2){stop("arm must have exactly 2 observed values")}
  # Might want to generalise to allow other tests, and adjustors.
  #build a formula object,
  
  fit_formula=update(formula, Sstar~.)
  
  # Might need to use quote(test) or similar
  #need to make it survival::test()  
  mycall=list(paste("survival",substitute(test),sep="::"), fit_formula, data,...)
  mycall=as.call(mycall)
  fit=eval(mycall)
  
  
  #fit=survdiff( MyFormula,data)
  
  ExtractZ(fit)-target
  
}