#' @include Recensor.R
NULL

#'Calculates the Estimating Equation to be solved in RPSFTM models
#' @export
#' @title Estimating Equations for \code{rpsftm()}
#' @name EstEqn
#' @inheritParams Recensor
#' @param Arm the randomisation that is independent of the recensored survival times
#' @param adjustors a forumula object of covariates to adjust for: \code{~strata(A)+B*C}
#' @param target the value to subtract off from the z-statistic 
#' @param test the survival regression function to calculate the z-statistic: survdiff, coxph, survreg
#' @param \code{...} arguments to supply to the test function.
#' @return A scalar value of the estimating equation: the z-statistics from a test minus a target value
#' @seealso Recensor
#' @author Simon Bond



EstEqn=function(phi,Time,CensorTime,Rx, data, Arm, adjustors=NULL, target=0, test=survdiff,...){
  
  # do a log-rank test
  # Could generalise this to other tests.
  #build a formula style interface
  
  require(survival)
  Sstar=Recensor( phi,Time,CensorTime,Rx)
  data=cbind(Sstar, data)
  # needs a check that Arm is two levels
  if( length(unique(Arm))!=2){stop("Arm must have exactly 2 observed values")}
  # Might want to generalise to allow other tests, and adjustors.
  #build a formula object,
  if(is.null(adjustors)){
    MyFormula=as.formula("~1")
  }else{
    MyFormula= as.formula(adjustors)
  }
  MyFormula=update(MyFormula, Sstar~.+Arm)
  
  # Might need to use quote(test) or similar
  mycall=list(test, formula, data,...)
  mycall=as.call(mycall)
  fit=eval(mycall)
  
  
  #fit=survdiff( MyFormula,data)
  
  ExtractZ(fit)-target
  
}