#' A Function that applies a test of independence of a Recensor object from a Randomisation variabe
#' @export
#' @title Estimating Equation for RPSFTM
#' @name Test
#' @param phi the parameter that measures how more rapidly the lifetime is expended under treatment
#' @param Time the observed failure or censoring time.
#' @param CensorTime the theoretical censoring time, either observed or set after Time.
#' @param Rx the proportion of Time spent on treatment
#' @param data a data.frame object containing the variables  
#' @param Arm A factor variable with two-levels specifying a randomisation variable
#' @param adjustors A formula giving any strata or adjustors to fit in the test. Of the format ~strata(A)+B
#' @param target A scalar giving the target value to subtract from the Z-statistic. Default value of 0 for the point estimate, and other values used for confidence intervals.
#' @param test the name of a function in that will fit a survival model: survdiff, coxph, survreg.
#' @param \code{...} arguments to feed into the test function
#' @return A scalar giving the value of the estimating equation at the value of \code{phi} given, minus the target value.
#' @author Simon Bond


Test=function(phi,Time,CensorTime,Rx, data, Arm, adjustors=NULL, target=0, test=survdiff, ...){
  
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