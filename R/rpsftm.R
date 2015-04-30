#'Main Function used for estmating causal parameters under the Rank Preserving Sturctural Failure Time Model
#'
#'@export
#'@title Rank Preserving Sturctural Failure Time Model
#'@name rpsftm
#' @param time the observed failure or censoring time.
#' @param censor_time the theoretical censoring time, either observed or set after Time.
#' @param rx the proportion of Time spent on treatment
#' @param data a data.frame object containing the variables 
#' @param arm the randomisation that is independent of the recensored survival times
#' @param adjustors a forumula object of covariates to adjust for: \code{~strata(A)+B*C}
#' @param target the value to subtract off from the z-statistic 
#' @param test the survival regression function to calculate the z-statistic: survdiff, coxph, survreg
#' @param lowphi the lower limit of the range to search for the causal parameter
#' @param hiphi the upper limit of the range to search for the causal paramater
#' @param alpha the significance level used to calculate confidence intervals
#' @param \code{...} arguments to supply to the test function.
#' @return a list of
#' \itemize{
#' \item phi the estimated parameter
#' \item fit the fitted survival object
#' \item Sstar the recensored \code{Surv()} data using the estimate parameter
#' \item ans the object return from \code{uniroot} 
#' \item CI a vector of the confidence interval around phi
#' \item call the R call object
#' }
#' @author Simon Bond

rpsftm=function(time, censor_time, rx, arm,data, adjustors=NULL, 
                test=logrank, lowphi=-10,hiphi=10, alpha=0.05,...){
  
  #create formula for fitting, and to feed into model.frame()
  if(is.null(adjustors)){
    adjustors=as.formula("~1")
  }
  update_formula=paste("~.",substitute(arm),sep="+")
  fit_formula=update.formula(adjustors, update_formula)
  update_formula=paste("~.",substitute(time), substitute(censor_time),substitute(rx),sep="+")
  data_formula=update.formula(fit_formula, update_formula)
  df=model.frame(data_formula,data=data)
    
    
  #check or handle missing data.
  
  
  
  #argument for adjustors or formula//
  #argument for a different test other than log rank.
  
  #solve to find the value of phi that gives the root to z=0
  ans=uniroot(EstEqn, c(lowphi,hiphi), 
              time=time, censor_time=censor_time, rx=rx, arm=arm, 
              data=df, formula=fit_formula, test=test,
              target=0)
  lower=uniroot(EstEqn, c(lowphi,hiphi), 
                time=time, censor_time=censor_time, rx=rx, arm=arm, 
                data=df, formula=fit_formula,test=test,
                target=qnorm(1-alpha/2))
  upper=uniroot(EstEqn, c(lowphi,hiphi), 
                time=time, censor_time=censor_time, rx=rx, arm=arm, 
                data=df, formula=fit_formula,test=test,
                target=qnorm(alpha/2))
  
  
  
  phiHat=ans$root
  Sstar=Recensor(phiHat, time, censor_time,rx, data=df)
  #not sure about this, maybe survfit(Sstart~1)??
  
 
  #might need to use update.formula to add in the LHS
  fit=survival::survfit(Sstar,fit_formula, df)
  
  
  list(phi=phiHat, 
       fit=fit, 
       Sstar=Sstar, 
       ans=ans, 
       CI=c(lower$root,upper$root),
       call=match.call())
  
}