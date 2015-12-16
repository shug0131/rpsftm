#'Main Function used for estmating causal parameters under the Rank Preserving Structural Failure Time Model
#'
#'@export
#'@title Rank Preserving Sturctural Failure Time Model
#'@name rpsftm
#'@inheritParams EstEqn
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
#' @importFrom survival strata cluster


rpsftm=function(time, censor_time, rx, arm,data, 
                formula=NULL, test=survdiff, 
                lowphi=-10,hiphi=10, alpha=0.05,
                Recensor=TRUE,Autoswitch=TRUE, ...){
  
  #create formula for fitting, and to feed into model.frame()
  if(is.null(formula)){
    formula=as.formula("~1")
  }
  update_formula=paste("~.",substitute(arm),sep="+")
  fit_formula=update.formula(formula, update_formula)
  #Need these two lines to be able to use strata and cluster
  fit_formula=as.character(fit_formula)[2]
  fit_formula=reformulate(fit_formula)
  update_formula=paste("~.",substitute(time), substitute(censor_time),substitute(rx),sep="+")
  data_formula=update.formula(fit_formula, update_formula)
   
 
  df=get_all_vars(data_formula,data=data)
  #Check that the number of arms is 2.
  if( length(unique(df[,deparse(substitute(arm))]))!=2){
    stop("arm must have exactly 2 observed values")
  }
    
  #check or handle missing data.
  
  
  test=deparse(substitute(test))
  time=deparse(substitute(time))
  rx=deparse(substitute(rx))
  censor_time=deparse(substitute(censor_time))
  arm=deparse(substitute(arm))
  
  #solve to find the value of phi that gives the root to z=0, and the limits of the CI.
  
  root=function(target){
    uniroot(EstEqn, c(lowphi,hiphi), 
            time=time, censor_time=censor_time, rx=rx, 
            data=df, arm=arm, formula=fit_formula,target=target,
            test=test,Recensor=Recensor, Autoswitch=Autoswitch, ...=...)
  }
  ans=root(0)
  lower=root(qnorm(1-alpha/2))
  upper=root(qnorm(alpha/2) )
  
  
  
  phiHat=ans$root
  
  #Used in the plot function - simple KM curves
  Sstar=recensor(phiHat, df[,time], df[,censor_time],df[,rx],df[,arm],Recensor,Autoswitch)
  fit=survival::survfit(update(fit_formula, Sstar~.), df)
  
  #provide the full fitted model for print and summary methods
  regression <- EstEqn(phiHat,time,censor_time,rx, df, arm, fit_formula, 
                       target=0,test=test,Recensor=Recensor, Autoswitch=Autoswitch,...)
 
  
  value=list(phi=phiHat, 
       fit=fit, 
       regression=attr(regression, "fit"),
       #Not strictly needed but why not include it.
       Sstar=Sstar, 
       ans=ans, 
       CI=c(lower$root,upper$root),
       call=match.call())
  structure(value, class="rpsftm")
}