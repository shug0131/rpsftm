#'Main Function used for estmating causal parameters under the Rank Preserving Structural Failure Time Model
#'
#'@export
#'@title Rank Preserving Structural Failure Time Model
#'@name rpsftm
#'@inheritParams recensor
#'@inheritParams EstEqn
#'@param data an optional data frame that contains variables
#' @param lowphi the lower limit of the range to search for the causal parameter
#' @param hiphi the upper limit of the range to search for the causal paramater
#' @param alpha the significance level used to calculate confidence intervals
#' @param treat_weight an optional parameter that phi is multiplied by on an individual observation level to give
#' differing impact to treatment. The values are transformed by abs(.)/max(abs(.)) to ensure 1 is the largest weight.
#' @param \code{...} arguments to supply to the test function.
#' @return a rpsftm method object that is a list of the following:
#' \itemize{
#' \item phi: the estimated parameter
#' \item fit: a survdiff object to produce Kaplan-Meier curves of the estimated counterfactual untreated failure times for each treatment arm
#' \item formula: a formula representing any adjustments, strata or clusters- used for the update() function
#' \item regression: the survival regression object at the estimated value of phi
#' \item Sstar: the recensored \code{Surv()} data using the estimate value of phi to give counterfactual untreated failure times.
#' \item ans: the object returned from \code{uniroot} used to solve the estimating equation
#' \item CI: a vector of the confidence interval around phi
#' \item call: the R call object
#' }
#' @author Simon Bond
#' @importFrom survival strata cluster


rpsftm=function(time, censor_time, rx, arm,data, 
                formula=~1, test=survdiff, 
                lowphi=-10,hiphi=10, alpha=0.05,
                treat_weight=1,
                Recensor=TRUE,Autoswitch=TRUE, ...){
  cl <- match.call()
  
  #create formula for fitting, and to feed into model.frame()
  #from the lm() function
  mf <- match.call(expand.dots = FALSE)
  cl$formula <-formula 
  mf$formula <- cl$formula
  #so that the use of the default value works as desired
  environment(mf$formula) <- parent.frame()
  m <- match(c("formula", "data","time", "censor_time", "rx", "arm","treat_weight"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::get_all_vars)
  #this returns a data frame with all the variables in and renamed time, censor_time, rx, arm as appropriate
  df <- eval(mf, parent.frame())
 
  #update_formula=paste("~.",substitute(arm),sep="+")
  #fit_formula=update.formula(formula, update_formula)
  fit_formula=update.formula(formula, ~.+arm)
  #Need these two lines to be able to use strata and cluster
  #fit_formula=as.character(fit_formula)[2]
  #fit_formula=reformulate(fit_formula)
  #update_formula=paste("~.",substitute(time), substitute(censor_time),substitute(rx),sep="+")
  #data_formula=update.formula(fit_formula, update_formula)
  #data_formula=update.formula(fit_formula, update_formula)
  #df2=get_all_vars(data_formula,data=data)
  
  
  #Check that the number of arms is 2.
  if( length(unique(df[,"arm"]))!=2){
    stop("arm must have exactly 2 observed values")
  }
    
  #check or handle missing data.
  
  
   test=deparse(substitute(test))
#  time=deparse(substitute(time))
#  rx=deparse(substitute(rx))
#  censor_time=deparse(substitute(censor_time))
#  arm=deparse(substitute(arm))
  
  #solve to find the value of phi that gives the root to z=0, and the limits of the CI.
  
  root=function(target){
    uniroot(EstEqn, c(lowphi,hiphi), 
            #time=time, censor_time=censor_time, rx=rx, 
            data=df, #arm=arm, 
            formula=fit_formula,target=target,
            test=test,Recensor=Recensor, Autoswitch=Autoswitch, ...=...)
  }
  ans=root(0)
  lower=root(qnorm(1-alpha/2))
  upper=root(qnorm(alpha/2) )
  #sort the ordering
  if(upper$root<lower$root){
    temp <- lower
    lower <- upper
    upper <- temp
    rm(temp)
  }
  
  
  
  
  
  
  #create a simple KM curve for each recensored arm. Used in the plot function - simple KM curves
  
  phiHat=ans$root
  if("treat_weight" %in% names(df)){
    treat_weight <- df[,"treat_weight"]
    #rescale to make sure that all weights are 1 or less for interpretability
    treat_weight <- abs(treat_weight)/max(abs(treat_weight), na.rm=TRUE)
    phiHat <- phiHat*treat_weight
  }
  
  Sstar=recensor(phiHat, df[,"time"], df[,"censor_time"],df[,"rx"],df[,"arm"],Recensor,Autoswitch)
  #environment(fit_formula) <- environment()
  #fit_formula <- update(fit_formula, Sstar~.)
  #Ignores any covariates, strata or adjustors. On Purpose as this is too general to deal with
  fit=survival::survfit(Sstar~arm, data=df)
  
  #provide the full fitted model for print and summary methods
  #regression <- EstEqn(phiHat,#time,censor_time,rx, 
  #                     df, #arm,
  #                     fit_formula, 
  #                     target=0,test=test,Recensor=Recensor, Autoswitch=Autoswitch,...)
 
  
  value=list(phi=ans$root, 
        #for the plot function
        fit=fit, 
        #for using the update() function
        formula=mf$formula,
        #for the print and summary methods
       regression=attr(ans$f.root, "fit"),
       #Not strictly needed but why not include it.
       Sstar=Sstar, 
       ans=ans, 
       CI=c(lower$root,upper$root),
       call=cl
       )
  structure(value, class="rpsftm")
}