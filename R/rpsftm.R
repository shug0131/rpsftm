#'Main Function used for estmating causal parameters under the Rank Preserving Structural Failure Time Model
#'
#'@export
#'@title Rank Preserving Structural Failure Time Model
#'@name rpsftm
#'@inheritParams recensor
#'@inheritParams EstEqn
#'@param formula a formula with a minimal structure of \code{ReCen(time, censor_time)~Instr(arm,rx)}.
#'Further terms can be added to the right hand side to adjust for covariates and use strata or cluster arguments.
#'@param data an optional data frame that contains variables
#' @param lowphi the lower limit of the range to search for the causal parameter
#' @param hiphi the upper limit of the range to search for the causal paramater
#' @param alpha the significance level used to calculate confidence intervals
#' @param treat_weight an optional parameter that phi is multiplied by on an individual observation level to give
#' differing impact to treatment. The values are transformed by \code{abs(.)/max(abs(.))} to ensure 1 is the largest weight.
#' @return a rpsftm method object that is a list of the following:
#' \itemize{
#' \item phi: the estimated parameter
#' \item fit: a survdiff object to produce Kaplan-Meier curves of the estimated counterfactual untreated failure times for each treatment arm
#' \item formula: a formula representing any adjustments, strata or clusters- used for the \code{update()} function
#' \item regression: the survival regression object at the estimated value of phi
#' \item Sstar: the recensored \code{Surv()} data using the estimate value of phi to give counterfactual untreated failure times.
#' \item ans: the object returned from \code{uniroot} used to solve the estimating equation
#' \item CI: a vector of the confidence interval around phi
#' \item call: the R call object
#' }
#' @details the formula object \code{ReCen(time, censor_time)~Instr(arm,rx)}, identifies particular meaning to the four
#' sets of arguments. \code{ReCen()} stands for ReCensoring. \code{Instr()} stands for Instrument. 
#' \itemize{
#' \item time: the observed failure or censoring time
#' \item censor_time: the time at which censoring would, or has occurred. This is provided for all observations
#' unlike standard Kaplan-Meier or Cox regression where it is only given for censored observations
#' \item arm: the randomised treatment arm. a factor with 2 levels, or numeric variable with values 0/1.
#' \item rx: the proportion of time on active treatment (arm=1 or the non-reference level of the factor)
#' }
#' Further adjustment terms can be added on the right hand side of the formula if desired, included \code{strata()}
#' or \code{cluster()} terms. 
#' 
#' @examples 
#' library(rpsftm)
#' ?immdef
#' fit <- rpsftm(ReCen(progyrs, censyrs)~Instr(imm,1-xoyrs/progyrs),immdef)
#' print(fit)
#' summary(fit)
#' plot(fit)
#' 
#' @author Simon Bond
#' @importFrom survival strata cluster


rpsftm=function(formula,data, 
                test=survdiff, 
                lowphi=-10,hiphi=10, alpha=0.05,
                treat_weight=1,
                Recensor=TRUE,Autoswitch=TRUE, ...){
  cl <- match.call()
  
  #create formula for fitting, and to feed into model.frame()
  #from the lm() function
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data","treat_weight"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  #this ultimately returns a data frame with all the variables in and renamed time, censor_time, rx, arm as appropriate
  special <- c("ReCen","Instr")
  mf$formula <- if (missing(data)) 
    terms(formula, special)
  else terms(formula, special, data = data)
  formula_env <- new.env(parent = environment(mf$formula))
  assign("ReCen", ReCen,envir=formula_env)
  assign("Instr", Instr, envir=formula_env)
  environment(mf$formula) <- formula_env
  df <- eval(mf, parent.frame())
  ReCen_index <- attr(mf$formula,"specials")$ReCen
  ReCen_drops=which(attr(mf$formula,"factors")[ReCen_index,]>0)
  if(length(ReCen_index)!=1){stop("Exactly one Recen() term needed")}
  if(length(ReCen_drops)>0){stop("Recen() term only on the LHS of the formula")}
  Instr_index <- attr(mf$formula,"specials")$Instr
  Instr_drops=which(attr(mf$formula,"factors")[Instr_index,]>0)
  if(length(Instr_drops)!=1){stop("Exactly one Instr() term allowed")}
  # remedies the df being a list of lists into just 1 list
  df <- cbind( df[,ReCen_index], df[,Instr_index], df[,-c(Instr_index,ReCen_index), drop=FALSE])
  fit_formula <- terms( update(mf$formula , .~arm + .))
  fit_formula <- drop.terms( fit_formula, dropx= 1+Instr_drops, keep.response=FALSE)
 
   
  #Check that the number of arms is 2.
  if( length(unique(df[,"arm"]))!=2){
    stop("arm must have exactly 2 observed values")
  }
    
  #check or handle missing data.
  
 
   test=deparse(substitute(test))
   if(is.na(match(test, c("survdiff", "coxph", "survreg")))){
     stop("Test must be one of: survdiff, coxph, survreg")
   }
   

  #solve to find the value of phi that gives the root to z=0, and the limits of the CI.
  
  root=function(target){
    uniroot(EstEqn, c(lowphi,hiphi), 
           data=df, 
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
  if("(treat_weight)" %in% names(df)){
    treat_weight <- df[,"(treat_weight)"]
    #rescale to make sure that all weights are 1 or less for interpretability
    treat_weight <- abs(treat_weight)/max(abs(treat_weight), na.rm=TRUE)
    phiHat <- phiHat*treat_weight
  }
  
  Sstar=recensor(phiHat, df[,"time"], df[,"censor_time"],df[,"rx"],df[,"arm"],Recensor,Autoswitch)
  #Ignores any covariates, strata or adjustors. On Purpose as this is too general to deal with
  fit=survival::survfit(Sstar~arm, data=df)
 
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