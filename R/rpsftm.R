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
#'@param subset expression indicating which subset of the rows of data should be used in the fit. 
#'This can be a logical vector (which is replicated to have length equal to the number of observations)
#', a numeric vector indicating which observation numbers are to be included (or excluded if negative), 
#'or a character vector of row names to be included. All observations are included by default.
#'@param na.action a missing-data filter function. This is applied to the model.frame after any subset 
#'argument has been used. Default is options()$na.action.
#' @param lowpsi the lower limit of the range to search for the causal parameter
#' @param hipsi the upper limit of the range to search for the causal paramater
#' @param alpha the significance level used to calculate confidence intervals
#' @param treat_weight an optional parameter that psi is multiplied by on an individual observation level to give
#' differing impact to treatment. The values are transformed by \code{abs(.)/max(abs(.))} to ensure 1 is the largest weight.
#' @return a rpsftm method object that is a list of the following:
#' \itemize{
#' \item psi: the estimated parameter
#' \item fit: a survdiff object to produce Kaplan-Meier curves of the estimated counterfactual untreated failure times for each treatment arm
#' \item formula: a formula representing any adjustments, strata or clusters- used for the \code{update()} function
#' \item regression: the survival regression object at the estimated value of psi
#' \item Sstar: the recensored \code{Surv()} data using the estimate value of psi to give counterfactual untreated failure times.
#' \item ans: the object returned from \code{uniroot} used to solve the estimating equation
#' \item CI: a vector of the confidence interval around psi
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


rpsftm=function(formula,data, subset, na.action, 
                test=survdiff, 
                lowpsi=-1,hipsi=1, alpha=0.05,
                treat_weight=1,
                Recensor=TRUE,Autoswitch=TRUE, ...){
  cl <- match.call()
  
  #create formula for fitting, and to feed into model.frame()
  #from the lm() function
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data","treat_weight","subset","na.action"), names(mf), 0L)
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
  na.action <- attr(df, "na.action")
  ReCen_index <- attr(mf$formula,"specials")$ReCen
  ReCen_drops=which(attr(mf$formula,"factors")[ReCen_index,]>0)
  #if(length(ReCen_index)!=1){stop("Exactly one Recen() term needed")}
  if(length(ReCen_drops)>0){stop("Recen() term only on the LHS of the formula")}
  Instr_index <- attr(mf$formula,"specials")$Instr
  Instr_drops=which(attr(mf$formula,"factors")[Instr_index,]>0)
  if(length(Instr_drops)!=1){stop("Exactly one Instr() term allowed")}
  Instr_column=attr(mf$formula,"factors")[,Instr_drops]
  if( sum(Instr_column>0)>1){stop("Instr() term must not be in any interactions")}
  
  # remedies the df being a list of lists into just 1 list
  df <- cbind( df[,ReCen_index], df[,Instr_index], df[,-c(Instr_index,ReCen_index), drop=FALSE])
  fit_formula <- terms( update(mf$formula , .~arm + .))
  fit_formula <- drop.terms( fit_formula, dropx= 1+Instr_drops, keep.response=FALSE)
 
   
  #Check that the number of arms is 2.
  if( length(unique(df[,"arm"]))!=2){
    stop("arm must have exactly 2 observed values")
  }
    

  
 
   test=deparse(substitute(test))
   if(is.na(match(test, c("survdiff", "coxph", "survreg")))){
     stop("Test must be one of: survdiff, coxph, survreg")
   }

  #Preliminary check of lowpsi and hipsi with a meaningful warning
   
  EstEqn.low <- EstEqn( lowpsi,data=df, 
                        formula=fit_formula,target=0,
                        test=test,Recensor=Recensor, Autoswitch=Autoswitch, ...=...)
  EstEqn.hi <- EstEqn( hipsi,data=df, 
                       formula=fit_formula,target=0,
                       test=test,Recensor=Recensor, Autoswitch=Autoswitch, ...=...)
  if( EstEqn.low*EstEqn.hi>0){
    message <- paste("The starting interval (",lowpsi,", ",hipsi,") to search for a solution for psi gives",
                     " values of the same sign (", signif(EstEqn.low,3),", ", signif(EstEqn.hi,3), "). Try a wider interval ?",
                     sep=""
                     )
    stop(message)
  }    
  
  #solve to find the value of psi that gives the root to z=0, and the limits of the CI.
  
  root=function(target){
    uniroot(EstEqn, c(lowpsi,hipsi), 
           data=df, 
            formula=fit_formula,target=target,
            test=test,Recensor=Recensor, Autoswitch=Autoswitch, ...=...)
  }
  ans=try(root(0),silent=TRUE)
  lower=try(root(qnorm(1-alpha/2)),silent=TRUE)
  upper=try(root(qnorm(alpha/2) ),silent=TRUE)
  
  #handle errors in root and CI finding
  
  ans.error <- class(ans)=="try-error"
  lower.error <- class(lower)=="try-error"
  upper.error <- class(upper)=="try-error"
  if( ans.error){
    warning("Evaluation of the estimated values of psi failed. It is set to 0")
    ans <- list(root=0)
    }
  if( lower.error){
    warning("Evaluation of a limit of the Confidence Interval failed.  It is set to 0")
    lower <- list(root=0)
  }
  if( upper.error){
    warning("Evaluation of a limit of the Confidence Interval failed.  It is set to 0")
    upper <- list(root=0)
  }

  
  
  
  #sort the ordering
  if(upper$root<lower$root){
    temp <- lower
    lower <- upper
    upper <- temp
    rm(temp)
  }
  
  
  
  
  
  
  #create a simple KM curve for each recensored arm. Used in the plot function - simple KM curves
  
  psiHat=ans$root
  if("(treat_weight)" %in% names(df)){
    treat_weight <- df[,"(treat_weight)"]
    #rescale to make sure that all weights are 1 or less for interpretability
    treat_weight <- abs(treat_weight)/max(abs(treat_weight), na.rm=TRUE)
    psiHat <- psiHat*treat_weight
  }
  
  Sstar=recensor(psiHat, df[,"time"], df[,"censor_time"],df[,"rx"],df[,"arm"],Recensor,Autoswitch)
  #Ignores any covariates, strata or adjustors. On Purpose as this is too general to deal with
  fit=survival::survfit(Sstar~arm, data=df)
 
  value=list(psi=ans$root, 
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

  if (length(na.action)){ value$na.action <- na.action }
  structure(value, class="rpsftm")
}