#'Main Function used for estimating causal parameters under the Iterative Parameter Estimate method
#'
#'@export
#'@title Iterative Parameter Estimate method
#'@name ipe
#'@param formula a formula with a minimal structure of \code{Surv(time, status)~rand(arm,rx)}.
#'Further terms can be added to the right hand side to adjust for covariates and use strata or 
#'cluster arguments.
#'@inheritParams survival::survreg
#'@param ... other arguments that will be passed to \code{survreg}
#'@param data an optional data frame that contains variables
#'@param censor_time variable or constant giving the time at which censoring would, or has occurred. 
#'This should be provided for all observations unlike standard Kaplan-Meier or Cox regression where 
#'it is only given for censored observations. If no value is given then re-censoring is not applied.
#'@param subset expression indicating which subset of the rows of data should be used in the fit. 
#'This can be a logical vector (which is replicated to have length equal to the number of observations),
#' a numeric vector indicating which observation numbers are to be included (or excluded if negative), 
#'or a character vector of row names to be included. All observations are included by default.
#'@param na.action a missing-data filter function. This is applied to the model.frame after any subset 
#'argument has been used. Default is options()$na.action.
#' @param alpha the significance level used to calculate confidence intervals
#' @param treat_modifier an optional variable that psi is multiplied by on an individual observation level to give
#' differing impact to treatment.
#' @param epsilon the threshold to define convergence of the parameter estimate psi. 
#' @param iter_max the maximum number of iterations allowed before termination.
#' @param verbose  logical to print a history of parameter estimates, or not. 
#' @return an ipe method object that is a list of the following:
#' \itemize{
#' \item psi: the estimated parameter
#' \item fit: a survdiff object to produce Kaplan-Meier curves of the estimated counterfactual failure times for each treatment arm assuming perfect compliance
#' \item Sstar: the recensored \code{Surv()} data using the estimate value of psi to give counterfactual failure times with perfect compliance.
#' \item rand: the rand() object used to specify the allocated and observed amount of treatment.
#' \item iter: the number of iterations taken to achieve convergence
#' \item Further elements corresponding to either a \code{survreg} object. This will always include:
#'   \itemize{
#'   \item call: the R call object
#'   \item formula: a formula representing any adjustments, strata or clusters- used for the \code{update()} function
#'   \item terms: a more detailed representation of the model formula
#'   }
#' }
#' @seealso \code{\link[survival]{survdiff}}, \code{\link[survival]{coxph.object}}, \code{\link[survival]{survreg.object}}
#'  
#' @details the formula object \code{Surv(time, status)~rand(arm,rx)}. \code{rand()} stands 
#' for randomisation, both the randomly assigned and actual observed treatment. 
#' \itemize{
#'   \item arm: the randomised treatment arm. a factor with 2 levels, or numeric variable with values 0/1.
#'  \item rx: the proportion of time on active treatment (arm=1 or the non-reference level of the factor)
#' }
#' Further adjustment terms can be added on the right hand side of the formula if desired, included \code{strata()}
#' or \code{cluster()} terms. 
#' 
#' @examples 
#' ?immdef
#' fit <- ipe(Surv(progyrs, prog)~rand(imm,1-xoyrs/progyrs),immdef, censyrs)
#' print(fit)
#' summary(fit)
#' plot(fit)
#' 
#' @author Simon Bond
#' @importFrom survival Surv strata cluster survdiff
#' @importFrom stats terms model.extract update drop.terms reformulate uniroot qnorm

ipe <- function(formula, data, censor_time, subset, na.action,  
                dist="weibull",
                treat_modifier = 1, autoswitch = TRUE, 
                epsilon=1e-5, iter_max=100, alpha=0.05,
                verbose=options()$verbose,
                ...) {
  cl <- match.call()
  
  # create formula for fitting, and to feed into model.frame() from the
  # lm() function
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "censor_time", "treat_modifier", "subset", "na.action"), 
             names(mf), 0L)
 
  mf <- mf[c(1L, m)]
 
  mf$drop.unused.levels <- TRUE
  
  # this ultimately returns a data frame, df, with all the variables in and
  # renamed time, status, censor_time, rx, arm as appropriate
  special <- c("rand","strata","cluster")
  mf$formula <- if (missing(data)) {
    terms(formula, special)
  } else {
    terms(formula, special, data = data)
  }
  
  
  formula_env <- new.env(parent = environment(mf$formula))
  assign("rand", rand, envir = formula_env)
  assign("Surv", Surv, envir = formula_env)
  assign("cluster", cluster, envir = formula_env)
  assign("strata", strata, envir = formula_env)
  environment(mf$formula) <- formula_env
  
  return_formula <- mf$formula
  
  mf[[1L]] <- as.name("model.frame")
  df <- eval(mf, parent.frame())

  na.action <- attr(df, "na.action")
 #adapted from coxph()   
  Y <- model.extract(df, "response")
  if (!inherits(Y, "Surv")) 
    stop("Response must be a survival object")
  type <- attr(Y, "type")
  if (type != "right" ) 
    stop(paste("rpsftm doesn't support \"", type, "\" survival data", 
               sep = ""))
  response_index <-  attr(mf$formula, "response")
  
  if(length(formula)>2){
    ytemp <- termsinner(formula[[2]])
    xtemp <- termsinner(formula[[3]])
    if (any(!is.na(match(xtemp, ytemp)))) 
      stop("a variable appears on both the left and right sides of the formula")
  }
  
  
  
  rand_index <- attr(mf$formula, "specials")$rand
  rand_drops <- which(attr(mf$formula, "factors")[rand_index, ] > 0)
  if (length(rand_drops) != 1) {
    stop("Exactly one rand() term allowed")
  }
  rand_column <- attr(mf$formula, "factors")[, rand_drops]
  if (sum(rand_column > 0) > 1) {
    stop("rand() term must not be in any interactions")
  }
  
  # check for special terms
  if( length(labels(mf$formula))>1){
    adjustor_formula <- drop.terms( mf$formula, dropx = rand_drops , keep.response = FALSE)
    adjustor_names <- unlist( lapply( attr( terms( adjustor_formula), "variables"), termsinner)[-1])
    if( any( adjustor_names %in% c(".arm",".rx","time","status"))){
      warning( "'.arm', '.rx', 'time', 'status' are used internally within rpsftm. Please rename these variables used in the formula outside of rand() and surv()")
    }
  }
  # add in the special covariate .arm
  
  fit_formula <- terms(update(mf$formula, . ~ .arm + .))
  fit_formula <- drop.terms(fit_formula, dropx = 1 + rand_drops, keep.response = FALSE)
  
  rand_object <- df[,rand_index]
  
  df_basic <- data.frame( time=df[,response_index][,"time"], 
                          status=df[,response_index][,"status"], 
                          ".arm"=df[, rand_index][,"arm"], 
                          ".rx"=df[,rand_index][,"rx"])
  
  
  df_adjustor <- df[,-c( response_index, rand_index), drop = FALSE]
  if( length( attr( fit_formula, "variables")) > 2){
    #this pulls out the core variables, rather than strata(var), say
    adjustor_formula <- drop.terms( mf$formula, dropx = rand_drops , keep.response = FALSE)
    adjustor_names <- unlist( lapply( attr( terms( adjustor_formula), "variables"), termsinner)[-1])
    mf$formula <- reformulate(adjustor_names)
    mf$formula <- if (missing(data)) {
      terms(mf$formula)
    } else {
      terms(mf$formula, data = data)
    }
    #to evaluate out of this internal loop
    environment(mf$formula) <- formula_env
    df_adjustor <- eval(mf,parent.frame())
  }
  df <- cbind( df_basic,  df_adjustor)
  

  #force the default value to be included in df if needed.
  if( !( "(censor_time)" %in% names(df))){
    df <- cbind(df, "(censor_time)" = Inf)
  }
 
  
  
  # Check that the number of arms is 2.
  if (length(unique(df[, ".arm"])) != 2) {
    stop("arm must have exactly 2 observed values")
  }
  
  # check the values of treatment modifier
  if ("(treat_modifier)" %in% names(df)) {
    if( all(df[,"(treat_modifier)"]==0, na.rm = TRUE) ){ 
      stop("treat_modifier values cannot all be zero")
    }
    if( any(df[,"(treat_modifier)"]<=0) ){ 
      warning("treat_modifier values are not all strictly positive")
    }
  }
  
  psi_old <- 1
  psi <- 0
  iter <- 0
  fit_formula <- update(fit_formula, Sstar ~ .)
  while( epsilon < abs(psi_old-psi) & iter< iter_max){
    psi_old <- psi
   iter <- iter + 1
   if ("(treat_modifier)" %in% names(df)) {
     psi_vector <- psi * df[, "(treat_modifier)"]
   } else{ psi_vector <- psi}
   
   # need to check if this works correctly for log-location models. 
   t0 <- df$time * ((1 - df[,".rx"]) + df[,".rx"] * exp(-psi_vector))
   t1 <- exp(psi_vector)*t0
   t_arm <- ifelse(df[,".arm"]==1, t1, t0)
   t_star <- pmin(t_arm, df[,"(censor_time)"])
   #only change delta if necessary
   delta_star <-  ifelse( df[,"(censor_time)"] < t_star, 0, df[,"status"])
   Sstar <- Surv(t_star, delta_star)
   
   # Sstar <- untreated(psi_vector, df[, "time"], df[, "status"], df[,"(censor_time)"],
   #                    df[,".rx"], df[, ".arm"],  autoswitch=autoswitch)
   # #  algorithm wants to transform to untreated in contorl
   # and fully treated in experimental (paper assumes this is 100% compliance)
   #  Sstar[,1] <- ifelse(df[,".arm"]==1, t1, t0)
   
  # And does it do re-censoring as desired.
   
   df_fit <- cbind(Sstar, df)
   
   fit <- survival::survreg(formula=fit_formula, data=df_fit, dist=dist, ...)
   psi <- fit$coefficients[".arm"]
   
   if(verbose){ cat("iter ",iter, " psi", round(psi,3),"\n")}
  }

  
  
  
  # create a simple KM curve for each recensored arm. Used in the plot
  # function - simple KM curves
  
  
 
    
    
    # Ignores any covariates, strata or adjustors. On Purpose as this is
    # too general to deal with
    fit_km <-survival::survfit(Sstar ~ .arm, conf.int=1-alpha, data = df_fit)
  
  
  regression <- fit#attr(fit, "fit")
  #modify the call and formula (for regressions)
  regression$call <- cl
  regression$formula <- return_formula
  regression$terms <- return_formula
  
  
  value=c(
    list(
      psi=psi, 
      #for the plot function
      fit=fit_km, 
      #CI=c(lower$root,upper$root),
      Sstar=Sstar, 
      rand=rand_object,
      iter=iter),
      #for the print and summary methods
    regression
  )

  if (length(na.action)){ value$na.action <- na.action }
  if( is.na( value$psi )){ value$fail <- "yes"}
  structure(value, class=c("ipe","rpsftm","survreg"))
}


# summary method - fix it  - print out CI or not...
# investigate recensoring, or none. Try to unify with rpsftm options.
# interface into bootstrapping
# compare to MIPE which has a github page
# contact authors to make comparison
# Ian white's thoughts
# tesitng


ipe_statistic <- function(data,index, fit,...){
  data <- data[index,]
  fit0 <- update(fit, data=data,...)
  c(fit0$coefficients, "scale"=fit0$scale)
}

fit_bs <- boot::boot(immdef, ipe_statistic, fit=fit, R=1000)

psi_hat <- fit_bs$t0[".arm"]
index_psi <- which(names(fit_bs$t0)==".arm")
hist(fit_bs$t[,index_psi])
abline(v=psi_hat)
quantile(fit_bs$t[,index_psi],c(0.025,0.5,0.975))

