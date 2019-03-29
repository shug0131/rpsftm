#'Main Function used for estimating causal parameters under the Rank Preserving Structural Failure Time Model
#'
#'@export
#'@title Rank Preserving Structural Failure Time Model
#'@name rpsftm
#'@inheritParams untreated
#'@inheritParams est_eqn
#'@param formula a formula with a minimal structure of \code{Surv(time, status)~rand(arm,rx)}.
#'Further terms can be added to the right hand side to adjust for covariates and use strata or 
#'cluster arguments.
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
#' @param low_psi the lower limit of the range to search for the causal parameter
#' @param hi_psi the upper limit of the range to search for the causal parameter
#' @param alpha the significance level used to calculate confidence intervals
#' @param treat_modifier an optional variable that psi is multiplied by on an individual observation level to give
#' differing impact to treatment.
#' @param conf_interval a logical to determine if confidence intervals will be calculated for psi. Default is TRUE
#' @param ci_search_limit a matrix with 2 columns and the length of psi for the number of rows. 
#' The confidence intervals are found by solving a univariate equation, searching between the estimate of psi, and psi plus an increment, given by the matrix. 
#' It defaults to NULL in which case the increments are heuristically given as 6*dim(psi)/sqrt(number of events).
#' @param n_eval_z The number of points to store evaluations of the chi-squared statistic minimised in 
#' the estimating equation. Default  is 100.
#' @return a rpsftm method object that is a list of the following:
#' \itemize{
#' \item psi: the estimated parameter
#' \item fit: a survdiff object to produce Kaplan-Meier curves of the estimated counterfactual untreated failure times for each treatment arm
#' \item CI: a vector of the confidence interval around psi
#' \item Sstar: the recensored \code{Surv()} data using the estimate value of psi to give counterfactual untreated failure times.
#' \item rand: the rand() object used to specify the allocated and observed amount of treatment.
#' \item ans: the values from \code{\link[rootSolve]{uniroot.all}} used to solve the estimating equation, 
#' but embedded within a list as per \code{\link[stats]{uniroot}}, with an extra element \code{root_all},
#' a vector of all roots found in the case of multiple solutions. The first element of \code{root_all} 
#' is subsequently used.
#' \item evaluation: a matrix with the chi-squared statistic from the estimating equation evaluated at
#' a sequence of values of psi. Used to plot and check if the range of values to search for solution
#' and limits of confidence intervals need to be modified.
#' \item Further elements corresponding to either a \code{survdiff}, \code{coxph}, or \code{survreg} object. This will always include:
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
#' fit <- rpsftm(Surv(progyrs, prog)~rand(imm,1-xoyrs/progyrs),immdef, censyrs)
#' print(fit)
#' summary(fit)
#' plot(fit)
#' 
#' @author Simon Bond
#' @importFrom survival Surv strata cluster survdiff
#' @importFrom stats terms model.extract update drop.terms reformulate uniroot qnorm

rpsftm_multi <- function(formula, data, censor_time, subset, na.action,  test = survdiff, start=0, alpha = 0.05, treat_modifier = 1, autoswitch = TRUE, 
                   n_eval_z = 100, method="Nelder-Mead", conf_interval=TRUE, ci_search_limit=NULL, ...) {
  cl <- match.call()
  
  # create formula for fitting, and to feed into model.frame() from the
  # lm() function
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "censor_time", "treat_modifier", "subset", "na.action"), 
             names(mf), 0L)
 
  mf <- mf[c(1L, m)]
 
  mf$drop.unused.levels <- TRUE
  
  # this ultimately returns a data frame, df, with all the variables in and
  special <- c("strata","cluster","rand")
  formula_list <- one_becomes_three(formula)
  mf$formula <- add_formula_list(formula_list, env=environment(formula))
 
  #modify the data argument to have all the variables 
 
  
  simple_formula <- reformulate(terms.inner(mf$formula))
  environment(simple_formula) <- environment(formula)
  
  temp <- mf
  temp$formula <- simple_formula
  temp[[1L]] <- quote(stats::model.frame)
  data <- eval(temp,parent.frame())
  #print(head(data))
  
  mf$formula <- if (missing(data)) {
    terms(mf$formula, special)
  } else {
    terms(mf$formula, special, data = data)
  }
  
  # from coxph, not clear why this is needed, but it fails in testthat() without.
  coxenv <- new.env(parent = environment(mf$formula))
  assign("Surv", Surv, env = coxenv)
  assign("strata", strata, env = coxenv)
  assign("cluster", cluster, env = coxenv)
  environment(mf$formula) <- coxenv
  
  
  mf[[1L]] <- quote(stats::model.frame)
  df <- eval(mf, parent.frame())
  #print(head(df))
  #df has everything in it  with na.action applied universally
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
  
  if(length(formula>2)){
    ytemp <- terms.inner(formula[[2]])
    xtemp <- terms.inner(formula[[3]])
    if (any(!is.na(match(xtemp, ytemp)))) 
      warning("a variable appears on both the left and right sides of the formula")
  }
  
  
  
  # rand_index <- attr(mf$formula, "specials")$rand
  # rand_drops <- which(attr(mf$formula, "factors")[rand_index, ] > 0)
  # if (length(rand_drops) != 1) {
  #   stop("Exactly one rand() term allowed")
  # }
  # rand_column <- attr(mf$formula, "factors")[, rand_drops]
  # if (sum(rand_column > 0) > 1) {
  #   stop("rand() term must not be in any interactions")
  # }
  # 
  # check for special terms
  # if( length(labels(mf$formula))>1){
  #   adjustor_formula <- drop.terms( mf$formula, dropx = rand_drops , keep.response = FALSE)
  #   adjustor_names <- unlist( lapply( attr( terms( adjustor_formula), "variables"), terms.inner)[-1])
  #   if( any( adjustor_names %in% c(".arm",".rx","time","status"))){
  #     warning( "'.arm', '.rx', 'time', 'status' are used internally within rpsftm. Please rename these variables used in the formula outside of rand() and surv()")
  #   }
  # }
  # add in the special covariate .arm
  
 
  

  #force the default value to be included in df if needed.
  if( !( "(censor_time)" %in% names(data))){
    data <- cbind(data, "(censor_time)" = Inf)
    attr(data,"terms") <- mf$formula
  }
  
  if (sum(data[,"(censor_time)"] < Y[,"time"])) {
    warning("You have observed events AFTER censoring")
  }
  
  
#
  treatment_matrix <- model.matrix(formula_list$treatment, data=df)
  if (sum(!(0 <= treatment_matrix & treatment_matrix <= 1))) {
    stop("Invalid values for treatment. Must be proportions in [0,1]")
  }
  
  
  rand_matrix <- model.matrix(formula_list$randomise, data=df)
  
  p <- qr(rand_matrix)$rank
  q <- max( ncol(treatment_matrix), qr(treatment_matrix)$rank)
  
  
  if (p!=q) {
    stop("the treament and rand model matrices must be the same rank & dimensions")
  }
  
  # check the values of treatment modifier
  if ("(treat_modifier)" %in% names(data)) {
    if( all(data[,"(treat_modifier)"]==0, na.rm = TRUE) ){ 
      stop("treat_modifier values cannot all be zero")
    }
    if( any(data[,"(treat_modifier)"]<=0) ){ 
      warning("treat_modifier values are not all strictly positive")
    }
  }
  
  
  
  test <- deparse(substitute(test))
  if (is.na(match(test, c("survdiff", "coxph", "survreg")))) {
    stop("Test must be one of: survdiff, coxph, survreg")
  }
  
  #check the format of n_eval_z is an single integer >=2
  if (!is.numeric(n_eval_z)|| length(n_eval_z)>1 || n_eval_z<2) {stop ("invalid value of n_eval_z")}
  #create values of psi to evaluate in a data frame
 
 # capture the evaluation of the test statistic
  
  assign("fn_count", 0, envir= environment(min_eqn))
  assign("n_eval_z", n_eval_z, envir= environment(min_eqn))
  assign("evaluation", matrix(0,nrow=n_eval_z, ncol=p), 
	envir=environment(min_eqn))
  
  
  
  # solve to find the value of psi that gives the root to z=0, and the
  # limits of the CI.
  
  root <- function(target) {
    #est_eqn_vectorize <- Vectorize(min_eqn, vectorize.args="psi")
    start[1:(p-1)] <- start
    ans <- optim(par=start, fn=min_eqn, method=method,
                                #  lower=low_psi, upper=hi_psi,
                                data = data, formula_list=formula_list, 
                                  response=Y,
                                  treatment_matrix=treatment_matrix, 
                                  rand_matrix=rand_matrix, 
                                  target = target, test = test, autoswitch = autoswitch, 
                                  ... = ...)
    #give back the same elements as uniroot()
   
    if( length(ans)>1){warning("Multiple Roots found")}
    list( root=ans$par, 
          details=ans,
          f.root= min_eqn(ans$par,
                          data = data, formula_list=formula_list, 
                          response=Y,
                          treatment_matrix=treatment_matrix, 
                          rand_matrix=rand_matrix,
                          target = target, test = test, autoswitch = autoswitch, 
                          ... = ...),
          iter=NA,
          estim.prec=NA
    )  
    
  }
  
  
  
  ans <- try(root(0), silent = TRUE)
  ans.error <- class(ans) == "try-error"
 
  #fit a K-M curve on the untreated transformed times
  
  if (!is.na(ans$root)) {
    psiHat <- ans$root
    if ("(treat_modifier)" %in% names(df)) {
      psiHat <- psiHat * df[, "(treat_modifier)"]
    }
    
    
    #response <- model.response(df)#model.frame(formula_list$formula, data=df))
    #treatment_matrix <- model.matrix(formula_list$treatment, data=df)
    #rand_matrix <- model.matrix(formula_list$randomise, data=df)
    
    Sstar <- untreated(psiHat, Y,treatment_matrix, rand_matrix, data[,"(censor_time)"], autoswitch)
    
    df <- cbind(Sstar, df)
    # Ignores any covariates, strata or adjustors. On Purpose as this is
    # too general to deal with
    fit <- survival::survfit(update(formula_list$randomise, Sstar ~ .) , data = data, conf.int=1-alpha)
  } else {
    fit <- NULL
    Sstar <- NULL
  }
  
  

  
  # Preliminary check of ans,  low_psi and hi_psi with a meaningful warning
  
# for CI.  Modify the min_eqn to 
# a) minimise the chisq  over the nuisance parameters with the parameter of interest fixed, 
# b) find the value of the parameter of interest that hits the target qchisq(1-alpha,df=1)
# this is 1 dimensional so use root finding, and limit to search in one direction from the estimate
# to get the upper and lower values. 
  
min_eqn_profile <- function(nuisance,x, index, test_args, ...){
  psi <- rep(0,length(nuisance)+1)
  psi[-index] <- nuisance
  psi[index] <- x
  # https://www.burns-stat.com/pages/Tutor/R_inferno.pdf 8.3.15 ???
  dots <- list(...)
  do.call(min_eqn, c(list(psi),dots,test_args))
 }  
  
  
find_limit <- function(x, outer_target=qchisq(1-alpha,df=1),psi_hat,
                       index, ... )  {
  optim(par=psi_hat[-index], fn=min_eqn_profile,
        target = 0, gr=NULL,
        index=index, 
        x=x,...)$value - outer_target
  
}

dim_psi <- length(ans$root)
n_events <- sum(fit$n.event)


if( conf_interval){
  CI <- data.frame(lower=rep(NA,dim_psi), upper=rep(NA, dim_psi))
  if(is.null(ci_search_limit)){
    limit <- rep(6*dim_psi/sqrt(n_events), dim_psi)
    ci_search_limit <- cbind(-limit, limit)
    }
  
  
  for( column in 1:2){
    for(index in 1:dim_psi){
      
      limit <-try(uniroot(
        find_limit,c(ans$root[index], ans$root[index]+ci_search_limit[index, column]), 
        index=index, psi_hat=ans$root,
        data = data, formula_list=formula_list, 
        treatment_matrix=treatment_matrix, 
        rand_matrix=rand_matrix,
        test = test, autoswitch = autoswitch, 
        response=Y,method=method,
        test_args = list(...)
      ))
      
      if(class(limit)!="try-error"){CI[index,column] <- limit$root}
    }
  }
} else { 
  CI <- NULL
}



  
  # create a simple KM curve for each recensored arm. Used in the plot
  # function - simple KM curves
  
  
  
  evaluation <- get("evaluation", envir=environment(min_eqn))
  colnames(evaluation) <- c(paste0("psi_",1:dim_psi),"chi_sq")
  fn_count <- get("fn_count", envir=environment(min_eqn))
  




  regression=attr(ans$f.root, "fit")
  #modify the call and formula (for regressions)
  regression$call <- cl
  regression$formula <- formula
  regression$terms <- formula
  
  
  value=c(
    list(
      psi=ans$root, 
      #for the plot function
      fit=fit, 
     CI=CI,
      Sstar=Sstar, 
      formula_list=formula_list,
      #rand=rand_object,
      ans=ans,
      evaluation=evaluation[1:min(fn_count,n_eval_z),],
      data=df),
      #for the print and summary methods
    regression
  )

  if (length(na.action)){ value$na.action <- na.action }
  if( is.na( value$psi )){ value$fail <- "yes"}
  structure(value, class=c("rpsftm",test))
}