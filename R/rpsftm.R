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
#' @param n_eval_z The number of points between hi_psi and low_psi at which to evaluate the Z-statistics
#' in the estimating equation. Default  is 100.
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
#' \item eval_z: a data frame with the Z-statistics from the estimating equation evaluated at
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

rpsftm <- function(formula, data, censor_time, subset, na.action,  test = survdiff, low_psi = -1, 
                   hi_psi = 1, alpha = 0.05, treat_modifier = 1, autoswitch = TRUE, 
                   n_eval_z = 100, ...) {
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
 
  
#
  
  p <- qr(model.matrix(formula_list$randomise, data=df))$rank
  q <- qr(model.matrix(formula_list$treatment, data=df))$rank
  
  
  if (p!=q) {
    stop("the treament and rand model matrices must be the same rank")
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
  eval_z <- data.frame( psi = seq(low_psi, hi_psi, length=n_eval_z))
  # evaluate them
  #print(head(df))
  
  treatment_matrix <- model.matrix(formula_list$treatment, data=df)
  rand_matrix <- model.matrix(formula_list$randomise, data=df)
  
  eval_z$Z <- apply(eval_z,1, est_eqn, 
                    response=Y,
                    data=data, 
                    formula_list=formula_list, 
                    treatment_matrix=treatment_matrix, 
                    rand_matrix=rand_matrix,
                    test=test,
                    target=0, autoswitch=autoswitch,...=...)
 
  
  # solve to find the value of psi that gives the root to z=0, and the
  # limits of the CI.
  
  root <- function(target) {
    est_eqn_vectorize <- Vectorize(est_eqn, vectorize.args="psi")
    ans <- rootSolve::uniroot.all(est_eqn_vectorize, 
                                  c(low_psi, hi_psi), data = data, formula_list=formula_list, 
                                  response=Y,
                                  treatment_matrix=treatment_matrix, 
                                  rand_matrix=rand_matrix, 
                                  target = target, test = test, autoswitch = autoswitch, 
                                  ... = ...)
    #give back the same elements as uniroot()
    if( length(ans)>1){warning("Multiple Roots found")}
    list( root=ans[1], 
          root_all=ans,
          f.root= est_eqn(ans[1],
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
  
 
  
  # Preliminary check of ans,  low_psi and hi_psi with a meaningful warning
  
  est_eqn_low <- eval_z[1,"Z"]
  est_eqn_hi <-  eval_z[n_eval_z,"Z"]
  
  
  if (
      (  # still get try-errors in simple case as I guess uniroot.all, looks at silly places for psi and crashes survdiff
        (!ans.error && length(ans$root_all)==0 ) || 
         ans.error
      ) &&
      !is.na( est_eqn_low) && 
      !is.na( est_eqn_hi ) &&  
      est_eqn_low * est_eqn_hi > 0) {
    message <- paste("\nThe starting interval (", low_psi, ", ", hi_psi, 
                     ") to search for a solution for psi\ngives values of the same sign (", 
                     signif(est_eqn_low, 3), ", ", signif(est_eqn_hi, 3), ").\nTry a wider interval. plot(obj$eval_z, type=\"s\"), where obj is the output of rpsftm()", 
                     sep = "")
    warning(message)
  }
  
  #Find limits to confidence interval
  
  lower <- try(root(qnorm(1 - alpha/2)), silent = TRUE)
  upper <- try(root(qnorm(alpha/2)), silent = TRUE)
  
  # handle errors in root and CI finding
  
 
  lower.error <- class(lower) == "try-error"
  upper.error <- class(upper) == "try-error"
  if (ans.error) {
    warning("Evaluation of the estimated values of psi failed. It is set to NA")
    ans <- list(root = NA)
  }
  if (lower.error) {
    lower <- list(root = NA)
  }
  if (upper.error) {
    upper <- list(root = NA)
  }
  if (lower.error|| upper.error) {
    warning("Evaluation of a limit of the Confidence Interval failed.  It is set to NA")
  }
  
  
  
  # sort the ordering
  if (!is.na(upper$root) & !is.na(lower$root) & upper$root < lower$root) {
    temp <- lower
    lower <- upper
    upper <- temp
    rm(temp)
  }
  
  
  
  
  
  
  # create a simple KM curve for each recensored arm. Used in the plot
  # function - simple KM curves
  
  
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
      CI=c(lower$root,upper$root),
      Sstar=Sstar, 
      formula_list=formula_list,
      #rand=rand_object,
      ans=ans,
      eval_z=eval_z,
      data=df),
      #for the print and summary methods
    regression
  )

  if (length(na.action)){ value$na.action <- na.action }
  if( is.na( value$psi )){ value$fail <- "yes"}
  structure(value, class=c("rpsftm",test))
}