#'Main Function used for estmating causal parameters under the Rank Preserving Structural Failure Time Model
#'
#'@export
#'@title Rank Preserving Structural Failure Time Model
#'@name rpsftm
#'@inheritParams untreated
#'@inheritParams est_eqn
#'@param formula a formula with a minimal structure of \code{ReCen(time, censor_time)~rand(arm,rx)}.
#'Further terms can be added to the right hand side to adjust for covariates and use strata or cluster arguments.
#'@param data an optional data frame that contains variables
#'@param censor_time the time at which censoring would, or has occurred. This is provided for all observations
#' unlike standard Kaplan-Meier or Cox regression where it is only given for censored observations. 
#'If no value is given then recensoring is not applied.
#'@param subset expression indicating which subset of the rows of data should be used in the fit. 
#'This can be a logical vector (which is replicated to have length equal to the number of observations)
#', a numeric vector indicating which observation numbers are to be included (or excluded if negative), 
#'or a character vector of row names to be included. All observations are included by default.
#'@param na.action a missing-data filter function. This is applied to the model.frame after any subset 
#'argument has been used. Default is options()$na.action.
#' @param low_psi the lower limit of the range to search for the causal parameter
#' @param hi_psi the upper limit of the range to search for the causal paramater
#' @param alpha the significance level used to calculate confidence intervals
#' @param treat_modifier an optional parameter that psi is multiplied by on an individual observation level to give
#' differing impact to treatment.
#' @param n_eval_z: The number of points between hi_psi and low_psi at which to evaluate the Z-statistics
#' in the estimating equation. Default  is 100.
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
#' \item eval_z: a data frame with the Z-statistics from the estimating equation evaluated at
#' a sequence of values of psi. Used to plot and check if the range of values to search for solution
#' and limits of confidence intervals need to be modified.
#' }
#' @details the formula object \code{Surv(time, status)~rand(arm,rx)}. \code{rand()} stands 
#' for randomistion, both the randomly assigned and actual observed treatment. 
#' \itemize{
#'   \item arm: the randomised treatment arm. a factor with 2 levels, or numeric variable with values 0/1.
#'  \item rx: the proportion of time on active treatment (arm=1 or the non-reference level of the factor)
#' }
#' Further adjustment terms can be added on the right hand side of the formula if desired, included \code{strata()}
#' or \code{cluster()} terms. 
#' 
#' @examples 
#' library(rpsftm)
#' ?immdef
#' fit <- rpsftm(Surv(progyrs, status)~rand(imm,1-xoyrs/progyrs),immdef)
#' print(fit)
#' summary(fit)
#' plot(fit)
#' 
#' @author Simon Bond
#' @importFrom survival Surv strata cluster


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
  mf[[1L]] <- quote(stats::model.frame)
  # this ultimately returns a data frame with all the variables in and
  # renamed time, censor_time, rx, arm as appropriate
  special <- c("rand")
  mf$formula <- if (missing(data)) {
    terms(formula, special)
  } else {
    terms(formula, special, data = data)
  }
  formula_env <- new.env(parent = environment(mf$formula))
  assign("rand", rand, envir = formula_env)
  environment(mf$formula) <- formula_env
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
  
  if(length(formula>2)){
    ytemp <- terms.inner(formula[[2]])
    xtemp <- terms.inner(formula[[3]])
    if (any(!is.na(match(xtemp, ytemp)))) 
      warning("a variable appears on both the left and right sides of the formula")
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
  
  # remedies the df being a list of lists into just 1 list
  df <- cbind(time=df[,response_index][,"time"], status=df[,response_index][,"status"], 
              df[, rand_index], df[, -c(response_index, rand_index), drop = FALSE])
  #force the default value to be included in df if needed.
  if( !( "(censor_time)" %in% names(df))){
    df <- cbind(df, "(censor_time)" = Inf)
  }
 
  fit_formula <- terms(update(mf$formula, . ~ arm + .))
  fit_formula <- drop.terms(fit_formula, dropx = 1 + rand_drops, keep.response = FALSE)
  
  
  # Check that the number of arms is 2.
  if (length(unique(df[, "arm"])) != 2) {
    stop("arm must have exactly 2 observed values")
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
  
  #need to wrap est_eqn in try() to cope with non-convergent fits
  
  fn_eval_z <- function(psi){
    answer <- try( est_eqn(psi,data = df, formula = fit_formula, target = 0, 
                           test = test, 
                           autoswitch = autoswitch, ... = ... ),
                   silent = TRUE
                   )
    if (class(answer) == "try-error"){ 
      return(NA)
    } else {
      return(answer)
    }
  }
  
  eval_z$Z <- apply(eval_z,1, fn_eval_z)
  
  # Preliminary check of low_psi and hi_psi with a meaningful warning
  
  est_eqn_low <- est_eqn(low_psi, data = df, formula = fit_formula, target = 0, 
                         test = test,  autoswitch = autoswitch, ... = ...)
  est_eqn_hi <- est_eqn(hi_psi, data = df, formula = fit_formula, target = 0, 
                        test = test, autoswitch = autoswitch, ... = ...)
  if (est_eqn_low * est_eqn_hi > 0) {
    message <- paste("\nThe starting interval (", low_psi, ", ", hi_psi, 
                     ") to search for a solution for psi\ngives values of the same sign (", 
                     signif(est_eqn_low, 3), ", ", signif(est_eqn_hi, 3), ").\nTry a wider interval. plot(obj$eval_z, type=\"s\"), where obj is the output of rpsftm()", 
                     sep = "")
    warning(message)
  }
  
  # solve to find the value of psi that gives the root to z=0, and the
  # limits of the CI.
  
  root <- function(target) {
    uniroot(est_eqn, c(low_psi, hi_psi), data = df, formula = fit_formula, 
            target = target, test = test, autoswitch = autoswitch, 
            ... = ...)
  }
  ans <- try(root(0), silent = TRUE)
  lower <- try(root(qnorm(1 - alpha/2)), silent = TRUE)
  upper <- try(root(qnorm(alpha/2)), silent = TRUE)
  
  # handle errors in root and CI finding
  
  ans.error <- class(ans) == "try-error"
  lower.error <- class(lower) == "try-error"
  upper.error <- class(upper) == "try-error"
  if (ans.error) {
    warning("Evaluation of the estimated values of psi failed. It is set to NA")
    ans <- list(root = NA)
  }
  if (lower.error) {
    warning("Evaluation of a limit of the Confidence Interval failed.  It is set to NA")
    lower <- list(root = NA)
  }
  if (upper.error) {
    warning("Evaluation of a limit of the Confidence Interval failed.  It is set to NA")
    upper <- list(root = NA)
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
    
    
    Sstar <- untreated(psiHat, df[, "time"], df[, "status"], 
                       df[,"(censor_time)"],df[, "rx"], df[, "arm"], autoswitch)
    # Ignores any covariates, strata or adjustors. On Purpose as this is
    # too general to deal with
    fit <- survival::survfit(Sstar ~ arm, data = df)
  } else {
    fit <- NULL
    Sstar <- NULL
  }
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
       call=cl,
       eval_z=eval_z
       )

  if (length(na.action)){ value$na.action <- na.action }
  structure(value, class="rpsftm")
}