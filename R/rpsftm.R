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
#' \item ans: the values from \code{uniroot_all} used to solve the estimating equation, 
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

rpsftm <- function(formula, data, censor_time, subset, na.action,  test = survdiff, 
                   alpha = 0.05, treat_modifier = 1, autoswitch = TRUE,
                   n_eval_z = 100,
                   # 1-d psi
                   low_psi = -1, hi_psi = 1,  
                   # 2+ -d psi
                   method="Nelder-Mead", conf_interval=TRUE, 
                   start=NULL, ci_search_limit=NULL,
                   
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
  
  
  mf[[1L]] <- quote(stats::get_all_vars)
  df_simple <- try(eval(mf, parent.frame()))
  #this can fail if _all_ the variables are at a global level, rather than in a data.frame
  # but then we can't deal with formulae like I(1-xoyr/progyrs) for the print_rand() function.
  
  mf[[1L]] <- quote(stats::model.frame)
  df <- eval(mf, parent.frame())
  if(class(df_simple)=="try-error"){df_simple <- df}
 
  
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
  
  #force the default value to be included in df if needed.
  if( !( "(censor_time)" %in% names(data))){
    
    #finite approximation to infinity
    max_time <- max(abs(Y[,1]))
    # Note that if exp(psi)>1000 then this could fail to work. 
    # But that would be an implausible parameter value in medical statistics at least.
    data <- cbind(data, "(censor_time)" = 1000*max_time)
    attr(data,"terms") <- mf$formula
  }
  if (any(data[,"(censor_time)"] < Y[,"time"])) {
    warning("You have observed events AFTER censoring")
  }
  
#
  
  
  treatment_matrix <- model.matrix(formula_list$treatment, data=df)  
  rand_matrix <- model.matrix(formula_list$randomise, data=df)
  
  if (any(!(0 <= treatment_matrix & treatment_matrix <= 1))) {
    stop("Invalid values for treatment. Must be proportions in [0,1]")
  }
  # need to ignore the intercept column.
  if( any( 1 < rowSums(treatment_matrix[,-1, drop=FALSE]) | 1<treatment_matrix[,-1, drop=FALSE] )){
    warning("Your treaments are individually, or sum to, greater than 1")}
  
  
  
  
  
  p <- qr(rand_matrix)$rank
  #q <- qr(treatment_matrix)$rank
  q <- max( ncol(treatment_matrix), qr(treatment_matrix)$rank)
  
  if (p!=q) {
    stop("the treament and randomisation model matrices must be the same rank")
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
  
 
  
  
  ### THIS is  good place to fork for 1d and 2+d psi...
  
  
  
  if(p==2){
    object <- est_eqn
    action <-  uniroot_all
    action_inputs <- list( f=est_eqn_vectorize,
                           interval=c(low_psi, hi_psi)
                           )
  } else{
    object <- min_eqn
    action <- optim
    # do a check on the length of start
    if( is.null(start)){start <- rep(0,p-1)}
    if( length(start)!=p-1){stop("your starting parameters `start` are the wrong length")}
    action_inputs <- list( par=start,
                           fn=min_eqn,
                           method=method
                           )
  }
  
  object_inputs <- list(data = data, 
                        formula_list=formula_list, 
                        response=Y,
                        treatment_matrix=treatment_matrix, 
                        rand_matrix=rand_matrix, 
                        test = test, autoswitch = autoswitch, 
                        ... = ... )
  
  
  #check the format of n_eval_z is an single integer >=2
  if (!is.numeric(n_eval_z)|| length(n_eval_z)>1 || n_eval_z<2) {stop ("invalid value of n_eval_z")}
  #initialisation to keep a history of evaluations of the object function up to the n_eval_z -th evaluation
  reset_record(n_eval_z)
  
  
  # solve to find the value of psi that gives the root to z=0,
  
  root <- function(target) {
    # works as the functino is defined internally, so can find the objects below.
     ans <- do.call(action, c(action_inputs, list(target=target),object_inputs))
     
    if( p==2 & length(ans)>1){warning("Multiple Roots found")}
    output=unlist(ans[1]) # to pick out whatever is given back first : uniroot or optim.
    list( root=output, 
          details=ans,
          f.root= do.call(object,c(list(psi=output, target=target),object_inputs))
    )  
  }
  ans <- try(root(0), silent = TRUE)
  
  ans.error <- class(ans) == "try-error"
  if (ans.error) {
    warning("Evaluation of the estimated values of psi failed. It is set to NA")
    ans <- list(root = NA)
  }
 
 #Failure diagnostics for 1d psi
if(p==2){  
  # Preliminary check of ans,  low_psi and hi_psi with a meaningful warning
  est_eqn_low <- do.call(object,c(list(psi=low_psi), list(target=0),object_inputs))
  est_eqn_hi <- do.call(object,c(list(psi=hi_psi), list(target=0),object_inputs))
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
}
  
  #Find limits to confidence interval
  
  
  CI <- conf_interval( alpha=alpha, object_inputs=object_inputs,
                       psi_hat=ans$root, 
                       ci_search_limit=ci_search_limit)
  
 
  
  
  # create a simple KM curve for each recensored arm. Used in the plot
  # function - simple KM curves
  
  
  if (!any(is.na(ans$root))) {
    psiHat <- ans$root
    if ("(treat_modifier)" %in% names(df)) {
      psiHat <- psiHat * df[, "(treat_modifier)", drop=FALSE]
    }else{
      psiHat <- matrix(psiHat, nrow=nrow(df), ncol=length(psiHat), byrow=TRUE)
    }
    
    rand_var <- data[, attr(formula_list$randomise,"term.labels")]
    Sstar <- untreated(psiHat, Y,treatment_matrix, rand_var, data[,"(censor_time)"], autoswitch)
    
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
      CI=CI,
      Sstar=Sstar, 
      formula_list=formula_list,
      #rand=rand_object,
      ans=ans,
      eval_z=list(rpsftm_env$psi, rpsftm_env$value),#evaluation[1:min(fn_count,n_eval_z),],
      data=df_simple),
      #for the print and summary methods
    regression
  )

  if (length(na.action)){ value$na.action <- na.action }
  if( any(is.na( value$psi ))){ value$fail <- "yes"}
  structure(value, class=c("rpsftm",test))
}