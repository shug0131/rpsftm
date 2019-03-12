#'Calculates the Estimating Equation to be solved in RPSFTM models
#' 
#' @title Estimating Equations for \code{rpsftm()}
#' @name est_eqn
#' @inheritParams untreated
#' @param data the data set that contains the variables. 
#' Must contain columns named: time, censor_time,rx, arm.
#' Optionally a column named: treat_modifier
#' @param formula a formula object of covariates to adjust for: \code{~strata(A)+B*C}
#' @param rand a one-side formula giving the randomised arms
#' @param treatment a one-sided formula giving the amount of treatment recieved. 
#' The model matrix it produces must be of the same rank as for the \code{rand} argument. So either remove the intercept with '-1',
#'  or only provide \code{(k-1)} terms on a scale \emph{relative} to the control treatment, where \code{k} is the number of arms.
#' @param target the value to subtract off from the z-statistic 
#' @param test the survival regression function to calculate the z-statistic: survdiff, coxph, survreg
#' @param ... arguments to supply to the test function.
#' @return A scalar value of the estimating equation: the z-statistics from a test minus a target value
#' @seealso \code{\link{untreated}}
#' @author Simon Bond
#' @importFrom stats update
#' @keywords internal


min_eqn <- function(psi, response, data, formula_list, treatment_matrix, rand_matrix,  target = 0, test = "survdiff", 
                   autoswitch, ...) {
  
  if ("(treat_modifier)" %in% names(data)) {
      psi <- psi * data[, "(treat_modifier)"]
  }
  
  #response <- model.response(data)
  #treatment_matrix <- model.matrix(treatment, data=data)
  #rand_matrix <- model.matrix(rand, data=data)
  
  Sstar <- untreated(psi, response,treatment_matrix, rand_matrix, data[,"(censor_time)"], autoswitch)
  data <- cbind(Sstar, data)
  # build a formula object,
  fit_formula <- reformulate(  c(as.character(formula_list$formula)[3], as.character(formula_list$rand)[2]), response="Sstar")
  rand_names <- colnames(rand_matrix)
  rand_names <- rand_names[rand_names!="(Intercept)"]
  
  # allow different methods to test the independence of arm constrained
  # to be from the survival package.
  functionName <- get(test, asNamespace("survival"))
  fit <- do.call(functionName, list(fit_formula, data, ...))
  # a 'cheat' to enable this to plugged into uniroot as a function that
  # returns a number AND store the fit object.
    .value <-  extract_chisq(fit, arm = rand_names) - target 
  attr(.value, "fit") <- fit
  .value
}