#' Calculates the counterfactual untreated event time, possibly using the recensoring method, assuming a given parameter value and returns a Surv() object
#' 
#' @title Untreated Event Time
#' @name untreated
#' @param psi the parameter that measures how more rapidly the lifetime is expended under treatment
#' @param response the response variable, a Surv() object
#' @param censor_time the theoretical censoring time, either observed or set after time. Set to Inf to ignore recensoring.
#' @param treatment_matrix a model.matrix object capturing the amount of active treatment(s) received
#' @param rand_matrix a model.matrix object catpuring the randomised treatment arms. Unused at presnet
#' @param autoswitch Currently has no effect ( formerly: a logical to autodetect cases of no switching. If TRUE, then if all observations in an arm
#' have perfect compliance then recensoring is not applied in that arm. If FALSE the recensoring is applied
#' regardless of perfect compliance.)
#' @return A Surv() object
#' @author Simon Bond
#' @importFrom survival Surv
#' @keywords internal

untreated <- function(psi, response,treatment_matrix, rand_matrix, censor_time, autoswitch) {
  
  time <- response[,"time"]
  delta <- response[,"status"]
  treatment_matrix <- treatment_matrix[, colnames(treatment_matrix)!="(Intercept)", drop=FALSE]
  
  if (any(!(0 <= treatment_matrix & treatment_matrix <= 1))) {
    stop("Invalid values for rx. Must be proportions in [0,1]")
  }
  if (any(censor_time < time)) {
    warning("You have observed events AFTER censoring")
  }
  
 
  nontreatment <- 1-apply(treatment_matrix,1,sum)
  treatment_matrix <- sweep(treatment_matrix,2, exp(psi), FUN="*")
  treatment <- apply(treatment_matrix,1, sum)
  
  u <- time * (nontreatment + treatment)
  
  
  #make use of setting censor_time=Inf to avoid recensoring, and implimenting Autoswitch
  c_star <- apply(cbind( censor_time , censor_time %o% exp(psi)),1, min)
  if( autoswitch){
    #if( all(rx[arm==1]==1)){ c_star <- ifelse(arm==1, Inf, c_star)}
    #if( all(rx[arm==0]==0)){ c_star <- ifelse(arm==0, Inf, c_star)}
  }
  
  t_star <- pmin(u, c_star)
  #only change delta if necessary
  delta_star <-  ifelse( c_star < u, 0, delta)
  output <- Surv(t_star, delta_star)
  return(output)
}