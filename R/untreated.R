#' Calculates the counterfactual untreated event time, possibly using the recensoring method, assuming a given parameter value and returns a Surv() object
#' 
#' @export
#' @title Untreated Event Time
#' @name untreated
#' @param psi the parameter that measures how more rapidly the lifetime is expended under treatment
#' @param time the observed failure or censoring time.
#' @param delta the status of the time: 1 = observed, 0 = censored.
#' @param censor_time the theoretical censoring time, either observed or set after time. Set to Inf to ignore recensoring.
#' @param rx the proportion of time spent on treatment
#' @param arm the randomised arm. Either a numerical indicator, with 0 as the placebo, or a factor with the lowest level as placebo.
#' @param autoswitch a logical to autodetect cases of no switching. Default is TRUE. If all observations in an arm
#' have perfect compliance then recensoring is not applied in that arm. If FALSE the recensoring is applied
#' regardless of perfect compliance.
#' @return A Surv() object
#' @author Simon Bond
#' @importFrom survival Surv

untreated <- function(psi, time, delta, censor_time, rx, arm, autoswitch) {
  
  
  
  
  
  if (any(!(0 <= rx & rx <= 1))) {
    stop("Invalid values for rx. Must be proportions in [0,1]")
  }
  if (any(censor_time < time)) {
    warning("You have observed events AFTER censoring")
  }
  
 
  
  u <- time * ((1 - rx) + rx * exp(psi))
  
  
  #make use of setting censor_time=Inf to avoid recensoring, and implimenting Autoswitch
  c_star <- pmin( censor_time , censor_time * exp(psi) )
  if( autoswitch){
    if( all(rx[arm==1]==1)){ c_star <- ifelse(arm==1, Inf, c_star)}
    if( all(rx[arm==0]==0)){ c_star <- ifelse(arm==0, Inf, c_star)}
  }
  
  t_star <- pmin(u, c_star)
  #only change delta if necessary
  delta_star <-  ifelse( c_star < u, 0, delta)
  output <- Surv(t_star, delta_star)
  return(output)
}