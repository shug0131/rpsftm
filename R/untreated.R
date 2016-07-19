#' Calculates the counterfactual untreated event time, possibly using the recensoring method, assuming a given parameter value and returns a Surv() object
#' 
#' @title Untreated Event Time
#' @name untreated
#' @param psi the parameter that measures how more rapidly the lifetime is expended under treatment
#' @param time the observed failure or censoring time.
#' @param censor_time the theoretical censoring time, either observed or set after time.
#' @param rx the proportion of time spent on treatment
#' @param arm the randomised arm. Either a numerical indicator, with 0 as the placebo, or a factor with the lowest level as placebo.
#' @param recensor a logical to use recensoring if set to TRUE. Default is TRUE. If FALSE then
#' the status of censoring or event is unchanged, but the time and censor_time values are both transformed according
#' to the proportion of time on treatment. If FALSE then recensoring is applied at the earliest of either 
#' a transformed or untransformed censor_time. recensoring avoids the bias induced by 
#' informative censoring on the transformed scale.
#' @param autoswitch a logical to autodetect cases of no switching. Default is TRUE. If all observations in an arm
#' have perfect compliance then recensoring is not applied in that arm. If FALSE the recensoring is applied
#' regardless of perfect compliance.
#' @return A Surv() object
#' @author Simon Bond
#' @importFrom survival Surv

untreated <- function(psi, time, censor_time, rx, arm, recensor, autoswitch) {
  
  
  
  
  
  if (any(!(0 <= rx & rx <= 1))) {
    stop("Invalid values for rx. Must be proportions in [0,1]")
  }
  if (any(censor_time < time)) {
    warning("You have observed events AFTER censoring. These are handled as censored")
  }
  u <- time * ((1 - rx) + rx * exp(psi))
  # calculate original status. Might be removed with revised syntax
  delta <- 1*( time < censor_time)
  
  #make use of setting censor_time=Inf to avoid recensoring, and implimenting Autoswitch
  c_star <- pmin( censor_time , censor_time * exp(psi) )
  if( autoswitch){
    if( all(rx[arm==1]==1)){ c_star <- ifelse(arm==1, Inf, c_star)}
    if( all(rx[arm==0]==0)){ c_star <- ifelse(arm==0, Inf, c_star)}
  }
  
  #might be removed with revised syntax
  if( !recensor){ c_star <- Inf}
  
  #if (!recensor) {
  #  scenario <- 1
  #} else {
  #  if (!autoswitch) {
  #    scenario <- 2
  #  } else {
  #    if (any(0 < rx[arm == 0]) & any(rx[arm == 1] < 1)) {
  #      scenario <- 2
  #    }
  #    if (any(0 < rx[arm == 0]) & all(rx[arm == 1] == 1)) {
  #      scenario <- 3
  #    }
  #    if (any(rx[arm == 1] < 1) & all(rx[arm == 0] == 0)) {
  #      scenario <- 4
   #   }
  #    if (all(rx == arm)) {
  #      scenario <- 1
  #    }
  #  }
    
  #}
  
  #c_star <- switch(scenario, 
  #                 censor_time * ((1 - rx) + rx * exp(psi)), 
  #                 pmin(censor_time, censor_time * exp(psi)), 
  #                 arm * censor_time * exp(psi) + (1 - arm) * pmin(censor_time, censor_time * exp(psi)), 
  #                 arm * pmin(censor_time, censor_time * exp(psi)) + (1 - arm) * censor_time)
  
  t_star <- pmin(u, c_star)
  #delta_star <- 1 * (u < c_star)
  #only change delta if necessary
  delta_star <-  ifelse( c_star < u, 0, delta)
  output <- Surv(t_star, delta_star)
  return(output)
}