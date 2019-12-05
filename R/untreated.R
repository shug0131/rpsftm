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
  
  
  
  # assumes and is user dependent! that the treatment matrix rowSums to a max of 1 
  nontreatment <- 1-rowSums(treatment_matrix) #apply(treatment_matrix,1,sum)
  treatment <- rowSums(treatment_matrix*exp(psi))
  
  u <- time * (nontreatment + treatment)
  
  
  #make use of setting censor_time=Inf to avoid recensoring, and implimenting Autoswitch
  
  #psi is a matrix, row - patient, cols for treatments (which go from 0 to 1)
   # works out which treatment, or none, gives the smallest possible transformed censor time.
  max_psi <- apply(cbind(1, exp(psi)),1, min)
  c_star <- censor_time* max_psi #min(1, exp(psi) ) #apply(cbind( censor_time , censor_time %o% exp(psi)),1, min)
  if( autoswitch & ncol(treatment_matrix)==1){
    check <- aggregate( treatment_matrix[,1], by=list(rand_matrix[,-1]), FUN=sd)
    switch_values <- check[check[,2]==0,1]
    c_star <- ifelse( rand_matrix[,-1] %in% switch_values, Inf, c_star)# DOES have to be Inf to work. 
    #psi_star <- ifelse(rand_matrix[,-1] %in% switch_values, 0, psi )   
    
    #then I wan tot work this across all the columns of Psi, to do the multi-arm version....
    
    #if( all(rx[arm==1]==1)){ c_star <- ifelse(arm==1, Inf, c_star)}
    #if( all(rx[arm==0]==0)){ c_star <- ifelse(arm==0, Inf, c_star)}
    
    
  }
  
  # Doesn't work yet see line 42:43
  #max_psi <- apply(cbind(1, exp(psi_star)),1, min)
  #c_star <- censor_time* max_psi 
  
  t_star <-  pmin(u, c_star) # doesn't work if c_star=Inf  : (u < c_star)*(u-c_star) + c_star 
  #only change delta if necessary
  delta_star <-  (u < c_star)*delta #ifelse( c_star < u, 0, delta)
  output <- cbind("time"=t_star, "status"=delta_star)
  attr(output, "type")="right"
  class(output) <- "Surv"
  return(output)
}