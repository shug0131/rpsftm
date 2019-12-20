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

untreated <- function(psi, response,treatment_matrix, rand_var, censor_time, autoswitch) {
  
  time <- response[,"time"]
  delta <- response[,"status"]
  treatment_matrix <- treatment_matrix[, colnames(treatment_matrix)!="(Intercept)", drop=FALSE]
  
  
  
  # assumes and is user dependent! that the treatment matrix rowSums to a max of 1 
  nontreatment <- 1-rowSums(treatment_matrix) #apply(treatment_matrix,1,sum)
  
  #print(head(treatment_matrix))
  #print(head(psi))
  
  treatment <- rowSums(treatment_matrix*exp(psi))
  
  u <- time * (nontreatment + treatment)
  
  
  
  #psi is a matrix, row - patient, cols for treatments (which go from 0 to 1)
  #works out which treatment, or none, gives the smallest possible transformed censor time.
  
  # From an earlier version with no autoswitch... But simpler to understand
  #max_psi <- apply(cbind(1, exp(psi)),1, min)
  #c_star <- censor_time* max_psi 
  
    treatment_var <- rep(1, nrow(psi))
    treatment_fix <- rep(0, nrow(psi))
    trans_ratio_var <- rep(1, nrow(psi))
  #these are not the rand_matrixes you are looking for... Unhelpfully they include the intercept...
    #print(dim(treatment_matrix))
    #print(length(rand_var))
  
    
      for( col in 1:ncol(treatment_matrix)){
     
      treatment_sd <- aggregate( treatment_matrix[,col], by=list(rand_var), FUN=sd)
      fixed_arms <- treatment_sd[ treatment_sd[,2]==0,1]# column 2 is the variance, column 1 the arm
      if(!autoswitch){ fixed_arms <- fixed_arms[0]}
      
      
     treatment_var <- treatment_var- ifelse( rand_var %in% fixed_arms, treatment_matrix[,col],0)
    treatment_fix <- treatment_fix+ifelse( rand_var %in% fixed_arms, treatment_matrix[,col]*exp(psi[,col]),0)
      trans_ratio_var <- pmin( ifelse( rand_var %in% fixed_arms, 1, exp(psi[,col])),trans_ratio_var) 
      
  }
  
  c_star <- censor_time*(treatment_fix+trans_ratio_var*treatment_var)
  
  if( any(is.infinite(c_star))){
    t_star <- ifelse(u<c_star,u, c_star)
  } else{
  t_star <-  (u < c_star)*(u-c_star) + c_star  
  # doesn't work if c_star=Inf , so we've set 1000*max_time as the fall-back for infinity 
  # coding efficiency prefers this line to ifelse.
  }
  #only change delta if necessary
  delta_star <-  (u < c_star)*delta #ifelse( c_star < u, 0, delta)
  output <- cbind("time"=t_star, "status"=delta_star)
  attr(output, "type")="right"
  class(output) <- "Surv"
  return(output)
}