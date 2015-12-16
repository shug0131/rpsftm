#' Applies the Recensoring method using a given parameter value and returns a Surv() object
#' 
#' @title Recensoring 
#' @name recensor
#' @param phi the parameter that measures how more rapidly the lifetime is expended under treatment
#' @param time the observed failure or censoring time.
#' @param censor_time the theoretical censoring time, either observed or set after time.
#' @param rx the proportion of time spent on treatment
#' @param arm the randomised arm. Either a numerical indicator, with 0 as the placebo, or a factor with the lowest level as placebo.
#' @param Recensor a logical to use recensoring if set to TRUE. Default is TRUE.
#' @param Autoswitch a logical to autodetect cases of no switching. Default is TRUE
#' @return A Surv() object with the recensoring applied
#' @author Simon Bond
#' @importFrom survival Surv

recensor=function(phi,time,censor_time,rx,arm,Recensor, Autoswitch){
  if(is.numeric(arm) & any( !(arm %in% c(0,1)))){
    warning("Auto checking of no switching needs treatment to have value 0 or 1")
  }
  if(is.factor(arm)) {
    message <- paste("Auto checking of switching assumes the lowest level of arm,",
                     levels(arm)[1], "is the control or placebo treatment")
    warning(message)
    # converts the numerically coding (1,2,..), to 0 or 1.
    arm <- as.numeric(arm)-1
  }
  
  
  
  
  if( any (!(0<=rx & rx<=1))){stop("Invalid values for rx. Must be proportions in [0,1]")}
  if( any(censor_time<time)){warning("You have observed events AFTER censoring. These are handled as censored")}
  U=time*((1-rx)+rx*exp(phi))
  if(!Recensor){
    scenario <- 1
  }else{
    if( any(0<rx[arm==0]) & any(rx[arm==1]<1) ){scenario <- 2}
    if( any(0<rx[arm==0]) & all(rx[arm==1]==1)){scenario <- 3}
    if( any(rx[arm==1]<1) & all(rx[arm==0]==0)){scenario <- 4}
    #This is order-dependent logic, not ideal
    if( !Autoswitch){scenario <- 2}
  }
 
  Cstar <- switch(scenario,
                  censor_time,
                  pmin(censor_time, censor_time*exp(phi)),
                  arm*censor_time*exp(phi)+(1-arm)*pmin(censor_time, censor_time*exp(phi)),
                  arm*pmin(censor_time, censor_time*exp(phi))+(1-arm)*censor_time
  )
  Tstar=pmin(U,Cstar)
  deltaStar=1*(U<Cstar)
  output=Surv(Tstar,deltaStar)
  return(output)
}
  
  