#' Applies the Recensoring method using a given parameter value and returns a Surv() object
#' 
#' @title Recensoring 
#' @name recensor
#' @param phi the parameter that measures how more rapidly the lifetime is expended under treatment
#' @param time the observed failure or censoring time.
#' @param censor_time the theoretical censoring time, either observed or set after time.
#' @param rx the proportion of time spent on treatment
#' @param data a data.frame object containing the variables  
#' @return A Surv() object with the recensoring applied
#' @author Simon Bond

recensor=function(phi,time,censor_time,rx, data){
  #Put in the data= argument and process.
  attach(data)
  if( any (!(0<=rx & rx<=1))){stop("Invalid values for rx. Must be proportions in [0,1]")}
  if( any(censor_time<time)){warning("You have observed events AFTER censoring. These are handled as censored")}
  U=time*((1-rx)+rx*exp(phi))
  Cstar=pmin(censor_time, censor_time*exp(phi))
  Tstar=pmin(U,Cstar)
  deltaStar=1*(U<Cstar)
  output=survival::Surv(Tstar,deltaStar)
  detach(data)
  retrun(output)
}