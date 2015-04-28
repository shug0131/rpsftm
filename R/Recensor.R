#' Applies the Recensoring method using a given parameter value and returns a Surv() object
#' @export
#' @title Recensoring 
#' @name Recensor
#' @param phi the parameter that measures how more rapidly the lifetime is expended under treatment
#' @param Time the observed failure or censoring time.
#' @param CensorTime the theoretical censoring time, either observed or set after Time.
#' @param Rx the proportion of Time spent on treatment
#' @param data a data.frame object containing the variables  
#' @return A Surv() object with the recensoring applied
#' @author Simon Bond

Recensor=function(phi,Time,CensorTime,Rx, data){
  require(survival)
  #Put in the data= argument and process.
  
  if( any (!(0<=Rx & Rx<=1))){stop("Invalid values for Rx. Must be proportions in [0,1]")}
  if( any(CensorTime<Time)){warning("You have observed events AFTER censoring. These are handled as censored")}
  U=Time*((1-Rx)+Rx*exp(phi))
  Cstar=pmin(CensorTime, CensorTime*exp(phi))
  Tstar=pmin(U,Cstar)
  deltaStar=1*(U<Cstar)
  Surv(Tstar,deltaStar)
}