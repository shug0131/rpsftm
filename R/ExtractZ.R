#'Generic S3 methods to extract the z-statistic from a set of survival fit objects
#'
#'@title Extracting the z-statistic from a survival object
#'
#'@param a fitted survival object : survdiff, coxph, survreg
#'@return a numeric value, the z statistic for the independence test of the treatment arm
#'@author Simon Bond


## NEEDS an extra argument giving the "Arm" variable, to cope with adjustors.

ExtractZ=function(x){UseMethod("ExtractZ")}

#' @describeIn ExtractZ Method for survdiff


ExtractZ.survdiff=function(fit){
print(fit)
  #print(fit$obs)
#print(fit$exp)
#print(fit$var)
  if (is.matrix(fit$obs)) {
    otmp <- apply(fit$obs, 1, sum)
    etmp <- apply(fit$exp, 1, sum)
  }
  else {
    otmp <- fit$obs
    etmp <- fit$exp
  }
  df <- (etmp > 0)
  if (sum(df) < 2) 
    z <- 0
  else {
    temp2 <- ((otmp - etmp)[df])[-1]
    vv <- (fit$var[df, df])[-1, -1, drop = FALSE]
    #chi <- sum(solve(vv, temp2) * temp2)
    z<- temp2/sqrt(vv)
  }
  print(z)
  z
}


#' @describeIn ExtractZ Method for coxph objects

ExtractZ.coxph=function(fit){
 stop("Error method not yet defined") 
}


#' @describeIn ExtractZ Method for survreg objects

ExtractZ.survreg=function(fit){
  stop("Error method not yet defined") 
}