#'Function used to plot the KM curves of the treatment-free transformed times
#'
#'@export
#'@title Print Method
#'@name print.rpsftm
#' @param x an object returned from the \code{rpsftm()} function
#' @return a print of the underlying test object at the point estimate
#' @author Simon Bond
#' 


print.rpsftm=function(x){
  obj=x$regression
  #remove the call object
  obj$call<-NULL
  #print(obj$regression)
  print(obj)
  cat("\nphi:" , x$phi)
  cat("\nExp(phi)", exp(x$phi))
  invisible(x)
}



