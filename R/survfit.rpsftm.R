#' Function to apply survfit method to rpsftm objects
#' 
#' @title survfit() method for rpsftm objects
#' @name survfit.rpsftm
#' @export
#' @inheritParams summary.rpsftm
#' @return a survfit object.
#' @seealso \code{\link[survival]{survfit}}
#' @author Simon Bond
#' @importFrom survival survfit
# need to import the original generic. else it won't be abel to find it in tests


survfit.rpsftm <- function(object, ...){
  if(class(object)[2] != "coxph"){
    stop(paste( "No applicable method for 'survfit' applied to an object of class", class(object)[2],"\n")
    )
  }else{
    coxfit <- attr( object$ans$f.root, "fit")
    survival::survfit(coxfit, ...=...)
  }
  
}

