#' Function to apply survfit method to rpsftm objects
#' 
#' @title survfit() method for rpsftm objects
#' @name survfit.rpsftm
#' @export
#' @param formula an object returned from the \code{rpsftm()} function. (with the name "formula" to agree with generic argument names)
#' @param ... further arguments passed to or from other methods.
#' @return a survfit object.
#' @seealso \code{\link[survival]{survfit}}
#' @author Simon Bond
#' @importFrom survival survfit
# need to import the original generic. else it won't be abel to find it in tests


survfit.rpsftm <- function(formula, ...){
  if(class(formula)[2] != "coxph"){
    stop(paste( "No applicable method for 'survfit' applied to an object of class", class(formula)[2],"\n")
    )
  }else{
    coxfit <- attr( formula$ans$f.root, "fit")
    survival::survfit(coxfit, ...=...)
  }
  
}

