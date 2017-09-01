#' Function to apply residual method to rpsftm objects
#' 
#' @title residual() method for rpsftm objects
#' @name residuals.rpsftm
#' @export
#' @inheritParams summary.rpsftm
#' @return a residuals object.
#' @seealso \code{\link[stats]{residuals}} \code{\link[survival]{residuals.coxph}}  \code{\link[survival]{residuals.survreg}}
#' @author Simon Bond
#' @importFrom stats residuals


residuals.rpsftm <- function(object, ...){
    object <- attr( object$ans$f.root, "fit")
    residuals(object, ...=...)
}
