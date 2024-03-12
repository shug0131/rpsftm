#'Function used to print of the underlying test object at the point estimate of a rpsftm object
#'
#'@export
#'@title Print Method
#'@name print.rpsftm
#' @param x an object returned from the \code{\link{rpsftm}} function.
#' @param ... further arguments passed to or from other methods.
#' @return a print of the underlying test object at the point estimate.
#' @author Simon Bond
#' 


print.rpsftm <- function(x,...) {
  print(x$rand)
  #NextMethod(generic="print", object=x, ...=...)
  #searches in the global environment first so doesn't use the print.* defined below.
  #whereas this looks in the environment that defines the function for the method print.
  #manually setting the "next" method using a copy of the object
  y<- x
  class(y)[1] <- paste0("rpsftm.",class(y)[2])
  if( !is.null(y$fail)){
    cat("Fitting failed." , y$fail,"\n")
    return()
  } else {
    print(y)
  }
  
  cat("\npsi:", x$psi)
  cat("\nexp(psi):", exp(x$psi),"\n")
  invisible(x)
}

#'modified version of print.coxph 
#'
#'this drops the "arm" term as this is not a real parameter
#'
#'
#'@name print.rpsftm.coxph
#' @param x an coxph()
#' @param digits the number of digits to print out
#' @param ... further arguments 
#' @keywords internal

print.rpsftm.coxph <- function (x, digits = max(options()$digits - 4, 3), ...){
    if (!is.null(x$fail)) {
      cat(" Coxph failed.", x$fail, "\n")
      return()
    }
    savedig <- options(digits = digits)
    on.exit(options(savedig))
    coef <- x$coefficients
    arm_index <- which(names(coef) == ".arm")
    coef <- coef[-arm_index, drop=FALSE]
    se <- sqrt(diag(x$var[-arm_index, -arm_index, drop=FALSE]))
    if (is.null(coef) | is.null(se)) 
      stop("Input is not valid")
    if (is.null(x$naive.var)) {
      tmp <- cbind(coef, exp(coef), se, coef/se, 1 - stats::pchisq((coef/se)^2, 
                                                            1))
      dimnames(tmp) <- list(names(coef), c("coef", "exp(coef)", 
                                           "se(coef)", "z", "p"))
    }
    else {
      nse <- sqrt(diag(x$naive.var[-arm_index, -arm_index, drop=FALSE]))
      tmp <- cbind(coef, exp(coef), nse, se, coef/se, 1 - stats::pchisq((coef/se)^2, 
                                                                 1))
      dimnames(tmp) <- list(names(coef), c("coef", "exp(coef)", 
                                           "se(coef)", "robust se", "z", "p"))
    }
    if(length(coef)>0){ 
      stats::printCoefmat(tmp, signif.stars = FALSE, P.values = TRUE, 
                 has.Pvalue = TRUE)}
    omit <- x$na.action
    cat("n=", x$n)
    if (!is.null(x$nevent)) cat(", number of events=", x$nevent)
    cat("\n")
    if (length(omit)) 
      cat("   (", stats::naprint(omit), ")\n", sep = "")
    invisible(x)
}

#'modified version of print.survreg
#'
#'this drops the "arm" term as this is not a real parameter
#'
#'@name print.rpsftm.survreg
#' @param x a survreg() object
#' @param ... further arguments 
#' @keywords internal

print.rpsftm.survreg <-function (x, ...) 
{
  
  if (!is.null(x$fail)) {
    cat(" Survreg failed.", x$fail, "\n")
    return(invisible(x))
  }
  coef <- x$coefficients
  arm_index <- which(names(coef) == ".arm")
  coef <- coef[-arm_index, drop=FALSE]
  cat("\nCoefficients:\n")
  print(coef, ...)
  if ( (nrow(x$var) -1 ) == length(coef)) 
    cat("\nScale fixed at", format(x$scale), "\n")
  else if (length(x$scale) ==  1) 
    cat("\nScale=", format(x$scale), "\n")
  else {
    cat("\nScale:\n")
    print(x$scale, ...)
  }
  nobs <- length(x$linear)
  omit <- x$na.action
  if (length(omit)) 
    cat("n=", nobs, " (", stats::naprint(omit), ")\n", sep = "")
  else cat("n=", nobs, "\n")
  invisible(x)
} 

#' print method for rand() objects - to display the summary of rx, by arm
#'
#'@title Print method
#'@export
#'@name print.rand
#'@param x a rand() object
#'@param ... further arguments passed to or from other methods.
#'@return a summary of rx values broken down by arm for a rand() object
#'@seealso \code{\link{rand}}, \code{\link{rpsftm}}
#'@author Simon Bond


print.rand <- function(x,...){
  print( stats::aggregate(rx~arm, data=x, summary) )
  invisible(x)
}

