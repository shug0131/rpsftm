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
  y <- x
  class(y) <- class(y)[2]
  # manually setting the "next" method using a copy of the objecti
  print(y)
  #whereas this looks in the environment that defines the function for the method print.*
  cat("\npsi:", x$psi)
  cat("\nexp(psi):", exp(x$psi))
  invisible(x)
}

#'modified version of print.coxph 
#'
#'this drops the "arm" term as this is not a real parameter
#'
#'
#'@name print.coxph
#' @param x an coxph()
#' @param digits the number of digits to print out
#' @param ... further arguments 
#' @keywords internal

print.coxph <- function (x, digits = max(options()$digits - 4, 3), ...){
    if (!is.null(x$fail)) {
      cat(" Coxph failed.", x$fail, "\n")
      return()
    }
    savedig <- options(digits = digits)
    on.exit(options(savedig))
    coef <- x$coefficients
    arm_index <- which(names(coef)=="arm")
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
    #logtest <- -2 * (x$loglik[1] - x$loglik[2])
    #if (is.null(x$df)) 
    #  df <- sum(!is.na(coef))
    #else df <- round(sum(x$df), 2)
    #cat("\n")
    #cat("Likelihood ratio test=", format(round(logtest, 2)), 
    #    "  on ", df, " df,", " p=", format(1 - pchisq(logtest, 
    #                                                  df)), "\n", sep = "")
    omit <- x$na.action
    cat("n=", x$n)
    if (!is.null(x$nevent)) 
      cat(", number of events=", x$nevent, "\n")
    else cat("\n")
    if (length(omit)) 
      cat("   (", stats::naprint(omit), ")\n", sep = "")
    invisible(x)
}

#'modified version of print.survreg
#'
#'this drops the "arm" term as this is not a real parameter
#'
#'@name print.survreg
#' @param x a survreg() object
#' @param ... further arguments 
#' @keywords internal

print.survreg <-function (x, ...) 
{
  
  if (!is.null(x$fail)) {
    cat(" Survreg failed.", x$fail, "\n")
    return(invisible(x))
  }
  coef <- x$coefficients
  arm_index <- which(names(coef)=="arm")
  coef <- coef[-arm_index, drop=FALSE]
# if (any(nas <- is.na(coef))) {
#    if (is.null(names(coef))) 
#      names(coef) <- paste("b", 1:length(coef), sep = "")
#    cat("\nCoefficients: (", sum(nas), " not defined because of singularities)\n", 
#        sep = "")
#  }
#  else 
  cat("\nCoefficients:\n")
  print(coef, ...)
  if (nrow(x$var)-1 == length(coef)) 
    cat("\nScale fixed at", format(x$scale), "\n")
  else if (length(x$scale) == 1) 
    cat("\nScale=", format(x$scale), "\n")
  else {
    cat("\nScale:\n")
    print(x$scale, ...)
  }
  nobs <- length(x$linear)
  #chi <- 2 * diff(x$loglik)
  #df <- sum(x$df) - x$idf
  #cat("\nLoglik(model)=", format(round(x$loglik[2], 1)), "  Loglik(intercept only)=", 
  #    format(round(x$loglik[1], 1)))
  #if (df > 0) 
  #  cat("\n\tChisq=", format(round(chi, 2)), "on", round(df, 
  #                                                       1), "degrees of freedom, p=", format(signif(1 - pchisq(chi, 
  #                                                                                                            df), 2)), "\n")
  #else cat("\n")
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

