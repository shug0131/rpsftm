#'Function used to summarise the fitted model to an rpsftm object
#'
#'@export
#'@title summary Method
#'@name summary.rpsftm
#' @param object an object returned from the \code{rpsftm()} function.
#' @param ... further arguments passed to or from other methods.
#' @return a summary of the fitted regression model.
#' @author Simon Bond
#' 


summary.rpsftm <- function(object,...) {
  y <- object
  class(y) <- class(object)[2]
  obj.summary <- summary(y,...=...)
  new_class <- sub("summary","rpsftm.summary", class(obj.summary))
  class(obj.summary) <- c(new_class, class(obj.summary))
  print(object$rand)
  print(obj.summary)
  cat("\npsi:", object$psi)
  cat("\nexp(psi):", exp(object$psi))
  cat("\nConfidence Interval, psi", object$CI)
  cat("\nConfidence Interval, exp(psi) ", exp(object$CI),"\n")
  # Have not added stuff to the obj.summary- the return object about psi,
  # and CI it does vary between the different tests so would be hard to
  # generalise.
  invisible(obj.summary)
}

#'modified version of print.summary.coxph 
#'
#'this drops the "arm" term as this is not a real parameter
#'
#'
#'@name print.rpsftm.summary.coxph
#' @param x the result of a call to \code{summary.coxph}
#' @param digits significant digits to print
#' @param signif.stars  show star to highlight small p-values
#' @param ... further arguments for future methods
#' @keywords internal
#' 

print.rpsftm.summary.coxph <- function (x, 
                                 digits = max(getOption("digits") - 3, 3), 
                                 signif.stars = getOption("show.signif.stars"), 
                                 ...) {
 
  if (!is.null(x$fail)) {
    cat(" Coxreg failed.", x$fail, "\n")
    return()
  }
  savedig <- options(digits = digits)
  on.exit(options(savedig))
  omit <- x$na.action
  cat("  n=", x$n)
  if (!is.null(x$nevent)) cat(", number of events=", x$nevent)
  cat("\n")
  if (length(omit)) 
    cat("   (", stats::naprint(omit), ")\n", sep = "")
  arm_index <- which(rownames(x$coefficients)==".arm")
  if (!is.null(x$coefficients) & nrow(x$coefficients)>1) {
    cat("\n")
    stats::printCoefmat(x$coefficients[-arm_index, , drop=FALSE], 
                        digits = digits, signif.stars = signif.stars, ...)
  }
  if (!is.null(x$conf.int)& nrow(x$conf.int)>1) {
    cat("\n")
    print(x$conf.int[-arm_index, , drop=FALSE])
  }
  cat("\n")
  if (!is.null(x$concordance)) {
    cat("Concordance=", format(round(x$concordance[1], 3)), 
        " (se =", format(round(x$concordance[2], 3)), ")\n")
  }
   invisible()
}

#'modified version of print.summary.survreg
#'
#'this drops the "arm" term as this is not a real parameter
#'
#'
#'@name print.rpsftm.summary.survreg
#' @param x the result of a call to \code{summary.survreg}
#' @param digits significant digits to print
#' @param ... further arguments for future methods
#' @keywords internal
#' 

print.rpsftm.summary.survreg <- function (x, digits = max(options()$digits - 4, 3), ...) 
{
  correl <- x$correlation
  if (is.null(digits)) 
    digits <- options()$digits
  cat("\nCall:\n")
  dput(x$call)
  arm_index <- which(rownames(x$table)==".arm")
  print(x$table[-arm_index,, drop=FALSE], digits = digits)
  if (nrow(x$var) == length(x$coefficients)) 
    cat("\nScale fixed at", format(x$scale, digits = digits), 
        "\n")
  else if (length(x$scale) == 1) 
    cat("\nScale=", format(x$scale, digits = digits), "\n")
  else {
    cat("\nScale:\n")
    print(x$scale, digits = digits, ...)
  }
  cat("\n", x$parms, "\n", sep = "")
  df <- sum(x$df) - x$idf
  cat("Loglik(model)=", format(round(x$loglik[2], 1)), "  Loglik(intercept only)=", 
      format(round(x$loglik[1], 1)))
  
  cat("\nNumber of Newton-Raphson Iterations:", format(trunc(x$iter)), 
      "\n")
  omit <- x$na.action
  if (length(omit)) 
    cat("n=", x$n, " (", stats::naprint(omit), ")\n", sep = "")
  else cat("n=", x$n, "\n")
  if (!is.null(correl)) {
    correl <- correl[-arm_index,-arm_index, drop=FALSE]
    p <- dim(correl)[2]
    if (p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      ll <- lower.tri(correl)
      correl[ll] <- format(round(correl[ll], digits = digits))
      correl[!ll] <- ""
      print(correl[-1, -p, drop = FALSE], quote = FALSE)
    }
  }
  cat("\n")
  invisible(NULL)
}

