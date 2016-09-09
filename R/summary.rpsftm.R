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
  obj <- object$regression
  # remove the call object without this it will print the entire data set
  obj$call <- NULL
  obj.summary <- summary(obj)
  print(object$call)
  print(obj.summary)
  cat("\npsi:", object$psi)
  cat("\nexp(psi):", exp(object$psi))
  cat("\nConfidence Interval, psi", object$CI)
  cat("\nConfidence Interval, exp(psi) ", exp(object$CI))
  # Have not added stuff to the obj.summary- the return object about psi,
  # and CI it does vary between the different tests so would be hard to
  # generalise.
  invisible(obj.summary)
}

