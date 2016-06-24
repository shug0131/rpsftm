#'Function used to summarise the fitted model to an rpsftm object
#'
#'@export
#'@title summary Method
#'@name summary.rpsftm
#' @param x an object returned from the \code{rpsftm()} function
#' @return a summary of the fitted regression model
#' @author Simon Bond
#' 


summary.rpsftm <- function(x) {
  obj <- x$regression
  # remove the call object without this it will print the entire data set
  obj$call <- NULL
  obj.summary <- summary(obj)
  print(x$call)
  print(obj.summary)
  cat("\npsi:", x$psi)
  cat("\nexp(psi):", exp(x$psi))
  cat("\nConfidence Interval, psi", x$CI)
  cat("\nConfidence Interval, exp(psi) ", exp(x$CI))
  # Have not added stuff to the obj.summary- the return object about psi,
  # and CI it does vary between the different tests so would be hard to
  # generalise.
  invisible(obj.summary)
}

