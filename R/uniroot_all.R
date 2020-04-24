#'The function uniroot_all searches the interval from lower to upper for several roots  of a function f with respect to its first argument.
#'
#'@title Finds many (all) roots of one equation within an interval
#'@name uniroot_all
#'
#'@param f the function for which the root is sought. This function needs to accept a vector of  input values.
#'@param interval a vector containing the end-points of the interval to be searched for the root.
#'@param lower the lower end point of the interval to be searched.
#'@param upper the upper end poitn of the interval to be searched.
#'@param tol the desired accuracy (convergence tolerance). Passed to function \code{\link{uniroot}}.
#'@param trace integer numberl if positive, tracing information is produced. Higher values giving more details. Passed to function \code{\link{uniroot}}.
#'@param n number of subintervals in which the root is sought.
#'@param ... additional named or unnamed arguments to be passed to \code{f} (but beware of parital matching to other arguments).
#'
#'@details this is a copy taken from the package \code{rootSolve} but imported here to avoid dependency
#'
#'@return a vector with the roots found in the interval.
#'@seealso \code{\link{uniroot}}
#'@author Karline Soetaert <karline.soetaert@nioz.nl> - original . Copied by Simon Bond.
#'@importFrom stats uniroot
#'@keywords internal


uniroot_all <- function (f, interval, lower = min(interval), upper = max(interval), 
          tol = .Machine$double.eps^0.2, maxiter = 1000, trace = 0, 
          n = 100, ...) 
{
  if (!missing(interval) && length(interval) != 2) 
    stop("'interval' must be a vector of length 2")
  if (!is.numeric(lower) || !is.numeric(upper) || lower >= 
      upper) 
    stop("lower < upper  is not fulfilled")
  xseq <- seq(lower, upper, len = n + 1)
  mod <- f(xseq, ...)
  Equi <- xseq[which(mod == 0)]
  ss <- mod[1:n] * mod[2:(n + 1)]
  ii <- which(ss < 0)
  for (i in ii) Equi <- c(Equi, uniroot(f, lower = xseq[i], 
                                        upper = xseq[i + 1], maxiter = maxiter, tol = tol, trace = trace, 
                                        ...)$root)
  return(Equi)
}
