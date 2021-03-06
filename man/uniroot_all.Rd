% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uniroot_all.R
\name{uniroot_all}
\alias{uniroot_all}
\title{Finds many (all) roots of one equation within an interval}
\usage{
uniroot_all(
  f,
  interval,
  lower = min(interval),
  upper = max(interval),
  tol = .Machine$double.eps^0.2,
  maxiter = 1000,
  trace = 0,
  n = 100,
  ...
)
}
\arguments{
\item{f}{the function for which the root is sought. This function needs to accept a vector of  input values.}

\item{interval}{a vector containing the end-points of the interval to be searched for the root.}

\item{lower}{the lower end point of the interval to be searched.}

\item{upper}{the upper end poitn of the interval to be searched.}

\item{tol}{the desired accuracy (convergence tolerance). Passed to function \code{\link{uniroot}}.}

\item{trace}{integer numberl if positive, tracing information is produced. Higher values giving more details. Passed to function \code{\link{uniroot}}.}

\item{n}{number of subintervals in which the root is sought.}

\item{...}{additional named or unnamed arguments to be passed to \code{f} (but beware of parital matching to other arguments).}
}
\value{
a vector with the roots found in the interval.
}
\description{
The function uniroot_all searches the interval from lower to upper for several roots  of a function f with respect to its first argument.
}
\details{
this is a copy taken from the package \code{rootSolve} but imported here to avoid dependency
}
\seealso{
\code{\link{uniroot}}
}
\author{
Karline Soetaert <karline.soetaert@nioz.nl> - original . Copied by Simon Bond.
}
\keyword{internal}
