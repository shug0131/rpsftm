\name{Recensor}
\alias{Recensor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Recensor(phi, Time, CensorTime, Rx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phi}{
%%     ~~Describe \code{phi} here~~
}
  \item{Time}{
%%     ~~Describe \code{Time} here~~
}
  \item{CensorTime}{
%%     ~~Describe \code{CensorTime} here~~
}
  \item{Rx}{
%%     ~~Describe \code{Rx} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (phi, Time, CensorTime, Rx) 
{
    require(survival)
    if (any(!(0 <= Rx & Rx <= 1))) {
        stop("Invalid values for Rx. Must be proportions in [0,1]")
    }
    if (any(CensorTime < Time)) {
        warning("You have observed events AFTER censoring. These are handled as censored")
    }
    U = Time * ((1 - Rx) + Rx * exp(phi))
    Cstar = pmin(CensorTime, CensorTime * exp(phi))
    Tstar = pmin(U, Cstar)
    deltaStar = 1 * (U < Cstar)
    Surv(Tstar, deltaStar)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
