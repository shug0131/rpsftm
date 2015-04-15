\name{Test}
\alias{Test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Test(phi, Time, CensorTime, Rx, Arm, target = 0)
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
  \item{Arm}{
%%     ~~Describe \code{Arm} here~~
}
  \item{target}{
%%     ~~Describe \code{target} here~~
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
function (phi, Time, CensorTime, Rx, Arm, target = 0) 
{
    require(survival)
    Sstar = Recensor(phi, Time, CensorTime, Rx)
    if (length(unique(Arm)) != 2) {
        stop("Arm must have exactly 2 observed values")
    }
    fit = survdiff(Sstar ~ Arm)
    z = (fit$exp - fit$obs)/sqrt(diag(fit$var))
    fit$chisq
    z[1] - target
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
