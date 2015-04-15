\name{rpsftm}
\alias{rpsftm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rpsftm(Time, CensorTime, Rx, Arm, data, lowphi = -10, hiphi = 10, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{lowphi}{
%%     ~~Describe \code{lowphi} here~~
}
  \item{hiphi}{
%%     ~~Describe \code{hiphi} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (Time, CensorTime, Rx, Arm, data, lowphi = -10, hiphi = 10, 
    alpha = 0.05) 
{
    require(survival)
    if (!missing(data)) {
        Time <- data[, deparse(substitute(Time))]
        CensorTime <- data[, deparse(substitute(CensorTime))]
        Rx <- data[, deparse(substitute(Rx))]
        Arm <- data[, deparse(substitute(Arm))]
    }
    ans = uniroot(Test, c(lowphi, hiphi), Time = Time, CensorTime = CensorTime, 
        Rx = Rx, Arm = Arm, target = 0)
    lower = uniroot(Test, c(lowphi, hiphi), Time = Time, CensorTime = CensorTime, 
        Rx = Rx, Arm = Arm, target = qnorm(1 - alpha/2))
    upper = uniroot(Test, c(lowphi, hiphi), Time = Time, CensorTime = CensorTime, 
        Rx = Rx, Arm = Arm, target = qnorm(alpha/2))
    phiHat = ans$root
    Sstar = Recensor(phiHat, Time, CensorTime, Rx)
    fit = survfit(Sstar ~ Arm)
    list(phi = phiHat, fit = fit, Sstar = Sstar, ans = ans, CI = c(lower$root, 
        upper$root), call = match.call())
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
