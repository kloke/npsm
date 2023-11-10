\name{rcnx}
\alias{rcnx}
\alias{rcnx100}
\alias{rcnx_01_100}
\title{
Fat-Finger Error Contaminated Normal Deviates
}
\description{
Generate random data from a contaminated normal distribution
where the contaimation is a multiplicative factor.  As, for example,
in cases of data recorded in incorrect units or incorrect decimal point.
}
\usage{
rcnx100(n,eps=0.001,x=100,mu=0,sigma=1,...)
rcnx(...)
rcnx_01_100(n)
}
\arguments{
  \item{n}{sample size to be drawn.}
  \item{eps}{amount (probability) of contaminated observations} 
  \item{x}{multiplier for the contaminated observations}
  \item{mu}{mean of uncontaminated samples}
  \item{sigma}{standard deviation of uncontaminated samples}
  \item{\dots}{optional arguments.}
}
\details{
Samples are drawn from a random normal distribution with mean mu and standard deviations.
A fraction of the observations (eps) are multiplied by the factor x.
rcnx is an alias for rcnx100.
rcnx_01_100 is a special case where the observations are drawn from a standard normal distribution (i.e., mu=0 and sigma=1 --- the defaults in rcnx100) and eps and x are specified as 0.01 and 100, respectively.
}
\value{
Numeric vector of length n is returned.
}
\references{
https://en.wikipedia.org/wiki/Fat-finger_error
}
\author{
John Kloke
}

\seealso{
\code{\link{rcn}}
}
\examples{
set.seed(101); x1 <- rcnx100(10)
set.seed(101); x2 <- rcnx(10)
set.seed(101); x3 <- rcnx_01_100(10)

qqnorm(rcnx(10000,eps=0.005,x=10))
qqnorm(rcnx(1000,eps=0.05,x=1/100))
}
