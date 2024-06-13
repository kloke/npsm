\name{fkk.test}
\alias{fkk.test}
\title{ k-Sample version of the Fligner-Kileen test for homogeneous scales.}
\description{
Returns the Fligner-Kileen test for homogeneous scales for k-samples.
Also estimates of ratio of scales based on the logs of folded median-aligned 
samples and a corresponding confidence interval is computed.  The first level (sample) is referenced.
See the discussion in Section 5.7 of  Kloke and McKean (2014)/Section 5.8 of Kloke and McKean (2024).
}
\usage{
fkk.test(y,ind,conf.level = 0.95)
}
\arguments{
  \item{y}{ vector of responses }
  \item{ind}{ vector of corresponding levels }
  \item{conf.level}{ confidence coefficient for the returned confidence intervals }
}
\details{
Returns the Fligner-Kileen test for the k-sample scale problem.
}
\value{
  \item{statistic}{chi-squared test statistic}
  \item{p.value}{p-value of the test}
  \item{estimate}{vector of estimates of ratio of scales}
  \item{conf.int}{table of confidence intervals}
  \item{cwts}{vector of weights based on the estimates difference in scales}
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.
Kloke, J. and McKean, J.W. (2024), \emph{Nonparametric statistcal methods using R, Second Edition}, Boca Raton, FL: Chapman-Hall.

}
\author{ John Kloke, Joseph McKean}

\seealso{
fk.test

}
\examples{
y1 <- rnorm(10)
y2 <- rnorm(12)*3
y3 <- rnorm(15)*5
y<-c(y1,y2,y3)
ind<-rep(1:3,times=c(10,12,15))
fkk.test(y,ind)
}
