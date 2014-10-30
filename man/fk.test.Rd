\name{fk.test}
\alias{fk.test}
\title{ Two-sample Fligner-Kileen test for homogeneous scales.}
\description{
Returns the Fligner-Kileen test for homogeneous scales for two-samples.
Also estimates of ratio of scales based on the logs of folded median-aligned 
samples and a corresponding confidence interval is computed.  
fk.test computes the value of the statistic based on squared-normal scores
following the optimal (for normal errors) such test described in 
Section 2.10 of Hettmansperger and McKean (2011).
Hence, it will differ from the core R routine fligner.test;
see  the discussion in Section 3.3 of  Kloke and McKean (2014).
}
\usage{
fk.test(x,y,alternative = c("two.sided", "less", "greater"),conf.level = 0.95)
}
\arguments{
  \item{x}{ vector of first sample responses }
  \item{y}{ vector of second sample responses }
  \item{alternative}{ alternative indicator for hypotheses }
  \item{conf.level}{ confidence coefficient for the returned confidence intervals }
}
\details{
Returns the Fligner-Kileen test for the two-sample scale problem.
}
\value{
  \item{statistic}{chi-squared test statistic}
  \item{p.value}{p-value of the test}
  \item{estimate}{vector of estimates of ratio of scales}
  \item{conf.int}{table of confidence intervals}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.

Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.
}
\author{ John Kloke \email{kloke@biostat.wisc.edu}, Joseph McKean}

\seealso{ fkk.test }
\examples{
x<-rnorm(18)
y<-rnorm(22)*3
fk.test(x,y)
}
