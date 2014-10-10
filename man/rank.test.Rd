\name{rank.test}
\alias{rank.test}
\title{
General scores rank test for two sample problem
}
\description{
A generalization of the Wilcoxon rank-sum test where a score function
is applied to the ranks.
Any scores from Rfit can be used as well as user defined.
Default is to perform a Wilcoxon analysis.
}
\usage{
rank.test(x, y, alternative = "two.sided", scores = wscores, 
  conf.int = FALSE, conf.level = 0.95)
}
\arguments{
  \item{x}{m x 1 vector}
  \item{y}{n x 1 vector }
  \item{alternative}{one of 'two.sided', 'less', or 'greater' }
  \item{scores}{an object of class scores}
  \item{conf.int}{logical indicating if a confidence interval should be estimated}
  \item{conf.level}{desired level of confidence for interval}
}
\details{
Test is based on T = sum_i a(R(y_i)) where R is the rank based on the combined sample
and a(t) = varphi(t/(N+1)).
Confidence interval, if requested, is based on call to Rfit.  
}
\value{
\item{statistic}{Standardized value of test statistics}
\item{Sphi}{Test statistic}
\item{p.value}{p-value}
\item{conf.int}{confidence interval for shift in location}
\item{estimate}{point estimate for shift in location}
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.
}
\author{John Kloke \email{kloke@biostat.wisc.edu}}

\seealso{
\code{\link{wilcox.test}}
}
\examples{
rank.test(rt(20,1),rt(22,1)+0.2)
}
\keyword{nonparametrics}
