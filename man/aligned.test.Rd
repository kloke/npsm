\name{aligned.test}
\alias{aligned.test}
\title{
Aligned Rank Test
}
\description{Aligned rank test for a group/treatment effect after adjusting for covariates.}
\usage{
aligned.test(x, y, g, scores = wscores)
}
\arguments{
  \item{x}{n by p design matrix} 
  \item{y}{n by 1 response vector}
  \item{g}{n by 1 vector denoting group/treatment membership.}
  \item{scores}{
Which scores should be used for the fit and the test.
An object of class scores.
}
}
\value{
\item{statistic}{The value of the test statistic.}
\item{p.value}{The p-value based on a chisq(k-1) distribution where k is the number of groups/treatments. }
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.
}
\author{John Kloke \email{kloke@biostat.wisc.edu}}

\examples{
y<-rt(30,2)
x<-runif(30)
g<-rep(1:3,each=10)
aligned.test(x,y,g)
}
\keyword{nonparametrics}
