\name{rcn}
\alias{rcn}
\alias{rcn_5_5}
\title{
random contaminated normal deviates
}
\description{
Generate a random sample from a contaminated normal distribution.
}
\usage{
rcn(n, eps, sigmac)
rcn_5_5(n)
}
\arguments{
  \item{n}{sample size}
  \item{eps}{proportion of proportion of contamination}
  \item{sigmac}{standard devation of contaiminated component}
}
\details{
With probability (1-eps) a deviates are drawn from a standard normal distribution.
With probability eps deviates are drawn from a normal distribution with
mean 0 and standard devation sigmac
rcn_5_5 is a special case where eps=0.05 and sigma=5.
}
\value{
n x 1 numeric vector containing the random deviates.
}
\references{
Hogg, R. McKean, J, Craig, A (2013) \emph{Introduction to Mathematical Statistics, 7th Ed.} Boston: Pearson.
}
\author{John Kloke, Joseph McKean}

\seealso{
\code{\link{rnorm}}
}
\examples{
qqnorm(rcn(100,.25,10))

set.seed(101); rcn(10,0.05,5)
set.seed(101); rcn_5_5(10)
}
