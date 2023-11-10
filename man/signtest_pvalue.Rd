\name{signtest_pvalue}
\alias{signtest_pvalue}
\title{ p-value for a one sample sign test }
\description{
p-value for a one sample sign test based on the binomial distribution.
}
\usage{
signtest_pvalue(x, alternative = "two.sided", theta0 = 0, ...)
}
\arguments{
  \item{x}{number vector.}
  \item{alternative}{type of alternative hypothesis}
  \item{theta0}{null value of the parameter}
  \item{\dots}{optional arguments.  currently ignored.}
}
\details{
Returns p-value using the binomial distribution.
}
\value{
a numeric scalar --- the p-value --- is returned
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\author{John Kloke, Joseph McKean}

\examples{
x <- round(rt(19,9) + 2,1)
signtest_pvalue(x,alternative='greater')
S <- sum(x > 0)
M <- sum(x != 0)
1-pbinom(S-1,M,0.5)
x <- round(rt(19,9) + 0,1)
signtest_pvalue(x)
S <- sum(x > 0)
M <- sum(x != 0)
2*min(pbinom(S,M,0.5), 1-pbinom(S-1,M,0.5))
}

\keyword{nonparametric}

