\name{HoggsQs}
\alias{HoggsQs}
\alias{Q1}
\alias{Q2}
\title{Hogg's Q1 and Q2.}
\description{ Q1 is a measure of skewness and Q2 is a measure of tail heaviness.  }
\usage{
Q1(z)
}
\arguments{
  \item{z}{n by 1 vector }
}
\details{
Used as selector statistics in adaptive schemes.
Both Q1 and Q2 are ratios.
For Q1, the numerator is upper 5\% mean minus the middle 50\% mean, while
the denominator is difference between the middle 5\% mean and the lower 5\% mean.
For Q2, the numerator is upper 5\% mean minus the lower 5\% mean, while
the denominator is difference between the upper 50\% mean and the lower 50\% mean.
These statistics are not robust.
}
\value{
Returns the calculated ratio as a numeric scalar.
}
\references{
Hogg, R. McKean, J, Craig, A (2013) \emph{Introduction to Mathematical Statistics, 7th Ed.} Boston: Pearson.
}
\author{John Kloke}

\seealso{
\code{\link{hogg.test}}
}
\keyword{nonparametrics}
