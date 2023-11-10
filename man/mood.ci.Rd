\name{mood.ci}
\alias{mood.ci}
\title{
Mood Median Confidence Interval
}
\description{
Mood's classical nonparametric method for calculating a difference in population medians.
}
\usage{
mood.ci(x, y, var.equal = FALSE, conf.level = 0.95, ...)
}
\arguments{
  \item{x}{
n x 1 vector
}
  \item{y}{
m x 1 vector
}
  \item{var.equal}{
Logical. Assume scale of the two populations are equal.
}
  \item{conf.level}{
numeric value.  confidence level for the confidence interval.
}
  \item{\dots}{
not currently implmented
}
}
\value{
A vector of length 2 containing the lower and upper endpoints of the confidence interval.
}
\references{
Hollander, M. and Wolfe, D.A. (1999), \emph{Nonparametric Statistical Methods}, New York: Wiley.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\author{ John Kloke, Joseph McKean}

\seealso{
\code{\link{hl.ci}},\code{\link{wilcox.test}}
}
\examples{
x <- rt(101,9)
y <- rt(108,9)+0.3
mood.ci(x,y)
}
\keyword{nonparametric}

