\name{wilson.ci}
\alias{wilson.ci}
\title{
Wilson (score) confidence interval for a population proportion.
}
\description{
Wilson (score) confidence interval for a population proportion.
}
\usage{
wilson.ci(x, n, conf.level = 0.95)
}
\arguments{
  \item{x}{number of events}
  \item{n}{number of samples}
  \item{conf.level}{confidence level}
}
\details{
Uses defintion in Agresti.
}
\value{
  \item{conf.int}{estimated confidence interval}
}
\references{
Agresti (2002), Categorical data analysis, New York: John Wiley & Sons, Inc.
}

\author{ John Kloke, Joseph McKean}
\seealso{
\code{\link{prop.test}}
}

\examples{
n <- 100
x <- rbinom(1,n,0.33)
wilson.ci(n,x)
}

