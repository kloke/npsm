\name{vanElteren.test}
\alias{vanElteren.test}
\title{
vanElteren test for stratified analysis
}
\description{
Performs the vanElteren extension of the Wilcoxon rank sum test
for stratified experiments.
}
\usage{
vanElteren.test(g, y, b)
}
\arguments{
  \item{g}{n x 1 vector: treatment/group indicator}
  \item{y}{n x 1 vector: responses}
  \item{b}{n x 1 vector: denotes strata }
}
\value{
\item{statistic}{Value of the test statistic.}
\item{p.value}{p-value based on a normal approximation.}
}

\keyword{nonparametrics}
