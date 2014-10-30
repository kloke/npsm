\name{sievers}
\alias{sievers}
\docType{data}
\title{
Doksum and Sievers rat data
}
\description{
Doksum and Sievers (1976) describe an experiment involving
the effect of ozone on weight gain of rats.
The experimental group consisted of 22 rats which were placed
in an ozone environment for seven days, while the  control group
contained 21 rats which were placed in an ozone-free environment for the same
amount of time.
The response was the weight gain in a rat over the time period.

}
\usage{data(sievers)}
\format{
  A data frame with 45 observations on the following 2 variables.
  \describe{
    \item{\code{group}}{indicator variable for treatment}
    \item{\code{weight.gain}}{response variable of weight gain}
  }
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

Doksum, K. A. and Sievers, G. L. (1976), Plotting with confidence: Graphical comparisons of two populations, 
\emph{Biometrika}, 63, 421-434.
}
\examples{
data(sievers)
boxplot(weight.gain~group,data=sievers)
}
\keyword{datasets}
