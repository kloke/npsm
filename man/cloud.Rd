\name{cloud}
\alias{cloud}
\docType{data}
\title{
Cloud Dewpoint
}
\description{
A regression example with response cloud point of a liquid and predictor the percent of
Iodine 8 added to the liquid; see Chapter 3 of Hettmansperger and McKean (2011) or
Exercise 4.9.10 of Kloke and McKean (2014).
}
\usage{data(cloud)}
\format{
  Nineteen observations on two variables.
  \describe{
    \item{\code{cloud.point}}{Cloud point of the liquid}
    \item{\code{I8}}{Percent Iodine 8 added}
  }
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
rfit(cloud.point ~ I8,data=cloud)
}
\keyword{datasets}
