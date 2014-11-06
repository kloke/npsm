\name{SCUD}
\alias{SCUD}
\docType{data}
\title{
Cyclone Data
}
\description{
A data set discussed in Hollander and Wolfe (1999) and Exercise 5.8.9 of Kloke and McKean (2014).
It contains part of a study on the effects of cloud seeding of cyclones.
}
\usage{data(SCUD)}
\format{
  Twenty-one observations on three variables.
  \describe{
    \item{\code{trt}}{treatment indicator (1) is Seeded and (2) is control}
    \item{\code{M}}{predictor M, the geostrophic meridional circulation index}
    \item{\code{RI}}{measure of precipitation}
  }
}
\references{
Hollander, M. and Wolfe, D.A. (1999), \emph{Nonparametric Statistical Methods}, New York: Wiley.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
plot(RI ~ M,data=SCUD)
}
\keyword{datasets}
