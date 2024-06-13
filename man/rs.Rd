\name{rs}
\alias{rs}
\docType{data}
\title{
Simulated Regression Model
}
\description{
A simulated regression model with one response and one predictor.
It is discussed in Exercise 6.5.6 of Kloke and McKean (2014)/Exercise 8.11.23 of Kloke and McKean (2024).
}
\usage{data(rs)}
\format{
  Fifty observations on two variables.
  \describe{
    \item{\code{y}}{simulated response}
    \item{\code{x}}{simulated predictor}
  }
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
Kloke, J. and McKean, J.W. (2024), \emph{Nonparametric statistical methods using R, Second Edition}, Boca Raton, FL: Chapman-Hall.
}
\examples{
rfit(y ~ x,data=rs)
}
\keyword{datasets}
