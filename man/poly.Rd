\name{poly}
\alias{poly}
\docType{data}
\title{
A Simulated Polynomial Data Set.
}
\description{
A simulated polynomial (3rd degree) model discussed in Section 4.7.1 of Kloke and McKean (2014)/4.6.1 of Kloke and McKean (2024).
}
\usage{data(poly)}
\format{
  One-hundred observations on two variables.
  \describe{
    \item{\code{y}}{response variable}
    \item{\code{x}}{predictor}
  }
}
\references{

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
Kloke, J. and McKean, J.W. (2024), \emph{Nonparametric statistical methods using R, Second Edition}, Boca Raton, FL: Chapman-Hall.
}
\examples{
plot(y ~ x,data=poly)
}
\keyword{datasets}
