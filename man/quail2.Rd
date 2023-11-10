\name{quail2}
\alias{quail2}
\docType{data}
\title{
Quail from a two-factor experiment.
}
\description{
Two sample quail data.
}
\usage{data(quail2)}
\format{
  A data frame with 30 observations on the following 2 variables.
  \describe{
    \item{\code{treat}}{indicator variable for treatment}
    \item{\code{ldl}}{ldl measurement}
  }
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

McKean J.W., Vidmar, T.J., and Sievers, G.L. (1989), A robust two stage multiple comparison procedure with application to a random drug screen, \emph{Biometrics}, 45, 1281--1297.
}
\examples{
data(quail2)
boxplot(ldl~treat,data=quail2)
}
\keyword{datasets}
