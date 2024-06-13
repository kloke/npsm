\name{qhic}
\alias{qhic}
\docType{data}
\title{qhic}
\description{
A regression example with response yearly upkeep of a home and the predictor value of home; see Bowerman et al.  (2005) and Exercise 4.9.8 of  Kloke and McKean (2014)/Exercise 7.6.2 of Kloke and McKean (2024).
}
\usage{data(qhic)}
\format{
  Forty observations on two variables.
  \describe{
    \item{\code{upkeep}}{annual upkeep expenditure of home (y)}
    \item{\code{value}}{value of the home (x)}
  }
}
\references{
Bowerman, B.L., O'Connell, R.T., and Koehler, A.B. (2005), \emph{Forecasting, time series, and regression: An applied approach},
Australia: Thomson.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
Kloke, J. and McKean, J.W. (2024), \emph{Nonparametric statistical methods using R, Second Edition}, Boca Raton, FL: Chapman-Hall.
}
\examples{
plot(upkeep~value,data=qhic,xlab='Value (in $1000s)',ylab='Annual upkeep (in $10s)')
}

\keyword{datasets}
