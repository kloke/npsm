\name{simon}
\alias{simon}
\docType{data}
\title{Simon (the memory game) dataset}
\description{
An experiment in which the members of two groups of students each played the game Simon twice.
}
\usage{data("simon")}
\format{
  A data frame with 31 observations on the following 3 variables.
  \describe{
    \item{\code{game1}}{score on first trial}
    \item{\code{game2}}{score on second trial}
    \item{\code{class}}{group variable}
  }
}
\details{
Demonstrates the concept of regression toward the mean.
Simulated data to represent a realistic realization of the experiment.
See Problem 4.9.20 of Kloke and McKean (2014)/Problem 4.7.17 of Kloke and McKean (2024).
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.
Kloke, J. and McKean, J.W. (2024), \emph{Nonparametric statistcal methods using R, Second Edition}, Boca Raton, FL: Chapman-Hall.
}
\examples{
data(simon)
plot(game2~game1,data=simon)
rfit(game2~game1,data=simon)
}
\keyword{datasets}
