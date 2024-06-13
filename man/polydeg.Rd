\name{polydeg}
\alias{polydeg}
\title{ Degree of Polynomial Determination}
\description{
Tests for the degree of a polnomial.
This test was suggested by Graybill (1976) and is discussed from a robust
point-of-view in Section 4.7.1. of Kloke and McKean (2014)/4.6.1 of Kloke and McKean (2024).
}
\usage{
polydeg(y, x, P, alpha = 0.05) 
}
\arguments{
  \item{y}{  vector of responses }
  \item{x}{   Predictor }
   \item{P}{ Super degree of polynomial which provides a satisfactory fit }
   \item{alpha}{ Level of the testing }
}
\details{
Returns the degree of the polynomial based on the algorithm.
}
\value{
  \item{deg}{The determined degree }
  \item{coll}{Matrix of step information}
  \item{fitf}{Fit of the polynomial based on the determoned degreer}
}
\references{
Graybill, F.A. (1976), \emph{Theory and application of the linear model},
North Scituate, Ma: Duxbury Press.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.
Kloke, J. and McKean, J.W. (2024), \emph{Nonparametric statistcal methods using R, Second Edition}, Boca Raton, FL: Chapman-Hall.

}
\examples{
 x <- 1:20
 xc <- x - mean(x)
 y<- .2*xc + xc^3 +rt(20,3)*90
 plot(y~x)
 polydeg(y,xc,6)

}
