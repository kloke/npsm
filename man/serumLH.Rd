\name{serumLH}
\alias{serumLH}
\docType{data}
\title{Serum LH data}
\description{
Hollander and Wolfe (1999) discuss a 2 x 5 factorial design for a study to determine the effect of light on the relase of luteinizing hormone (LH).
The factors in the design are: light regimes at two levels and a luteinizing release factor (LRF) at 5 different dosage levels.
}
\usage{data("serumLH")}
\format{
  A data frame with 60 observations on the following 3 variables.
  \describe{
    \item{\code{serum}}{numeric response variable}
    \item{\code{light}}{a factor with levels \code{1} \code{2}}
    \item{\code{lrfdose}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5}}
  }
}
\references{
Hollander, M. and Wolfe, D.A. (1999), \emph{Nonparametric Statistical Methods}, New York: Wiley.
}
\examples{
raov(serum~light+lrfdose+light*lrfdose,data=serumLH)
}
\keyword{datasets}
