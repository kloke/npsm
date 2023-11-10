\name{acov231}
\alias{acov231}
\docType{data}
\title{
Analysis of Covariance Example for a two by three two-way design}
\description{
This a simulated data set which	is used as an example of analysis of
covariance.   The data frame acov231 contains the
data.  The responses are in column 1, column 2 contains the levels of
factor A, column 3 contains the levels of factor B, and the 4th column contains the covariate.
All true parameters (effects) are 0 in this generated data set.
}
\usage{data(acov231)}
\format{
A data frame with 33 observations and 4 variables.
\describe{
\item{\code{response}}{numeric.  the response.}
\item{\code{fA}}{numeric.  factor A with 2 levels.}
\item{\code{fB}}{numeric.  factor B with 3 levels.}
\item{\code{covariate}}{numeric.  a covariate.}
}
}
\references{
Kloke, J. and McKean J.W. (2014), \emph{Nonparametric Statistical Methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
levs = c(2,3)
data = acov231[,1:3]
xcov = matrix(acov231[,4],ncol=1)
temp = kancova(levs,data,xcov)
}
\keyword{datasets}
