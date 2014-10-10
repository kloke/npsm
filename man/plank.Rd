\name{plank}
\alias{plank}
\docType{data}
\title{Plank data}
\description{Ask Joe}
\usage{data(plank)}
\format{
  A data frame with 64 observations on the following 4 variables.
  \describe{
    \item{\code{response}}{a numeric vector}
    \item{\code{strain}}{a factor with levels \code{1} \code{2}}
    \item{\code{gender}}{a factor with levels \code{1} \code{2}}
    \item{\code{age}}{a factor with levels \code{1} \code{2} \code{3}}
  }
}
\references{
Abebe, A., Crimin, K., McKean, J. W., Vidmar, T. J., and Haas, J. V. 
(2001) ``Rank-Based Procedures for Linear Models: Applications to Pharmaceutical
Science Data" \emph{Drug Information Journal}, 
}
\examples{
data(plank)
boxplot(response~strain,data=plank)
}
\keyword{datasets}
