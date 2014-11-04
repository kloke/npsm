\name{speed}
\alias{speed}
\docType{data}
\title{Predict top speed based on miles per gallon}
\description{
A sample of 82 cars with variables speed and miles per gallon collected.
}

\usage{data("speed")}
\format{
  A data frame with 82 observations on the following 2 variables.
  \describe{
    \item{\code{mpg}}{Miles per gallon}
    \item{\code{sp}}{a numeric vector}
  }
}
\source{
Higgins (2003) Introduction to modern nonparmetric statistics.
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.
}

\examples{
data(speed)
plot(sp~mpg,data=speed)
rfit(sp~mpg+I(mpg^2),data=speed)
}
\keyword{datasets}
