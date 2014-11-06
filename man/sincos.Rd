\name{sincos}
\alias{sincos}
\docType{data}
\title{Sine Cosine Model}
\description{
Simulated dataset 
}
\usage{data("sincos")}
\format{
  A data frame with 197 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{independent variable}
    \item{\code{y}}{dependent variable}
  }
}
\details{
The data were generated using 
\code{x <- seq(1,50,by=.25) ; y <- 5*sin(3*x) + 6*cos(x/4)+rnorm(length(x),0,10)
}
}
\examples{
data(sincos)
plot(y~x,sincos)
}
\keyword{datasets}
