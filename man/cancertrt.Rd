\name{cancertrt}
\alias{cancertrt}
\docType{data}
\title{
Survival time based on two treatments
}
\description{
Survival times (in days) for undergoing standard treatment (S) 
and a new treatment (N).
}
\usage{data("cancertrt")}
\format{
  A data frame with 17 observations on the following 3 variables.
  \describe{
    \item{\code{time}}{Survival time in days}
    \item{\code{event}}{Indicator for event}
    \item{\code{trt}}{a factor with levels \code{N} \code{S}}
  }
}
\references{
Higgins (2004), \emph{Introduction to Modern Nonparametric Statistics},
Pacific Grove, CA:Brooks/Cole--Thomson Learning
}
\examples{
data(cancertrt)
with(cancertrt,gehan.test(time,event,trt))
}
\keyword{datasets}
