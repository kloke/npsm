\name{print}
\alias{print.hogg.test}
\alias{print.rank.test}
\alias{print.fkk.test}
\alias{print.knn_cv}
\alias{print.npsm.ci}
\title{Internal print functions}
\description{Internal print functions} 
\usage{
\method{print}{hogg.test}(x, digits = max(5, .Options$digits - 2), ...)
\method{print}{rank.test}(x,...)
\method{print}{fkk.test}(x,...)
\method{print}{knn_cv}(x,...)
\method{print}{npsm.ci}(x, estimate=FALSE,stderr=FALSE,digits = max(5, .Options$digits - 2),...)
}
\arguments{
  \item{x}{Object to be printed.}
  \item{digits}{Number of digits to present.  Passed to print function.}
  \item{\dots}{ Additional arguments.}
  \item{estimate}{not currently implemented.}
  \item{stderr}{not currently implemented.}
}
\author{John Kloke, Joseph McKean}
