\name{hodgkins}
\alias{hodgkins}
\docType{data}
\title{Relapse-Free Survival Times for Hodgkin's Disease Patients}
\description{
These data are described in Example~11.7 of Hollander and Wolfe (1999).
Results from a clinical trial in early Hodgkin's disease.
Subjects received one of two treatments:
radiation of affected node (AN) or total nodal radiation (TN).
}
\usage{data("hodgkins")}
\format{
  A data frame with 49 observations on the following 3 variables.
  \describe{
    \item{\code{time}}{Survival time}
    \item{\code{relapse}}{Indicator variable for relapse}
    \item{\code{trt}}{treatment: a factor with levels \code{AN} \code{TN}}
  }
}
\references{
Hollander, M. and Wolfe, D.A. (1999), \emph{Nonparametric Statistical Methods}, New York: Wiley.
}
\keyword{datasets,survival}
