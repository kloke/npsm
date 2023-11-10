\name{insulation}
\alias{insulation}
\docType{data}
\title{Insulating Fluid Data}
\description{
Study the breakdown time of an electrical insulating fluid subject to seven different levels of voltage stress.
}
\usage{data("insulation")}
\format{
  A data frame with 76 observations on the following 2 variables.
  \describe{
    \item{\code{log.stress}}{log of voltage stress }
    \item{\code{log.time}}{log of failure time}
  }
}
\source{
	Nelson, W. (1982), \emph{Applied lifetime data analysis}, New York: John Wiley and Sons.

	Lawless, J.F. (1982), \emph{Statistical models and methods for lifetime data}, New York: John Wiley and Sons.
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.
}
\examples{
myscores <- logGFscores
myscores@param <- c(1,5)
fit <- rfit(log.time ~ log.stress,scores=myscores,data=insulation)
summary(fit)
fit$tauhat
}
\keyword{datasets}
