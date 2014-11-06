\name{energy}
\alias{energy}
\docType{data}
\title{
Energy as a Function of temperature difference.
}
\description{
A regression example with response energy output in watts and the predictor temperature difference in degrees Kevin; see Devore (2012) and Exercise 4.9.11 of  Kloke and McKean (2014).
}
\usage{data(energy)}
\format{
  Twenty-four observations on two variables.
  \describe{
    \item{\code{output}}{Energy output in watts}
    \item{\code{temp.diff}}{Temperature difference in K}
  }
}
\source{
Devore, J. (2012), \emph{Probaility and statistics for engineering and the sciences, 8th ed.}, Boston: Brooks/Cole.
}

\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
rfit(output ~ temp.diff,data=energy)
}
\keyword{datasets}
