\name{cloud}
\alias{cloud}
\docType{data}
\title{
Cloud Dewpoint
}
\description{
A regression example with response cloud point of a liquid and predictor the percent of
Iodine 8 added to the liquid; see Chapter 3 of Hettmansperger and McKean (2011) or
Exercise 4.9.10 of Kloke and McKean (2014)/Exercise 4.7.7 of Kloke and McKean (2024).
}
\usage{data(cloud)}
\format{
  Nineteen observations on two variables.
  \describe{
    \item{\code{cloud.point}}{Cloud point of the liquid}
    \item{\code{I8}}{Percent Iodine 8 added}
  }
}
\source{
	Draper, N.R. and Smith, H. (1966), \emph{Applied Regression Analysis}, New York: John Wiley and Sons.
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods Using R}, Boca Raton, FL: Chapman-Hall.
Kloke, J. and McKean, J.W. (2024), \emph{Nonparametric statistical methods Using R, Second Edition}, Boca Raton, FL: Chapman-Hall.
}
\examples{
rfit(cloud.point ~ I8,data=cloud)
}
\keyword{datasets}
