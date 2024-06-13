\name{weather}
\alias{weather}
\docType{data}
\title{ January Weather Data for Kalamazoo}
\description{
January weather data for Kalamazoo, MI  for the years 1900 to 1995.
It is discussed in Example 4.7.4, page 105-106, of Kloke and McKean (2014)/Example 4.6.4, p.177-178, of Kloke and McKean (2024).
}
\usage{data(weather)}
\format{
  Ninety-six observations (1900-1995) for twelve weather variables.
  \describe{
    \item{\code{avemax}}{avemax}
    \item{\code{avemin}}{avemin}
    \item{\code{coldestmax}}{coldestmax}
    \item{\code{hihest}}{hihest}
    \item{\code{lowest}}{lowest}
    \item{\code{maxdayprec}}{maxdayprec}
    \item{\code{maxdaysnowfall}}{maxdaysnowfall}
    \item{\code{meantmp}}{meantmp}
    \item{\code{totalprec}}{totalprec}
    \item{\code{totalsnow}}{totalsnow}
    \item{\code{warmest}}{warmest}
    \item{\code{year}}{year}
  }
}
\source{http://weather-warehouse.com/WeatherHistory/}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
Kloke, J. and McKean, J.W. (2024), \emph{Nonparametric statistical methods using R, Second Edition}, Boca Raton, FL: Chapman-Hall.
}
\examples{
plot(avemax ~ year,data=weather)
}
\keyword{datasets}
