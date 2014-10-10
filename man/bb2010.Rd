\name{bb2010}
\alias{bb2010}
\docType{data}
\title{
Batting statistics for the 2010 baseball season.
}
\description{
Batting (average, home runs, RBIs) statistics for 2010 full time players.
By full time we mean that
the batter had at least 450 official at bats during the season.
}
\usage{data(bb2010)}
\format{
  A data frame with 122 observations on the following 3 variables.
  \describe{
    \item{\code{ave}}{batting average}
    \item{\code{hr}}{home runs}
    \item{\code{rbi}}{runs batted in}
  }
}
\source{ baseballguru.com }
\examples{
plot(hr~ave,data=bb2010)
}
\keyword{datasets}
