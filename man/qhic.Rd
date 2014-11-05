\name{qhic}
\alias{qhic}
\docType{data}
\title{
}
\description{
}
\usage{data("qhic")}
\format{
  A data frame with 40 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{x is value (in $1000) of a home}
    \item{\code{y}}{y is annual upkeep expenditure (in $10)}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Bowerman, BL,  O'Connell, RT, Koehler, AB (2005) \emph{Forecasting, time series, and regression:An applied approach}, Austalia: Thomson.
}
\examples{
data(qhic)
plot(y~x,data=qhic,xlab='Value (in $1000s)',ylab='Annual upkeep (in $10s)')
}
\keyword{datasets}
