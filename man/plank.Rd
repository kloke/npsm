\name{plank}
\alias{plank}
\docType{data}
\title{Plank data}
\description{Abebe et al. (2001) discuss a dataset resulting from a three-way
layout for a neurological experiment in which the time
  required for a mouse to exit a narrow elevated wooden plank is
  measured. The response is the log of time (in seconds) to exit.
Interest lies in assessing the effects of three factors:
the Mouse Strain
(Tg+, Tg-),
 the mouse's Gender (female, male), and the mouse's Age (Aged, Middle, Young).
The design is a 2 by 2 by 3 factorial design.
}
\usage{data(plank)}
\format{
  A data frame with 64 observations on the following 4 variables.
  \describe{
    \item{\code{response}}{a numeric vector}
    \item{\code{strain}}{a factor with levels \code{1} \code{2}}
    \item{\code{gender}}{a factor with levels \code{1} \code{2}}
    \item{\code{age}}{a factor with levels \code{1} \code{2} \code{3}}
  }
}
\references{
Abebe, A., Crimin, K., McKean, J. W., Vidmar, T. J., and Haas, J. V. 
(2001) ``Rank-Based Procedures for Linear Models: Applications to Pharmaceutical
Science Data" \emph{Drug Information Journal}, 
}
\examples{
data(plank)
boxplot(response~strain,data=plank)
raov(response~strain:gender:age,data=plank)
}
\keyword{datasets}
