\name{energy}
\alias{energy}
\docType{data}
\title{Energy dataset}
\description{
}
\usage{data("energy")}
\format{
  A data frame with 24 observations on the following 2 variables.
  \describe{
    \item{\code{output}}{Energy output in watts}
    \item{\code{temp.diff}}{Temperature difference in K}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Devore, J (2012), \emph{Probability and statistics for engineering and the sciences}, Boston:Brooks/Cole, 8th ed.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(energy)
plot(output~temp.diff,data=energy,xlab='Output (watts)',ylab='Temperature difference (K)')
}
\keyword{datasets}
