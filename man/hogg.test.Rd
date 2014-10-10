\name{hogg.test}
\alias{hogg.test}
\title{ Hogg's Adaptive Test }
\description{
Based on selector statistics (Q1 & Q2) one of four score functions is choosen.
A rank test and p-value is then calculated based on it.
}
\usage{
hogg.test(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{n by 1 vector }
  \item{y}{m by 1 vector }
  \item{\dots}{ additional arguments.  currently not used }
}
\value{
\item{statistic}{Value of the test statistic.}
\item{p.value}{p-value based on a normal approximation.}
\item{scores}{Which of the score functions was choosen.}
}
\references{
Hogg, R. McKean, J, Craig, A (2013) \emph{Introduction to Mathematical Statistics, 7th Ed.} Boston: Pearson.
}
\author{John Kloke \email{kloke@biostat.wisc.edu}, Patrick Kimes}
\examples{
hogg.test(rt(20,1),rt(22,1)+0.2)
}
\keyword{nonparametrics}
