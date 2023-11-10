\name{huitema496}
\alias{huitema496}
\docType{data}
\title{
Analysis of Covarince Data Set
}
\description{
A data set presented on Page 496 of huitema (2011).   The design is a 2 by 2 with
one covariate.
}
\usage{data(huitema496)}
\format{
  A 16 by 4 array with the following 4 columns:
  \describe{
    \item{\code{y}}{number of novel responses.}
    \item{\code{i}}{type of reinforcement (2 levels).}
    \item{\code{j}}{type of program (2 levels).}
    \item{\code{x}}{covariate, a measure of verbal fluency.}
  }
}
\details{
Discussion can be found in both references listed below.
}
\source{
Huitema, B.E. (2011), \emph{The analysis of covariance and alternatives, 2nd ed.}, New York: Wiley.
}
\references{
Huitema, B.E. (2011), \emph{The analysis of covariance and alternatives, 2nd ed.}, New York: Wiley.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
huitema496 <- data.frame(huitema496)
fit <- rfit(y~factor(i)+factor(j)+x,data=huitema496)
summary(fit)
}

\keyword{datasets}
