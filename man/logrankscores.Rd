\name{logrankscores}
\alias{logrankscores}
\docType{data}
\title{Log Rank Scores}
\description{ 
An object of class scores which includes the score function and it's derivative for rank-based regression inference.
}
\usage{data("logrankscores")}
\details{
Log rank scores are optimal when the underlying error distribution is exponential.
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.
}
\examples{
data(logrankscores)
e <- log(rexp(10))
x <- 1:10
y <- 4*x+e
rfit(y~x,scores=logrankscores)
}
\keyword{datasets}
