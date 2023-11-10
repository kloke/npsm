\name{hodges_lehmann.ci}
\alias{hodges_lehmann.ci}
\alias{hl.ci}
\title{
Hodges-Lehmann type estimation and confidence intervals.
}
\description{
Hodges-Lehmann type estimation and confidence intervals.
}
\usage{
hodges_lehmann.ci(x, y, var.equal = FALSE, conf.level = 0.95, ...)
}
\arguments{
  \item{x}{numeric vector.}
  \item{y}{numeric vector.}
  \item{var.equal}{logical. Assume scales are equal (TRUE) of not (FALSE).}
  \item{conf.level}{confidence level to be used for the confidence interval.}
  \item{\dots}{optional arguments.  currently unused.}
}
\details{
Currently implements 2-sample estimation and confidence intervals based on methods purposed by Hodges and Lehnmann.
}
\value{
  \item{estimate}{parameter point estimate}
  \item{stderr}{estimated standard error of point estimate}
  \item{conf.int}{estimated confidence interval} 
}
\references{
Hollander, M. and Wolfe, D.A. (1999), \emph{Nonparametric Statistical Methods}, New York: Wiley.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}

\author{ John Kloke, Joseph McKean}


\seealso{
\code{\link{wilcox.test}}
}
\examples{
zoo<-c(390,258,298,255,324,240,416,319,225,284)
rh <- c(187,186,179,269,382,264,353 ,38,350,267,229,383,254,302,195, 43,337,390)
hodges_lehmann.ci(zoo,rh)
}
\keyword{nonparametric}
