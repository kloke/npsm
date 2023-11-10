\name{jonckheere}
\alias{jonckheere}
\title{ Jonckheere's Test for Ordered Alternatives}
\description{
Computes Jonckheere's Test for Ordered Alternatives; see Section 5.6 of Kloke and McKean (2014).
}
\usage{
jonckheere(y, groups)
}
\arguments{
  \item{y}{vector of responses}
  \item{groups}{vector of associated groups (levels)}
}
\details{
Computes Jonckheere's Test for Ordered Alternatives.
The main source was downloaded from the site:

smtp.biostat.wustl.edu/sympa/biostat/arc/s-news/2000-10/msg00126.html
}
\value{
  \item{Jonckheere}{test statistic}
  \item{ExpJ}{null expectation}
  \item{VarJ}{null variance}
  \item{p}{p-value}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.

smtp.biostat.wustl.edu/sympa/biostat/arc/s-news/2000-10/msg00126.html

}

\author{ John Kloke, Joseph McKean}

\examples{
 r<-rnorm(30)
 gp<-c(rep(1,10),rep(2,10),rep(3,10))
jonckheere(r,gp)

}
