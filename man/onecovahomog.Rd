\name{onecovahomog}
\alias{onecovahomog}
\title{ Robust Analysis of Covariance under Heterogeneous Slopes}
\description{
Returns a robust rank-based analysis of covariance for a one-way layout assuming
homogeneous slopes; see Section 5.4 of Kloke and McKean (2014).
Currently only wilcoxon scores are used.
}
\usage{
onecovaheter(levs,data,xcov,print.table=TRUE)
}
\arguments{
  \item{levs}{Number of levels of the one-way design}
  \item{data}{matrix with response in column 1 and level in column 2 }
   \item{xcov}{ matrix of covariates}
}
\details{
Returns the analysis of covariance table assuming homogeneous slopes.
}
\value{
  \item{tab}{analysis of covariance }
  \item{fit}{rank-based ful model (homogeneous slopes}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.

}
\examples{
data=latour[,c('quality','rain')]
xcov<-cbind(latour['end.of.harvest'])
onecovahomog(2,data,xcov,print.table=TRUE)

}
