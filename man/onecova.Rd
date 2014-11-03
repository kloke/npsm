\name{onecova}
\alias{onecova}
\title{ Robust Analysis of Covariance under Heterogeneous Slopes}
\description{
Returns tests for homogeneous slopes and also assuming homogeneous slopes a test for
differences in level.
Currently only wilcoxon scores are used.
}
\usage{
onecova(levs,data,xcov,print.table=TRUE)
}
\arguments{
  \item{levs}{Number of levels of the one-way design}
  \item{data}{matrix with response in column 1 and level in column 2 }
   \item{xcov}{ matrix of covariates}
   \item{print.table}{logical indicating a table should be printed}
}
\details{
Returns the analysis of covariance table.
}
\value{
  \item{tab}{analysis of covariance }
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
data=latour[,c('quality','rain')]
xcov<-cbind(latour['end.of.harvest'])
onecova(2,data,xcov,print.table=TRUE)
}
