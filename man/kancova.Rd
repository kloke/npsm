\name{kancova}
\alias{kancova}
\title{ Robust Analysis of Covariance under Heterogeneous Slopes for a k-way layout}
\description{
Returns a robust rank-based analysis of covariance for a k-way layout assuming
heterogenous slopes; see Section 5.4 of Kloke and McKean (2014)/Sections 5.6 and 7.3 of Kloke and McKean (2024).
Currently only wilcoxon scores are used.
}
\usage{
kancova(levs,data,xcov,print.table=TRUE)
}
\arguments{
  \item{levs}{vector of levels corresponding to the factors A, B, C, etc.}
  \item{data}{matrix with response in column 1 and level in column 2 }
   \item{xcov}{ matrix of covariates}
   \item{print.table}{logical indicating a table should be printed}
}
\details{
Returns the analysis of covariance table assuming heterogenous slopes for a k-way layout.
}
\value{
  \item{tab2}{analysis of covariance }
  \item{fint}{rank-based ful model (heterogenous slopes}
  \item{fithomog}{rank-based ful model (homogeneous slopes}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.
Kloke, J. and McKean, J.W. (2024), \emph{Nonparametric statistcal methods using R, Second Edition}, Boca Raton, FL: Chapman-Hall.
}
\author{ John Kloke, Joseph McKean}
\examples{
 levels <- c(2,2)
 y.group <- huitema496[,c('y','i','j')]
 xcov <- huitema496[,'x']
 kancova(levels,y.group,xcov)

}
