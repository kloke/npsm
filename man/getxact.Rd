\name{getxact}
\alias{getxact}
\title{ Design Function for Robust Analysis of Covariance}
\description{
Returns the hetrogeneous slopes design matrix used in ANCOVA. It refereences the first level.
}
\usage{
getxact(amat,bmat)
}
\arguments{
  \item{amat}{cell mean design matrix of factor.}
  \item{bmat}{matrix of covariates.}
}
\details{
Returns the heterogeneous slopes analysis of covariance matrix.
}
\value{
  \item{cmat}{heterogeneous slopes analysis of covariance matrix}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.
}
