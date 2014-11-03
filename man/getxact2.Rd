\name{getxact2}
\alias{getxact2}
\title{ Design Function for Robust Analysis of Covariance}
\description{
Returns the hetrogeneous slopes design matrix used in ANCOVA. It  refereences the first level.
Also, column names are supplied.
}
\usage{
getxact2(amat,bmat)
}
\arguments{
  \item{amat}{cell mean design matrix of factor.}
  \item{bmat}{matrix of covariates.}
}
\details{
Returns the heterogeneous slopes analysis of covariance matrix.
}
\value{
  \item{cmat}{heterogeneous slopes analysis of covariance matrix eith columns named}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.
}
