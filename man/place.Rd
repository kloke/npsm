\name{place}
\alias{place}
\title{ Placements.}
\description{
Returns the placements of the first vector in terms of the second vector used the R function fp.test;
 see Section 2.11
of Hettmansperger and McKean (2011) and Section 4.4 of Hollander and Wolfe (1999).
The version computed by fp.test is discussed in Section 4.4 of Kloke and McKean (2014).
}
\usage{
place(x,y)
}
\arguments{
  \item{x}{ first vector }
  \item{y}{ second vector of second sample responses }
}
\details{
Returns the Placements for the routine fp.test.
}
\value{
  \item{ic}{vector of placements.}
}
\references{

Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

Hollander, M. and Wolfe, D.~A. (1999), \emph{Nonparametric statistical methods, 2nd Edition},
New York: John Wiley \& Sons.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.

}
\author{ John Kloke \email{kloke@biostat.wisc.edu}, Joseph McKean}

