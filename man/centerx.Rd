\name{centerx}
\alias{centerx}
\title{Center Matrix}
\description{
Centers a matrix.
}
\usage{
centerx(x)
}
\arguments{
  \item{x}{ a matrix }
}
\details{
Returns a centered matrix, i.e., each column of the matrix is replaced by deviations from its column mean.
}
\value{
The centered matrix.
}
\author{ John Kloke, Joseph McKean}

\seealso{ scale }
\examples{
x <- cbind(seq(1,5,length=5),seq(10,20,length=5))
xc <- centerx(x)
apply(xc,1,mean)
}
