\name{turtle}
\alias{turtle}
\docType{data}
\title{
Turtle Data
}
\description{
	A data frame containg measurements of 48 turtles.
	The first three columns are the Length, Width, and Height measurements 
	of the carapace of the turtle.  The fourth column is a categorical variable sex
	with values of female and male.
	Data are drawn from Johnson and Wichern (2007).
}
\usage{data(turtle)}
\format{
  48 observations on four variables.
  \describe{
  \item{Length}{numeric vector.}
  \item{Width}{numeric vector.}
  \item{Height}{numeric vector.}
  \item{sex}{character vector.}
  }
}
\references{
	Johnson, R.A. and Wichern, D.W. (2007), \emph{Applied Multivariate Statistical Analysis, 6th ed.}, Upper Saddle River, NJ: Pearson.
}
\examples{
with(turtle,boxplot(Length~sex))
with(turtle,boxplot(Length~sex,ylab='Length (units)'))
}
\keyword{datasets}
