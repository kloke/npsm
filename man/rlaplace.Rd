\name{rlaplace}
\alias{rlaplace}
\title{
Random Laplace.
}
\description{
Random generation for the Laplace (double exponential) data with location 0 and scale 1.
}
\usage{
rlaplace(n)
}
\arguments{
  \item{n}{scalar.  number of random draws.}
}
\details{
A Laplace or double expoential distribution has heavier tails than a normal distribution and so a sample will tend to have additional outliers.
}
\value{
A vector of length n is returned containing the random data.
}
\references{
Hogg, Robert V.; McKean, Joseph; and Craig, Allen T., "Introduction to Mathematical Statistics (6th Edition)" (2005).
}
\author{
John Kloke, Joseph McKean
}
\examples{
x <- rlaplace(100)
qqnorm(x)
}
