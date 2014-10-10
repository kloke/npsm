\name{cor.boot.ci}
\alias{cor.boot.ci}
\title{ Confidence interval for a correlation based on a bootstrap.  }
\description{
Returns a bootstrap confidence interval for any of the correlations available in the base R 
\code{cor} function.
}
\usage{
cor.boot.ci(x, y, method = "spearman", conf = 0.95, nbs = 3000)
}
\arguments{
  \item{x}{ n by 1 vector }
  \item{y}{ n by 1 vector }
  \item{method}{Which correlation to use.  Argument passed to \code{cor}.  }
  \item{conf}{ Confidence level. }
  \item{nbs}{ number of bootstrap samples to base CI on. }
}
\details{
Obtains a percentile bootstrap confidence interval.
The bootstrap samples are obtained via the function \code{boot}.
}
\value{
A confidence interval.
}
\author{ John Kloke \email{kloke@biostat.wisc.edu}, Joseph McKean}

\seealso{
See Also as \code{\link{cor}}
}
\examples{
library(boot)
with(bb2010,cor.boot.ci(ave,hr))
}
