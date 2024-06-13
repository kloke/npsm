\name{fp.test}
\alias{fp.test}
\title{ Placement Test for the Behrens-Fisher problem.}
\description{
Returns the test based on placements for the Behrens-Fisher problem.
This test was developed by Fligner and Policello (1981); see, also, Section 2.11
of Hettmansperger and McKean (2011) and Section 4.4 of Hollander and Wolfe (1999).
The version computed by fp.test is discussed in Section 3.4 of Kloke and McKean (2014)/Section 3.6 of Kloke and McKean (2024).
}
\usage{
fp.test(x,y,delta0=0,alternative = "two.sided")
}
\arguments{
  \item{x}{ vector of first sample responses }
  \item{y}{ vector of second sample responses }
  \item{delta0}{ null value tested }
  \item{alternative}{ alternative indicator for hypotheses }
}
\details{
Returns the Placement Test for the Behrens-Fisher problem.
}
\value{
  \item{statistic}{chi-squared test statistic}
  \item{p.value}{p-value of the test}
  \item{numerator}{numerator of test statistic}
  \item{denominator}{denominator of test statistic}
}
\references{
Fligner, M.~A. and Policello, G.~E. (1981), Robust rank procedures for the Behrens-Fisher problem,
\emph{Journal of the American Statistical Association}, 76, 162--168.

Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

Hollander, M. and Wolfe, D.~A. (1999), \emph{Nonparametric statistical methods, 2nd Edition},
New York: John Wiley and Sons.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistcal methods using R}, Boca Raton, FL: Chapman-Hall.
Kloke, J. and McKean, J.W. (2024), \emph{Nonparametric statistcal methods using R, Second Edition}, Boca Raton, FL: Chapman-Hall.


}
\author{ John Kloke, Joseph McKean}
