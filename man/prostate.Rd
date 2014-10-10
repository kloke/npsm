\name{prostate}
\alias{prostate}
\docType{data}
\title{DES for treatment of prostate cancer.}
\description{
Under investigation in this clinical trial was the pharmaceutical agent diethylstilbestrol DES; subjects were assigned treatment to 1.0 mg DES (treatment = 2)  or to placebo (treatment = 1).
}
\usage{data(prostate)}
\format{
  A data frame with 38 observations on the following 8 variables.
  \describe{
    \item{\code{patient}}{a numeric vector}
    \item{\code{treatment}}{a numeric vector}
    \item{\code{time}}{a numeric vector}
    \item{\code{status}}{a numeric vector}
    \item{\code{age}}{a numeric vector}
    \item{\code{shb}}{a numeric vector}
    \item{\code{size}}{a numeric vector}
    \item{\code{index}}{a numeric vector}
  }
}
\source{http://www.crcpress.com/product/isbn/9781584883258}
\references{Collett, D. (2003)
\emph{Modeling survival data in medical research}
CRC press.

}
\examples{
data(prostate)
boxplot(size~treatment,data=prostate)
}
\keyword{datasets}
