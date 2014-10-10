\name{latour}
\alias{latour}
\docType{data}
\title{Chateau Latour Wine Data}
\description{
The response variable is the quality of a  vintage based on  a scale of 1 to 5 over the years
1961 to 2004.
The predictor is end of harvest, days between August 31st and the end of harvest
for that year, and the factor of interest is whether or not it rained at harvest time.
}
\usage{data(latour)}
\format{
  A data frame with 44 rows and 4 columns.
  \describe{
    \item{\code{year}}{Year of harvest}
	\item{\code{quality}}{Rating on a scale of 1-5}
	\item{\code{end.of.harvest}}{Days August 31 and the end of harvest}
	\item{\code{rain}}{indicator variable for rain}
  }
}
\references{
Sheather, SJ (2009), \emph{A Modern Approach to Regression with R},
New York: Springer.
}
\examples{
data(latour)
plot(quality~end.of.harvest,pch='',data=latour)
points(quality~end.of.harvest,data=latour[latour$rain==0,],pch=3)
points(quality~end.of.harvest,data=latour[latour$rain==1,],pch=4)
}
\keyword{datasets}
