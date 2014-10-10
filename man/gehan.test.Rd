\name{gehan.test}
\alias{gehan.test}
\title{Gehan generalization the Wilcoxon two-sample test}
\description{Generalization of the Wilcoxon rank sum which allows for censored data.}
\usage{ gehan.test(time, event, trt) }
\arguments{
  \item{time}{Time of event or of censoring}
  \item{event}{Indicator variable representing a event occur or not (time is censored)}
  \item{trt}{Variable indicating treatment group.}
}
\value{
\item{statistic}{Value of the test statistic}
\item{p.value}{p-value}
}
\references{
Higgins (2004), \emph{Introduction to Modern Nonparametric Statistics}, 
Pacific Grove, CA:Brooks/Cole--Thomson Learning
}

\author{John Kloke \email{kloke@biostat.wisc.edu}}

\examples{
n<-76
y<-rexp(n)
event<-rbinom(n,1,0.7) # about 30%  censored
trt<-sample(c(0,1),n,replace=TRUE)
gehan.test(y,event,trt)
}
\keyword{nonparametrics}
\keyword{survival}
