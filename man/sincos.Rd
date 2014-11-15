\name{sincos}
\alias{sincos}
\docType{data}
\title{Sine Cosine Model}
\description{
Simulated dataset 
}
\usage{data("sincos")}
\format{
  A data frame with 197 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{independent variable}
    \item{\code{y}}{dependent variable}
  }
}
\details{
The data were generated using 
\code{x <- seq(1,50,by=.25) ; y <- 5*sin(3*x) + 6*cos(x/4)+rnorm(length(x),0,10)
}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
data(sincos)
plot(y~x,sincos)

### code to create Figure 4.9 of Kloke & McKean 2014 ###
my.sincos<-sincos
my.sincos$y3<-my.sincos$y
my.sincos$y3[137] <- 800

plot(y3~x,ylim=c(-50,50),data=my.sincos)
fit4 <- loess(y3 ~ x,data=my.sincos)
# lines(fit4$x,fit4$fitted,lty=2)
with(fit4,lines(x,fitted,lty=2))
fit5 <- loess(y3 ~ x,family="symmetric",data=my.sincos)
with(fit5,lines(x,fitted,lty=1))
legend('bottomleft',legend=c('Local Robust Fit','Local LS Fit'),lty=1:2)
title("loess Fits of Sine-Cosine Data")
}
\keyword{datasets}
