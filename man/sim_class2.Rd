\name{sim_class2}
\alias{sim_class2}
\docType{data}
\title{
A simulated classification example with two variables and two classes (labels).
}
\description{
A simulated classification example with two variables and two classes (labels).
}
\usage{data("sim_class2")}
\format{
  A data frame with 1000 observations on the following 4 variables.
  \describe{
    \item{\code{train}}{an indicator for training and test sets}
    \item{\code{x1}}{an explantory variable}
    \item{\code{x2}}{an explantory variable}
    \item{\code{y}}{response variable - a factor with levels \code{0} \code{1}}
  }
}
\details{
Random points in the x1,x2 plane were generated. 
Class labels based on location relative to two circles in the x1,x2 plane with some random variation in the labels simulated.
}
\examples{
data(sim_class2)
dim(sim_class2)

train_set <- sim_class2[sim_class2$train==1,]
dim(train_set)

with(train_set,plot(x1,x2,main='Training Set',cex=0.625))
with(train_set,points(x1,x2,main='Training Set',pch=20,col=y,cex=0.625))


}
\keyword{datasets}
