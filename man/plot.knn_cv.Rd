\name{plot.knn_cv}
\alias{plot.knn_cv}
\title{
plot function for knn_cv
}
\description{
plots the misclassification error rate versus number of neighbors based on call to knn_cv
}
\usage{
\method{plot}{knn_cv}(x, ...)
}
\arguments{
  \item{x}{
object of class knn_cv.
}
  \item{\dots}{
additional arguments.  currently not used.
}
}
\details{
The list x is assumed to have attributes kvec and error representing the number of neighbors and the corresponding misclassification rate, respectively. 
}
\value{
No return value, called for side effects of creating plot.
}
\references{
Hastie, T., Tibshiani, R., and Friedman, J. (2017), The Elements of Statistical Learning: Data Mining, Inference, and Prediction, Second Edition, New York: Springer.

James, G., Witten, D., Hastie, T., and Tibshirani, R. (2013), An Introduction to Statistical Learning with Applications in R, New York: Springer.

Venables, W. N. and Ripley, B. D. (2002) _Modern Applied Statistics with S._ Fourth edition.  Springer.
}
\author{
John Kloke
}

\seealso{
\code{\link{knn_cv}}
}
