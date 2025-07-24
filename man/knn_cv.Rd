\name{knn_cv}
\alias{knn_cv}
\title{
Train a k nearest neighbors (knn) classifer via cross validation (cv).
}
\description{
Train a k nearest neighbors (knn) classifer via cross validation (cv).
The number of folds and the set of the number of neihbors to consider may be specified.
}
\usage{
knn_cv(xy, k.cv = 5, kvec = seq(1, 47, by = 2))
}
\arguments{
  \item{xy}{
Data frame with the data matrix x as the first set of columns and the vector y as the last column.
}
  \item{k.cv}{
scalar.  number of folds to use.  default is 5.
}
  \item{kvec}{
vector.  set of neighbors to consider.  default is odd integers between 1 and 47 (inclusive).
}
}
\value{
\item{kvec}{set of neighbors considered}
\item{error}{vector of misclassification error rates corresponding to kvec}
\item{k.best}{number of neighbors with lowest error rate}
\item{k.cv}{number of folds to used}
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
\code{\link[class]{knn}}
}
\examples{
train_set <- sim_class2[sim_class2$train==1,-1]
set.seed(19180511)
fit_cv <- knn_cv(train_set,k.cv=10)
fit_cv
}
