knn_cv <- function(xy,k.cv=5,kvec=seq(1,47,by=2)) {

  cvmat <- matrix(nrow=k.cv,ncol=length(kvec))

  folds <- sample(cut(1:nrow(xy), breaks=k.cv, labels=FALSE))

  for(i in 1:k.cv) {

    x1 <- as.matrix(xy[folds!=i,-ncol(xy)])
    x0 <- as.matrix(xy[folds==i,-ncol(xy)])

    y1 <- xy[folds!=i,ncol(xy)]
    y0 <- xy[folds==i,ncol(xy)]

    for(j in 1:length(kvec) ) {
      cvmat[i,j] <- mean(knn(x1,x0,y1,k=kvec[j]) != y0)
    }

  }

  cv.error <- colMeans(cvmat)

  result <- list(kvec=kvec,error=cv.error,k.best=kvec[which.min(cv.error)],k.cv=k.cv)
  class(result) <- 'knn_cv'
  result

}

print.knn_cv <- function(x,...) { 

  cat("\n",paste0(x$k.cv,"-fold Cross Validation for kNN"),"\n")
  cat(paste0("Best k=", x$k.best, ", CV error rate=", x$error[x$kvec==x$k.best]), "\n")

}

plot.knn_cv <- function(x,...) {
  with(x,plot(kvec,error,main=paste0(k.cv,"-fold Cross Validation for kNN"),
    xlab='Number of Neigbors (k)', ylab='Misclassification Error Rate')
  )
}

