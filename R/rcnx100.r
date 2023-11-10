rcnx100<-function(n,eps=0.001,x=100,mu=0,sigma=1,...) {
  ind <- sample(c(FALSE,TRUE),n,prob=c(1-eps,eps),replace=TRUE)
  r <- rnorm(n,mu,sigma)
  r[ind] <- x*r[ind]
  r
}

rcnx <- function(...) rcnx100(...)

rcnx_01_100 <- function(n) rcnx(n,eps=0.001,x=100)

