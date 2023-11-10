rcn<-function(n,eps,sigmac) rnorm(n,0,sample(c(1,sigmac),n,replace=TRUE,prob=c(1-eps,eps)))

rcn_5_5 <- function(n) rcn(n,0.05,5)

