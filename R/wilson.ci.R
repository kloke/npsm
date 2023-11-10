wilson.ci <- function(x,n,conf.level=0.95) {
# Wilson (Score) CI for p (one sample prop)

# x integer number of sucesses
# n integer number of failures
# conf.level numeric between (0.5,1) representing the desired confidence

# Reference Agresti (2002) Categorical Data Analysis p.16

if( any( (conf.level >= 1), (conf.level <= 0.5) ) ) stop("Incorrect Argument(s)")

phat <- x/n

zcv <- qnorm( (1-conf.level)/2 ,lower.tail=FALSE)
zcv2 <- zcv*zcv

D <- 1/(n+zcv2)

A <- x*D
B <- 0.5*zcv2*D

est <- A+B
err <- zcv*sqrt( D*( (1-phat)*A + 0.5*B) ) 

conf.int <- est + c(-1,1)*err

attr(conf.int,'conf.level') <- conf.level

conf.int

}

