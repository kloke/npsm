polydeg <- function(y,x,P,alpha=0.05) {


  if( P < 1 ) stop("P > 1 is required")

  x <- x - mean(x)
  xmat <- matrix(nrow=length(y),ncol=P)

  for( i in 1:P ) xmat[,i] <- x^i


  result <- matrix(nrow=P,ncol=3)

  deg <- 0

  fitf <- rfit(y ~ xmat)

  for( i in 1:P ) {
    
    p <- P - i + 1
    if (p > 1) {
      xr <- xmat[, 1:(p-1)]
      fitr <- rfit(y ~ xr)
      tst <- drop.test(fitf, fitr)
    } else {
      tst <- drop.test(fitf)
    }

    result[i,] <- c(p, tst$F, tst$p.value)
    if( tst$p.value < alpha) {
      deg <- p
      break()
    }

    fitf <- fitr

  } 

  colnames(result) <- c("Deg", "Robust F", "p-value")

  list(coll=result[1:i,],deg=deg,fitf=fitf)

}

