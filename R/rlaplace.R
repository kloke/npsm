rlaplace<-function(n) {

########################################
#
#  function: rlaplace - returns a random sample of size n 
#    from a laplace (double exponential) distribution 
#    with location 0 and scale 1
#
#  programmer: John Kloke
#
#  input:
#    n - sample size
#
#  output: a vector of length n 
#
#  reference: Hogg, McKean, and Craig (2005)
#
########################################

	ind <- rbinom(n,1,0.5) == 1
	x <- rexp(n)/2
	x[ind] <- -x[ind]

	return(x)

}


