mood.ci <- function(x,y,var.equal=FALSE,conf.level=0.95,...) {
	ny <- length(y); nx <- length(x); df <- ny+nx-2
	thetahaty <- median(y)
	thetahatx <- median(x)
	deltahat <- thetahatx - thetahaty
	tauhaty <- taustar(y-thetahaty,0)
	tauhatx <- taustar(x-thetahatx,0)
	if(var.equal){
		tauhatp <- ((nx-1)*tauhatx^2 + (ny-1)*tauhaty^2)/df
		se <- sqrt(tauhatp)*sqrt((1/nx) + (1/ny))
	} else {

	        se <- sqrt((tauhaty^2/ny)+(tauhatx^2/nx))
	}

        tcv <- qt((1-conf.level)/2,df,lower.tail=FALSE)

	lb <- deltahat - tcv*se
	ub <- deltahat + tcv*se
	conf.int <- c(lb,ub)
        attr(conf.int,'conf.level') <- conf.level

	result <- list(estimate=deltahat,stderr=se,conf.int=conf.int)
        class(result) <- 'npsm.ci'
	result
}
