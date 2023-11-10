print.npsm.ci <- function(x,estimate=FALSE,stderr=FALSE,digits = max(5, .Options$digits - 2),...) {

  if( estimate ) 
    warning("estimate not currently implemented.")
  if( stderr ) 
    warning("stderr not currently implemented.")

  cat("\n",100*attr(x$conf.int,'conf.level')," percent confidence interval:\n",sep='')
  print(format(x$conf.int, digits = digits), quote = FALSE)

}
