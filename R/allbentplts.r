allbentplts = function(){
#
#  This programs gets examples of the four types of bent scores
#  used in Rfit.  Stores them for plots dataplts.  It also
#  runs a simple regre example and brings back a table
#  of coefficients and tauhats.   Default parameters are used.
#

     u = (1:100)/101
     dataplts = cbind(u)
     tab = matrix(rep(0,3*4),ncol=3)
# Bentscore 4
     mybentscores = bentscores4
     mybentscores@param = c(.25,.75,-1.0,1.0)
     vert = getScores(mybentscores,u)
     dataplts = cbind(dataplts,vert)
#     fit = rfit(y~x,scores=mybentscores)
#     tab[4,] = c(fit$coef,fit$tauhat)
# Bentscore 1
     mybentscores = bentscores1
     mybentscores@param = c(0.75,-2,1)
     vert = getScores(mybentscores,u)
     dataplts = cbind(dataplts,vert)
#     fit = rfit(y~x,scores=mybentscores)
#     tab[1,] = c(fit$coef,fit$tauhat)
# Bentscore 3
     mybentscores = bentscores3
     mybentscores@param = c(.5,-1.0,2.0)
     vert = getScores(mybentscores,u)
     dataplts = cbind(dataplts,vert)
#     fit = rfit(y~x,scores=mybentscores)
#     tab[3,] = c(fit$coef,fit$tauhat)
# Bentscore 2
     mybentscores = bentscores2
     mybentscores@param = c(.25,.75,-1.0,1.0,0.0)
     vert = getScores(mybentscores,u)
     dataplts = cbind(dataplts,vert)
#     fit = rfit(y~x,scores=mybentscores)
#     tab[2,] = c(fit$coef,fit$tauhat)
     allbentplts = dataplts
     allbentplts
}
