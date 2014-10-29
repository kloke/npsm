vanElteren.test<-function(g,y,b) {

# g is group (assumes 0/1)
# y is response vector
# b is strata/block 

d<-cbind.data.frame(y,g,b)

res<-ddply(d,.(b),summarize,
	statistic=sum(rank(y)*(g==1)),
	m=sum(g==0),
	n=sum(g==1)
)

weight<-with(res,1/(m+n+1))
W<-with(res,sum(weight*statistic))
mu<-0.5*with(res,sum(n))
sigma<-sqrt(with(res,sum(m*n*weight))/12)

	stat<-(W-mu)/sigma
	list(statistic=stat,p.value=2*pnorm(abs(stat),lower.tail=FALSE))

}
