\name{seinfeld}
\alias{seinfeld}
\docType{data}
\title{
Seinfeld --- the sitcom --- viewership counts by episode
}
\description{
Counts of viewers for 9 seasons of Seinfeld
}
\usage{data("seinfeld")}
\format{
  A data frame with 180 observations on the following 4 variables.
  \describe{
    \item{\code{episodeNumberOverall}}{a numeric vector}
    \item{\code{season}}{a numeric vector}
    \item{\code{episodeNumberSeason}}{a numeric vector}
    \item{\code{viewers}}{a numeric vector}
  }
}
\source{
Wikipedia 
https://en.wikipedia.org/wiki/List_of_Seinfeld_episodes
(date unknown).
}
\examples{
data(seinfeld)
#Comparison boxplots of views versus season
boxplot(viewers~season,data=seinfeld,ylab='Number of Viewers (in millions)',xlab='Season')

# Normal q-q plots for selected seasons.
seasons2display <- c(4,5,6,9)
par(mfrow=c(2,2))
for( s in seasons2display) {
  v <- seinfeld[seinfeld$season==s,'viewers']
  qqnorm(v,main=paste("Season",s))
  abline(a=median(v),b=mad(v))
}

# Use centered and scaled residuals
seasons2display <- c(4,5,6,9)
par(mfrow=c(2,2))
for( s in seasons2display) {
  v0 <- seinfeld[seinfeld$season==s,'viewers']
  v1 <- (v0 - median(v0))/mad(v0)
  qqnorm(v1,main=paste("Season",s))
  abline(a=0,b=1)
}



}
\keyword{datasets}
