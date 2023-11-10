\name{brewers1982}
\alias{brewers1982}
\docType{data}
\title{
Basic Summaries of Boxscores for the Milwaukee Brewers 1982 Season
}
\description{
Basic Summaries of Boxscores for the Major League Baseball team Milwaukee (WI) Brewers 1982 Season.  
The Brewers won the American League championship that year.
Brewer, Robin Yount won the Most Valueable Player (MVP) award.  
#Robin Yount.  MVP.
}
\usage{data("brewers1982")}
\format{
  A data frame with 163 observations on the following 8 variables.
  \describe{
    \item{\code{Date}}{a character vector}
    \item{\code{Opp}}{a character vector}
    \item{\code{R}}{a numeric vector}
    \item{\code{RA}}{a numeric vector}
    \item{\code{Time}}{a character vector}
    \item{\code{Attendance}}{a numeric vector}
    \item{\code{home}}{a logical vector}
    \item{\code{win}}{a logical vector}
  }
}
\examples{
data(brewers1982)
# proportion of wins for a given number of runs scored
pwin <- with(brewers1982,tapply(win,R,mean))
pwin
# graphical display of the above
plot(names(pwin),pwin,xlab='Runs', ylab='Proportion of Wins',main='Brewers 1982')
}
\keyword{datasets}
