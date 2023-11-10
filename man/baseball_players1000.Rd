\name{baseball_players1000}
\alias{baseball_players1000}
\docType{data}
\title{
Career Information for a Random Sample of 1000 Baseball Players
}
\description{
Demographics and position information on 1000 randomly selected baseball players who debuted after 1945.
}
\usage{data("baseball_players1000")}
\format{
  A data frame with 1000 observations on the following 28 variables.
  \describe{
    \item{\code{playerID}}{a character vector}
    \item{\code{birthYear}}{a numeric vector}
    \item{\code{birthMonth}}{a numeric vector}
    \item{\code{birthDay}}{a numeric vector}
    \item{\code{birthCountry}}{a character vector}
    \item{\code{birthState}}{a character vector}
    \item{\code{nameFirst}}{a character vector}
    \item{\code{nameLast}}{a character vector}
    \item{\code{weight}}{a numeric vector}
    \item{\code{height}}{a numeric vector}
    \item{\code{bats}}{a character vector}
    \item{\code{throws}}{a character vector}
    \item{\code{debutYear}}{a numeric vector}
    \item{\code{G_all}}{a numeric vector}
    \item{\code{G_p}}{a numeric vector}
    \item{\code{G_c}}{a numeric vector}
    \item{\code{G_1b}}{a numeric vector}
    \item{\code{G_2b}}{a numeric vector}
    \item{\code{G_3b}}{a numeric vector}
    \item{\code{G_ss}}{a numeric vector}
    \item{\code{G_lf}}{a numeric vector}
    \item{\code{G_cf}}{a numeric vector}
    \item{\code{G_rf}}{a numeric vector}
    \item{\code{G_of}}{a numeric vector}
    \item{\code{G_dh}}{a numeric vector}
    \item{\code{G_ph}}{a numeric vector}
    \item{\code{G_pr}}{a numeric vector}
    \item{\code{pitcher}}{a logical vector}
  }
}
\details{
A random subset of baseball players who debuted after 1945 and played in at least 160 games.
Includes information on birth (date and location); height (inches) and weight (pounds); whether they bat left (L), right (R), or switch (B); and games played at each postion.  The variable pitcher is a derived variable based on if the majority of games were played as a pitcher (i.e.; G_pr/G_all > 0.5).
}
\source{
https://github.com/chadwickbureau/baseballdatabank
}
\references{
https://github.com/chadwickbureau/baseballdatabank/blob/master/readme2014.txt
}
\examples{
data(baseball_players1000)
hist(baseball_players1000$weight,xlab="Weight (lbs)",
     probability=TRUE, ylim=c(0,0.02),
     main="Histogram of Weight for 1000 Baseball Players")
lines(density(baseball_players1000$weight,na.rm=TRUE))

}
\keyword{datasets}
