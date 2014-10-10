\name{blood.plasma}
\alias{blood.plasma}
\docType{data}
\title{
Blood plasma measurements related to total triglyceride level
}
\description{
Data table from Table 9.11 of Hollander and Wolfe (1999).
The data consists of triglyceride levels on 13 patients.
Two factors, each at two levels, were recorded: Sex and Obesity.
The concomitant variables are chylomicrons, age, and three lipid
variables (very low-density lipoproteins (VLDL),  low-density lipoproteins (LDL),
and high-density lipoproteins (HDL)).
}
\usage{data(blood.plasma)}
\source{
Hollander, M. and Wolfe, D.A. (1999), \emph{Nonparametric Statistical Methods}, New York: Wiley.
}
\references{
Hollander, M. and Wolfe, D.A. (1999), \emph{Nonparametric Statistical Methods}, New York: Wiley.
}
\examples{
data(blood.plasma)
plot(Total~Age,data=blood.plasma)
boxplot(Total~Obese,data=blood.plasma)
}
\keyword{datasets}
