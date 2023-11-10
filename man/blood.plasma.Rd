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
\format{
	A data frame with 13 observations on 8 variables.
	\describe{
		\item{\code{Total}}{Triglyceride level, response}
		\item{\code{Sex}}{Sex, 2 levels}
		\item{\code{Obese}}{Obesity, 2 levels}
		\item{\code{Chylo}}{Chylomicrons, covariate}
		\item{\code{VLDL}}{Very low density, lipids, covariate}
		\item{\code{LDL}}{Low density, lipids, covariate}
		\item{\code{HDL}}{High density, lipids, covariate}
		\item{\code{Age}}{Age}
	}
}
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
