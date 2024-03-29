% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearCombPoints.R
\name{dlcpois1}
\alias{dlcpois1}
\title{The PMF of a linear combination of Poisson RVs}
\usage{
dlcpois1(l, lambdas, points, grid = 1000, a = 2)
}
\arguments{
\item{l}{integer argument, must be positive}

\item{lambdas}{mean rates}

\item{points}{coefficients representing points per goal}

\item{grid}{grid size for numeric integration}

\item{a}{interval bounds for the CF inversion integration}
}
\value{
numeric
}
\description{
{Numeric integration of PMF of a linear combination of Poisson
RVs via inverting the CF using trapezoid rule.}
}
