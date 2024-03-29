% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numerical.R
\name{num_near}
\alias{num_near}
\title{Compare two numeric vectors}
\usage{
num_near(x, y, tol = 1e-05)
}
\arguments{
\item{x, y}{Numeric vectors to compare}

\item{tol}{Tolerance of comparison.}
}
\description{
This is a safe way of comparing if two vectors of floating point numbers
are (pairwise) equal.  This is safer than using \code{==}, because it has
a built in tolerance
}
\examples{
sqrt(2) ^ 2 == 2
num_near(sqrt(2) ^ 2, 2)
}
