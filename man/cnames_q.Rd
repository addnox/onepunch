% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cnames.R
\name{cnames_q}
\alias{cnames_q}
\title{Escape version of \code{cnames_q}, usually used inside a function}
\usage{
cnames_q(DT, cols = NULL)
}
\description{
Must work with quoted arguments inside a function
e.g. cnames_q(DT, substitute(cols))
}
