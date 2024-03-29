% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readxl.R
\name{xlrange}
\alias{xlrange}
\title{A wrapper to cellranger cell_* functions}
\usage{
xlrange(range = NULL, rows = NULL, cols = NULL, ws = NULL)
}
\description{
A wrapper to cellranger cell_* functions
}
\examples{
xlrange("A1")
xlrange("A1:B3", ws = "Data")
xlrange(rows = 2:10)
xlrange(rows = c(3, NA)) # NA means to page end
xlrange(cols = "A:C")
xlrange(rows = 2:5, cols = c(9, NA))
}
