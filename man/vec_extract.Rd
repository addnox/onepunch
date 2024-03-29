% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vec_extract.R
\name{vec_extract}
\alias{vec_extract}
\title{Extract certain element from one vector}
\usage{
vec_extract(
  x,
  patterns,
  exclusions = NULL,
  fill_direction = NULL,
  ignore_case = TRUE,
  trim = TRUE
)
}
\arguments{
\item{x}{A character vector.}

\item{patterns}{A vector means "AND" relations. See examples.}

\item{exclusions}{Patterns to be excluded (i.e. set to FALSE in results).}

\item{fill_direction}{If \code{NULL}, no fill.  Otherwise, passed to \code{vec_nafill}'s \code{direction} parameter.}

\item{trim}{Whether to trim white spaces of final results.}
}
\description{
Useful for messy data cleaning
}
\examples{
x <- c("", "Property_NoOfRisk", "SI", "Prem", "MB_NoOfRisk", "SI", "Prem", "\% of Prem")
vec_extract(x, c("property|mb"))
vec_extract(x, c("property|mb"), ignore_case = FALSE)
vec_extract(x, c("property|mb"), fill_direction = "down")
vec_extract(x, c("property", "(.*)_NoOfRisk")) ## AND relation
## use regex to get full contents
vec_extract(x, ".*NoOfRisk")
## or use ifelse and vec_detect
ifelse(vec_detect(x, "NoOfRisk"), x, NA)
## regex can also extract the key contents in parenthesis (i.e. capture group in regex)
vec_extract(x, "(.*)_NoOfRisk")
## Use `exclusions` to increase accuracy
vec_extract(x, c("SI|Prem"), exclusions = "\% of Prem")
vec_extract(x, c("SI|^Prem")) ## a well-defined regex can also work
}
