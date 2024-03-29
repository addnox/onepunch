% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qKeyFactor.R
\name{qKeyFactor}
\alias{qKeyFactor}
\title{Data Analysis function to find key factors}
\usage{
qKeyFactor(
  x,
  y = NULL,
  by,
  f,
  base_type = c("nil", "mean"),
  auto_sum = TRUE,
  marginal = TRUE
)
}
\arguments{
\item{x, y}{A \code{data.table}, where \code{y} can be \code{NULL}}

\item{by}{Column names that can uniquely define \code{x} and \code{y}, in character vector}

\item{f}{Unquoted formula, applied to \code{x} amd \code{y}.  The result of \code{f} will be evaluate to find key factors}

\item{auto_sum}{TRUE/FALSE, whether to sum all vars if \code{by} cannot uniquely define each row for \code{x} and \code{y}}

\item{marginal}{TRUE/FALSE, whether to calculate marginal effect for each row of records.  Note that calculating marginal effect
can be high computationally expensive}
}
\description{
Data Analysis function to find key factors
}
\examples{
DT_Year1 <- data.table::data.table(Branch = LETTERS[1:5], Premium = 100 * (1:5), LossRatio = c(.25, .2, .8, .3, .2))
DT_Year2 <- data.table::data.table(Branch = LETTERS[1:5], Premium = c(100, 200, 100, 400, 600), LossRatio = c(.25, .2, .2, .3, .4))
# test what drives Year2 loss ratio
qKeyFactor(DT_Year2, by = "Branch", f = sum(Premium * LossRatio) / sum(Premium), base_type = "nil")
qKeyFactor(DT_Year2, by = "Branch", f = sum(Premium * LossRatio) / sum(Premium), base_type = "mean")
# test that drives the change from Year1 to Year2
qKeyFactor(DT_Year1, DT_Year2, by = "Branch", f = sum(Premium * LossRatio) / sum(Premium))
}
