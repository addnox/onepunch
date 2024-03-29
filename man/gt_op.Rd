% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gt_extra.R
\name{gt_op}
\alias{gt_op}
\title{Basic format for \code{gt} table}
\usage{
gt_op(
  DT,
  theme = c("blue", "blue.fill", "green", "green.fill"),
  spanner_split = NA,
  title = NULL,
  title_block_color = "#ee1c25",
  missing_text = "",
  ...
)
}
\description{
Basic format for \code{gt} table
}
\details{
Some tips of using \code{gt} package
\itemize{
\item use \code{gt::gtsave} to save table as png
\item check \code{gt::cols_label} for column header renaming
\item check \code{gt::cols_merge_range} to combine Lbound and Ubound into one
}
}
\examples{
x <- data.table::data.table(
  " " = as.character(1:3),
  "Terms_Limit" = c(10e6, 10e6, 20e6),
  "Terms_Deductible" = c(10e6, 20e6, 30e6),
  "Terms_Reinstatement" = c("2@100\%", "2@100\%", "1@100\%"),
  "Price_ROL" = c(.4, .35, NA)
  )
  gt_op(x, title = "Price Table")
  gt_op(x, title = "Price Table", title_block_color = "blue")
  gt_op(x, spanner_split = "_", theme = "green", title = "Price Table") |> gt::fmt_number(2:3, suffixing = TRUE)
}
