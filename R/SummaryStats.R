#' Common summary statistics in reinsurance practice
#'
#' @description
#' `gMean` mean
#' `gSD` standard deviation
#' `gVaR` Value at Risk
#' `gTVaR` Tail Value at Risk (a.k.a Conditional Tail Expectation, or CTE)
#' @export
gMean <- function(x, ...) UseMethod("gMean")
#' @export
#' @rdname gMean
gSD <- function(x, ...) UseMethod("gSD")
#' @export
#' @rdname gMean
gVaR <- function(x, ...) UseMethod("gVaR")
#' @export
#' @rdname gMean
gTVaR <- function(x, ...) UseMethod("gTVaR")


#' @exportS3Method
gMean.default <- function(x, ...) {
  mean.default(x)
}

#' @exportS3Method
gSD.default <- function(x, ...) {
  sd(x)
}

#' @exportS3Method
gVaR.default <- function(x, prob, ...) {
  quantile(x, prob, names = FALSE, type = 2)
}

#' @exportS3Method
gTVaR.default <- function(x, prob, ...) {
  mean.default(x[x >= quantile(x, prob, names = FALSE, type = 2)])
}
