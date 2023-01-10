#' @export
loss_in_layer <- function(x, Limit, Deductible, ...) {
  UseMethod("loss_in_layer")
}

#' @export
build_ecdf <- function(x, min, max, n = 500, ...) {
  UseMethod("build_ecdf")
}

#' Common summary statistics in reinsurance practice
#'
#' @description
#' `opMean` mean
#' `opSD` standard deviation
#' `opVaR` Value at Risk
#' `opTVaR` Tail Value at Risk (a.k.a Conditional Tail Expectation, or CTE)
#' @export
opMean <- function(x, ...) UseMethod("opMean")
#' @export
#' @rdname opMean
opSD <- function(x, ...) UseMethod("opSD")
#' @export
#' @rdname opMean
opVaR <- function(x, ...) UseMethod("opVaR")
#' @export
#' @rdname opMean
opTVaR <- function(x, ...) UseMethod("opTVaR")


#' @exportS3Method
opMean.default <- function(x, ...) {
  mean.default(x)
}

#' @exportS3Method
opSD.default <- function(x, ...) {
  sd(x)
}

#' @exportS3Method
opVaR.default <- function(x, prob, ...) {
  quantile(x, prob, names = FALSE, type = 2)
}

#' @exportS3Method
opTVaR.default <- function(x, prob, ...) {
  mean.default(x[x >= quantile(x, prob, names = FALSE, type = 2)])
}

#' @export
build_script <- function(x, ...) {
  UseMethod("build_script")
}
