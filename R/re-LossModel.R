#' @export
loss_in_layer <- function(x, Limit, Deductible, ...) {
  UseMethod("loss_in_layer")
}

#' @export
build_cdf <- function(x, min, max, n = 500, ...) {
  UseMethod("build_cdf")
}
