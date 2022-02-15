#' Loop for side-effect only
#' @param X Vector to loop through
#' @param FUN Function that is applied to \code{X}
#' @param ... optional arguments to \code{FUN}
#' @export
#' @examples
#' a <- slapply(1:3, function(x) cat("Num", x, "\n"))
#' a ## Return 1, 2 and 3, rather than Num 1, Num 2 and Num 3

slapply <- function(X, FUN, ...) {
  FUN <- match.fun(FUN)
  if (!is.vector(X) || is.object(X)) {
    X1 <- as.list(X)
  } else {
    X1 <- X
  }

  .Internal(lapply(X1, FUN))
  invisible(X)
}

#' Feed elements in a list to a function
#' @param X A list
#' @param FUN A function
#' @param ... optional arguments to \code{FUN}
#' @export
#' @examples
#' x <- list(a = 1:3, b = 4:6)
#' f <- function(a, b, mult = 1) (a + b) * mult
#' elapply(x, f) ## 5, 7, 9
#' elapply(x, f, mult = 2) ## 10, 14, 18
#' elapply(x[1], f, b = 1) ## 2, 3, 4
elapply <- function(X, FUN, ...) {
  if (!is.list(X)) stop("`X` must be a list", call. = FALSE)

  do.call(Map, c(list(f = FUN), X, list(...)))
}
