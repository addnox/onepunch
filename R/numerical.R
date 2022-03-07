#' Compare two numeric vectors
#'
#' This is a safe way of comparing if two vectors of floating point numbers
#' are (pairwise) equal.  This is safer than using `==`, because it has
#' a built in tolerance
#'
#' @param x,y Numeric vectors to compare
#' @param tol Tolerance of comparison.
#' @export
#' @examples
#' sqrt(2) ^ 2 == 2
#' num_near(sqrt(2) ^ 2, 2)
num_near <- function(x, y, tol = 1e-5) {
  abs(x - y) < tol
}

#' Check if the elements of a numeric vector are close to integers
#'
#' @param x Numeric vectors to check
#' @param tol Tolerance of comparison.
#' @export
#' @examples
#' num_is_integer(c(1.0000001, -1.9999999999, 5.99999999, -2, 2.123456, 2.0001, pi))
num_is_integer <- function(x, tol = 1e-5) {
  num_near(x, round(x), tol = tol)
}

