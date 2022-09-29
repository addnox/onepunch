#' A more general `nafill` that can work with non-numeric data type
#'
#' @param direction Direction in which to fill missing values.
#'                  Currently either "down" (the default), "up", "downup" (i.e. first down and then up) or "updown" (first up and then down).
#'                  "forwards" is an alias of "down", as "backwards" is of "up".
#' @export
#' @examples
#' x <- c("A", NA, "B", NA, NA, NA, "C", "D", NA)
#' vec_nafill(x, "down")
#' vec_nafill(x, "up")
#' vec_nafill(x, fill = "missing")
#' vec_nafill(x, "down", breaks = c(F, F, F, F, T, F, F, F, F))

vec_nafill <- function(x, direction = c("constant", "down", "up", "forwards", "backwards"), fill = NA, breaks = NULL) {
  DT <- data.table::data.table(...x = x)
  if (!is.null(breaks) && length(x) != length(breaks)) stop("length of x and breaks must be identical.", call. = TRUE)

  idx <- is.na(x) | breaks

  direction <- match.arg(direction)

  if (direction %in% c("down", "forwards")) type <- "locf"
  if (direction %in% c("up", "backwards")) type <- "nocb"

  if (direction != "constant") {
    idx <- replace(seq_along(x), is.na(x), NA)
    idx[breaks] <- NaN
    res <- x[data.table::nafill(idx, type = type, nan = NaN)]
  } else {
    res <- x
    res[is.na(res)] <- fill
  }

  return(res)
}
