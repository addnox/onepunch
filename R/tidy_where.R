#' Return row index (or logical vector) that match the pre-defined patterns
#'
#' @inheritParams vec_detect
#' @export
#' @examples
#' x <- data.table(
#'   c("ABC P&C Limited", "Layer", "Limit", NA, 10, 10, 10, "Total Limit"),
#'   c(NA, NA, "Excess", NA, 10, 20, 30, "")
#' )
#' tidy_where(x, "limit")
#' tidy_where(x, c("limit|excess"), exclusions = "ABC")
#' tidy_which(x, c("limit|excess"), exclusions = "ABC")

tidy_where <- function(DT, patterns, exclusions = NULL) {
  DT <- data.table::as.data.table(DT)

  res <- vapply(DT, vec_detect, logical(nrow(DT)), patterns = patterns, exclusions = exclusions, USE.NAMES = FALSE)

  res
}

#' @export
#' @rdname tidy_where

tidy_which <- function(DT, patterns, exclusions = NULL, as.DT = TRUE) {
  res_where <- tidy_where(DT, patterns, exclusions)
  res <- which(res_where, arr.ind = TRUE, useNames = FALSE)
  if (as.DT) {
    res <- as.data.table(res)
    data.table::setnames(res, c("row", "col"))
  }
  res
}
