#' Search for pattern
#'
#' @param x A character vector.
#' @param patterns A vector means "AND" relations. See examples.
#' @param exclusions Patterns to be excluded (i.e. set to FALSE in results).
#' @param negate single logical value; whether a no-match to `patterns` is rather of interest
#' @param offset Offset the results (see sample).
#' @param ignore.case Decide whether the `patterns` are case-insensitive.
#' @param na.rm Set `NA`s in original data as FALSE.
#' @export
#' @examples
#' x <- c(NA, NA, "layer", "no", 1, 2, 3, "layer info:")
#' x[vec_detect(x, c("layer|no"))] # OR relation by regular expression
#' x[vec_detect(x, c("layer|no"), na.rm = FALSE)]
#' x[vec_detect(x, c("layer", "info"))] # AND relation for vector
#' x[vec_detect(x, c("layer|no"), exclusions = "info")]
#' x[vec_detect(x, c("layer$|no"))]
#' x[vec_detect(x, c("layer$|no"), negate = TRUE)]

#' ## offset
#' y <- c(NA, NA, "2020", "(Jan-1)", "Company A", NA, NA, "2018/19", "0701", "Comp A")
#' y[vec_detect(y, "20\\d{2}")] ## easier to get year info
#' y[vec_detect(y, "20\\d{2}", offset = 1)] ## use offset for inconsistent month info
#' y[vec_detect(y, "^comp", offset = -1)] ## negative offset means moving upwards

vec_detect <- function(x, patterns = NULL, exclusions = NULL, offset = 0L, negate = FALSE, ignore_case = TRUE, na.rm = TRUE) {
  n_x <- length(x)

  vec_detect_ <- function(.pattern) {
    n_pattern <- length(.pattern)
    mat_res <- vapply(.pattern, stringi::stri_detect_regex, logical(n_x), str = x, negate = negate, case_insensitive = ignore_case)
    res <- rowSums(mat_res) == n_pattern
    res
  }

  res <- rep(TRUE, n_x)

  if (!is.null(patterns)) {
    if (!is.vector(patterns, mode = "character")) stop("`patterns` must be a character vector.", call. = FALSE)
    res <- vec_detect_(patterns)
  }

  if (!is.null(exclusions)) {
    if (!is.vector(exclusions, mode = "character")) stop("`exclusions` must be a character vector.", call. = FALSE)
    idx_exclusion <- vec_detect_(exclusions)
    res[idx_exclusion] <- FALSE
  }

  if (na.rm) res <- vec_nafill(res, fill = FALSE)
  if (offset != 0) res <- data.table::shift(res, n = offset, fill = FALSE)

  res
}

#' @export
#' @rdname vec_detect

vec_pick <- function(x, patterns = NULL, exclusions = NULL, y = x, offset = 0L, fill_direction = NULL, negate = FALSE, ignore_case = TRUE, na.rm = TRUE) {
  idx <- vec_detect(x, patterns, exclusions, offset = offset, negate = negate, ignore_case = ignore_case, na.rm = na.rm)
  res <- ifelse(idx, y, NA)
  if (!is.null(fill_direction)) res <- vec_nafill(res, direction = fill_direction)
  res
}
