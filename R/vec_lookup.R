#' Excel `vlookup` equivalent
#'
#' @export
#' @examples
#' (x <- c("A", "B", "C", "CBA", "abc"))
#' key <- c("a", "b", "BA")
#' value <- c("Alice", "Bob", "Apple")
#' vec_lookup(x, key, value)
#' vec_lookup(x, key, value, ignore_case = FALSE)
#' vec_lookup(x, key, value, default = "Nobody")
#' vec_lookup(x, key, value, default = x)
vec_lookup <- function(x, key, value = key, ignore_case = TRUE, default = NULL, ...) {
  if (ignore_case) {
    x <- tolower(x)
    key <- tolower(key)
  }

  m <- match(x, key, ...) ## get only the first match

  res <- value[m]

  if (!is.null(default)) {
    if (length(default) == 1L) {
      res[is.na(res)] <- default
    } else if (length(default) == length(res)) {
      res[is.na(res)] <- default[is.na(res)]
    }
  }

  res
}
