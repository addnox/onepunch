#' Excel \code{vlookup} equivalent
#'
#' @export
#' @examples
#' (x <- c("A", "B", "C", "CBA", "abc"))
#' key <- c("a", "b", "A")
#' value <- c("Alice", "Bob", "Apple")
#' vec_lookup(x, key, value)
#' vec_lookup(x, key, value, ignore_case = FALSE)
#' vec_lookup(x, key, value, default = "Nobody")
#' vec_lookup(x, key, value, default = x)
#' vec_lookup(x, key, value, method = "regex")
#' vec_lookup(x, paste0("^", key, "$"), value, method = "regex")
vec_lookup <- function(x, key, value, method = c("exact", "regex"), ignore_case = TRUE, default = NULL) {
  method <- match.arg(method)

  if (ignore_case) {
    x <- tolower(x)
    key <- tolower(key)
  }

  if (method == "exact") {
    m <- match(x, key) ## get only the first match
  } else if (method == "regex") {
    dt0 <- lapply(key, function(.k) 1L * (grepl(.k, x = x, ignore.case = ignore_case)))
    data.table::setDT(dt0)
    ## by-ref modification is the fastest way I can think of now
    for (j in seq_along(key)) {
      ## along each col
      data.table::set(dt0, i = which(dt0[[j]] > 0), j, j)
      data.table::set(dt0, i = which(dt0[[j]] == 0), j, NA_integer_)
    }

    m <- data.table::fcoalesce(dt0)
  }

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
