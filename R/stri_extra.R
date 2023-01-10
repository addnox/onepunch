#' Truncate a string to maximum width
#'
#' Code borrowed and modified from `stringr` package
#' @param x Input vector. Either a character vector, or something coercible to one.
#' @param width Maximum width of string.
#' @param side,ellipsis Location and content of ellipsis that indicates content has been removed.
#' @export
#' @examples
#' x <- "This string is moderately long"
#' rbind(
#'   stri_trunc(x, 20, "right"),
#'   stri_trunc(x, 20, "left"),
#'   stri_trunc(x, 20, "center")
#' )
stri_trunc <- function (x, width, side = c("right", "left", "center"), ellipsis = "...") {
  side <- match.arg(side)
  # if (!is.integer(width)) stop("`width` must be an integer", call. = FALSE)
  # if (width > 0) stop("`width` must be a positive integer", call. = FALSE)
  too_long <- !is.na(x) & stringi::stri_length(x) > width
  width... <- width - stringi::stri_length(ellipsis)
  if (width... < 0) stop("`width` is shorter than `ellipsis`", .call = FALSE)
  x[too_long] <- switch(
    side,
    right = stringi::stri_c(stringi::stri_sub(x[too_long], 1, width...), ellipsis),
    left = stringi::stri_c(ellipsis, stringi::stri_sub(x[too_long], -width..., -1)),
    center = stringi::stri_c(stringi::stri_sub(x[too_long], 1, ceiling(width.../2)), ellipsis, stringi::stri_sub(x[too_long], -floor(width.../2), -1))
  )
  x
}

#' In addition to trimming, `stri_squish` also reduces repeated whitespace inside a string.
#' @export
#' @rdname stri_trunc
stri_squish <- function(x) {
  stringi::stri_trim_both(stringi::stri_replace_all_regex(x, "\\s+", " "))
}

#' Cross-join character vectors and paste
#' @param ... One or more character vectors, as in `paste` function
#' @param sep A character string to separate the terms
#' @export
#' @examples
#' stri_cj(c("SI", "Prem"), c("Gross", "SP", "Net"), c("Actual", "AsIf"), sep = "..")

stri_cj <- function(..., sep = "_") {
  x <- data.table::CJ(..., sorted = FALSE)
  res <- x[, do.call(paste, c(.SD, sep = sep))]
  res
}
