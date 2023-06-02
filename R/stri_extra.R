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

#' Extract Chinese characters from strings
#' @export
#' @examples
#' x <- c("Sunflower Insurance (葵花保险) （新型）", "Sugarbeet Insurance（甜菜保险）", "Corn Full Cost Insurance (Irrigated Land)（水地玉米完全成本保险）", "Corn Full Cost Insurance (Dry Land)（旱地玉米完全成本保险）")
#' stri_extract_Chinese(x)
#' stri_extract_Chinese(x, simplify = FALSE)

stri_extract_Chinese <- function(x, sep = "_", simplify = TRUE) {
  pattern <- "(\\p{Han}){1,}"
  x1 <- stringi::stri_extract_all_regex(x, pattern)
  if (simplify == TRUE) {
    res <- stringi::stri_c_list(x1, sep = sep)
  } else {
    res <- x1
  }

  res
}

#' Approximate string mapping
#' @export
#' @examples
#' x1 <- c("水稻种植保险\\nRice Insurance", "水地玉米种植保险\\nIrrigated Land Corn Insurance", "旱地玉米种植保险\\nDryland Corn Insurance", "水地小麦种植保险\\nIrrigated Land Wheat Insurance ", "旱地小麦种植保险\\nDryland Wheat Insurance", "水地马铃薯保险\\nIrrigated Potato Insurance ", "旱地马铃薯保险\\nDryland Potato Insurance", "油菜种植保险\\nRape Insurance")
#' x2 <- c("Irrigated Land Corn Insurance (水地玉米种植保险)", "Rice Insurance (水稻种植保险)", "Irrigated Land Wheat Insurance（水地小麦种植保险）", "Rape Insurance （油菜种植保险)", "Dryland Potato Insurance (旱地马铃薯保险)", "Dryland Wheat Insurance（旱地小麦种植保险）", "Irrigated Potato Insurance (水地马铃薯保险)", "Dryland Corn Insurance (旱地玉米种植保险)")
#' data.frame(Orig = x1, Mapped = stri_amap(x1, x2))
stri_amap <- function(x, y) {
  res <- y[stringdist::amatch(x, y, maxDist = Inf)]
  res
}

#' Convert full-width characters into half-width
#' @export
#' @examples
#' x <- c("(Irrigated Land Corn InsuranceABC,.?!) （水地玉米种植保险ＡＢＣ，。？！）", "Rice Insurance (水稻种植保险)")
#' stri_full_to_half(x)

stri_full_to_half <- function(x) {

  stri_full_to_half_single <- function(x1) {
    # 全角空格为12288，半角空格为32
    # 其他字符半角(33-126)与全角(65281-65374)的对应关系是：均相差65248
    int1 <- utf8ToInt(x1)
    int1_half <- data.table::fcase(
      int1 > 65280 & int1 < 65375, int1 - 65248L,
      int1 == 12288L,               32L,
      int1 > 0,                    int1
    )

    res <- intToUtf8(int1_half)
    res
  }

  vapply(x, stri_full_to_half_single, character(1L), USE.NAMES = FALSE)
}

