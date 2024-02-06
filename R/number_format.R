#' Format numbers
#'
#' @export
#' @examples
#' x <- c(1.23e12, 3.45e7, -6.78e3, Inf, NA)
#' num(x)
#' num(x, "free")
#' num(x, "free", inf.as = "Unlimited", na.as = "")
num <- function(x, scale = c("auto", "free", "C", "P", "K", "M", "B", "pp"), digits = 2, na.as = NA_character_, inf.as = "Inf", ...) {
  scale <- match.arg(scale)
  .accuracy <- 1 / 10 ^ digits

  if (scale == "free") {
    res <- scales::comma(x, scale_cut = scales::cut_short_scale()[-5], accuracy = .accuracy) ## get rid of trillion
  } else if (scale == "auto") {
    tmp_scale <- cut(abs(x), c(0, 1e3, 1e6, 1e9, Inf), c("C", "K", "M", "B"))
    final_scale_label <- names(which.max(table(tmp_scale)))
    if (is.na(final_scale_label)) final_scale_label <- "C"
    final_scale_factor <- switch(final_scale_label, "C" = 1, "K" = 1e3, "M" = 1e6, "B" = 1e9)
    if (final_scale_label == "C") final_scale_label <- ""

    res <- scales::comma(x, accuracy = .accuracy, scale = 1 / final_scale_factor, suffix = final_scale_label, ...)

  } else {
    scale_factor <- switch(scale, "C" = 1, "K" = 1e3, "M" = 1e6, "B" = 1e9, "P" = .01, "pp" = .01)
    scale_label <- switch(scale, "C" = "", "K" = "K", "M" = "M", "B" = "B", "P" = "%", "pp" = " p.p.")

    res <- scales::comma(x, accuracy = .accuracy, scale = 1 / scale_factor, suffix = scale_label, ...)
  }

  res <- ifelse(is.na(x), na.as, res)
  res <- ifelse(is.infinite(x), inf.as, res)

  return(res)
}


#' @export
#'@rdname num
num_format <- function(scale = c("auto", "free", "C", "P", "K", "M", "B"), digits = 0, ...) {
  scale <- match.arg(scale)
  f <- function(x) num(x, scale, digits, ...)
  f
}
