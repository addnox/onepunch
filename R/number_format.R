#' Format numbers
#'
#' @export
#' @examples
#' x <- c(1.23e12, 3.45e7, 6.78e3, 0.1, 0.012, 0.0123, pi / c(1, 100, 1000), Inf, NA)
#' num(x, digits = 4)
#' num(x, "P")
#' vapply(x, num_auto, character(1L))
#' vapply(x, num_auto, character(1L), perc_cutoff = 2)
num <- function(x, scale = c("C", "P", "K", "M", "B", "pp"), digits = 0, sep = "", na_as = NA_character_, inf_as = "Inf") {
  scale <- match.arg(scale)

  scale_factor <- switch(scale, "C" = 1, "K" = 1e3, "M" = 1e6, "B" = 1e9, "P" = .01, "pp" = .01)
  scale_label <- switch(scale, "C" = "", "K" = "K", "M" = "M", "B" = "B", "P" = "%", "pp" = " p.p.")
  scale_label <- paste0(sep, scale_label)

  res <- scales::comma(x, accuracy = 1 / 10 ^ digits, scale = 1 / scale_factor, suffix = scale_label)

  res <- ifelse(is.na(x), NA_character_, res)

  return(res)
}


#' @export
#'@rdname num
num_format <- function(scale = c("C", "P", "K", "M", "B"), digits = 0, sep = "") {
  f <- function(x) num(x, scale, digits, sep)
  f
}

#' @param perc_cutoff The threshold for percentage format.  If most elements are lower than it, the whole vector will be formatted as percentage.
#' @rdname num
#' @examples
#' num_auto(c(NA, 100, 1000, 10))
#' num_auto(c(NA, 100, 1000, 10), prefer_digits = 1)
#' num_auto(c(100e6, 200e6, 300e6))
#' num_auto(c(-123.456789e6, 1e9, 2e9, 0, -3e9))
#' num_auto(c(-123.456789e6, 1e9, 2e9, 0, -3e9), free_digits = TRUE, max_digits = 2)

num_auto <- function(x, prefer_digits = 0, free_digits = FALSE, max_digits = 4, perc_cutoff = 5, p = .95) {
  # scale
  scale_factor <- c("C" = 1, "K" = 1e3, "M" = 1e6, "B" = 1e9)
  x_nonNA <- na.omit(x)
  min_N <- floor(length(x_nonNA) * p)

  if (sum(abs(x_nonNA) <= perc_cutoff) >= min_N) {
    selected_scale <- "P"
    selected_factor <- .01
  } else {
    for (i in rev(seq_along(scale_factor))) {
      if (sum(abs(x_nonNA) %/% (10 ^ (-prefer_digits) * scale_factor[[i]]) > 0) >= min_N) { ## 0.1 scaled so that 0.9M is prefered than 900K
        selected_scale <- names(scale_factor)[[i]]
        selected_factor <- scale_factor[[i]]
        break
      } else {
        ## default to be comma scale
        selected_scale <- names(scale_factor)[[1]]
        selected_factor <- scale_factor[[1]]
      }
    }
  }

  # digits
  num_scaled <- as.vector(x_nonNA / selected_factor)
  v <- round(abs(num_scaled) %% 1, max_digits)
  selected_digits <- pmax(0, pmax(nchar(v) - 2))

  # results
  if (free_digits) {
    res <- unlist(Map(function(x, d) num(x, selected_scale, d), x = x, d = selected_digits))
  } else {
    res <- num(x, selected_scale, max(selected_digits))
  }

  return(unname(res))
}

