#' Calculate duration between two dates
#'
#' Use month as basis.  Treat one "standard" month as of 30 days. Number of months are rounded to integer. e.g. 29 days are treated as 1 month, 1/3 quarter and 1/12 year
#'
#' @param d1,d2 vector of \code{Date} object
#' @param unit One of month, quarter or year
#' @examples
#' d <- as.Date(c("2010-03-31", "2010-06-30", "2012-12-31", "2013-1-1")) # 2012 is a leap year
#' date_duration(as.Date("2010-01-01"), d, "month")
#' date_duration(as.Date("2010-01-01"), d, "quarter")
#' date_duration(as.Date("2010-01-01"), d, "year")
#' @export
date_duration <- function(d1, d2, unit = c("day", "month", "quarter", "year"), round_digits = 1) {
  unit <- match.arg(unit)

  tmp_int <- lubridate::interval(d1, d2)

  if (unit == "day") {
    res <- tmp_int / lubridate::ddays(1)
  } else if (unit == "year") {
    res <- tmp_int / lubridate::years(1)
  } else {
    res_month <- tmp_int / lubridate::dmonths(1)
    if (unit == "month") {
      res <- res_month
    } else {
      ## quarter
      res <- res_month / 3
    }
  }

  if (!is.na(round_digits)) res <- round(res, digits = round_digits)

  res
}

#' Convert excel date integer into R date
#'
#' @export
#' @examples
#' date_parse_excel(c(NA, 43831, 43466))
#' x_month <- c(NA, "Aug.2005", "Jan to Feb 2008", "Mid September, 2018", "2018 July", "2019/07")
#' date_parse_month(x_month)
#' date_parse_month(x_month, period_end = TRUE)
#' x_quarter <- c(NA, "2019/Q3", "2018Q3", "Q2-2016", "1Q/15")
#' date_parse_quarter(x_quarter)
#' date_parse_quarter(x_quarter, period_end = FALSE)
date_parse_excel <- function(x, ...) {
  x1 <- as.integer(x)
  res <- openxlsx::convertToDate(x1, ...)
  res
}

#' @export
#' @rdname date_parse_excel
date_parse_month <- function(x, period_end = FALSE) {
  res <- as.Date(lubridate::parse_date_time(x, c("ym", "my"), quiet = TRUE))
  if (period_end) res <- lubridate::ceiling_date(res, "month") - lubridate::ddays(1L)
  return(res)
}

#' @export
#' @rdname date_parse_excel
date_parse_quarter <- function(x, period_end = TRUE) {
  # "Q1-yyyy" won't be parsed, so do some adjustments
  x1 <- stringi::stri_replace_first_regex(x, "^Q([1-4])", "$1Q")
  res <- as.Date(lubridate::parse_date_time(x1, c("qy", "yq"), quiet = TRUE))
  if (period_end) res <- lubridate::ceiling_date(res, "quarter") - lubridate::ddays(1L)
  return(res)
}
