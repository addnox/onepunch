#' Extract certain element from one vector
#'
#' Useful for messy data cleaning
#' @inheritParams vec_detect
#' @param fill_direction If `NULL`, no fill.  Otherwise, passed to `vec_nafill`'s `direction` parameter.
#' @param trim Whether to trim white spaces of final results.
#' @export
#' @examples
#' x <- c("", "Property_NoOfRisk", "SI", "Prem", "MB_NoOfRisk", "SI", "Prem", "% of Prem")
#' vec_extract(x, c("property|mb"))
#' vec_extract(x, c("property|mb"), ignore_case = FALSE)
#' vec_extract(x, c("property|mb"), fill_direction = "down")
#' vec_extract(x, c("property", "(.*)_NoOfRisk")) ## AND relation
#' ## use regex to get full contents
#' vec_extract(x, ".*NoOfRisk")
#' ## or use ifelse and vec_detect
#' ifelse(vec_detect(x, "NoOfRisk"), x, NA)
#' ## regex can also extract the key contents in parenthesis (i.e. capture group in regex)
#' vec_extract(x, "(.*)_NoOfRisk")
#' ## Use `exclusions` to increase accuracy
#' vec_extract(x, c("SI|Prem"), exclusions = "% of Prem")
#' vec_extract(x, c("SI|^Prem")) ## a well-defined regex can also work

vec_extract <- function(x, patterns, exclusions = NULL, fill_direction = NULL, ignore_case = TRUE, trim = TRUE) {
  if (!is.vector(patterns, mode = "character")) stop("`patterns` must be a character vector.", call. = FALSE)

  vec_extract_single_pattern <- function(.pattern) {
    res_matched_split <- stringi::stri_match_first_regex(x, .pattern, case_insensitive = ignore_case)
    if (ncol(res_matched_split) == 1) {
      res_matched <- res_matched_split[, 1, drop = TRUE]}
    else {
      res_matched <- res_matched_split[, 2, drop = TRUE] # multiple columns when there is capture groups (i.e. (p) in regex)
    }
    res <- stringi::stri_trim_both(data.table::fcoalesce(res_matched))
  }

  mat_res <- vapply(patterns, vec_extract_single_pattern, character(length(x)))
  mat_firstCol <- mat_res[, 1]
  res <- mat_firstCol[rowSums(mat_firstCol == mat_res) == ncol(mat_res)]

  if (!is.null(exclusions)) {
    res_ignore <- vec_detect(x, patterns = exclusions, exclusions = NULL, ignore_case = ignore_case, na.rm = TRUE)

    res[res_ignore] <- NA
  }

  if (!is.null(fill_direction)) res <- vec_nafill(res, direction = fill_direction)
  if (trim) res <- stringi::stri_trim_both(res)

  res
}
