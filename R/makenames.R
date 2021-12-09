#' Quickly fixed vector names
#'
#' @export
#' @examples
#' vec_setnames(1:3)
#' vec_setnames(c("a" = 1, 2))
#' vec_setnames(c("a" = 1, 2), overwrite = FALSE)
#' vec_setnames(list("a" = 1, 2), overwrite = FALSE)
#'
vec_setnames <- function(x, nm = x, overwrite = TRUE) {
  if (is.null(nm)) newnm <- NULL
  nm <- as.character(nm)

  if (overwrite) {
    newnm <- nm
  } else {
    if (is.null(names(x))) {
      newnm <- rep(NA_character_, length(x))
    } else {
      newnm <- names(x)
      newnm[newnm == ""] <- NA_character_ ## partially named vector, blank names are denoted as "", rather than `NA`
    }

    newnm <- fcoalesce(newnm, nm)
  }

  names(x) <- newnm

  return(x)
}

#' Make names from a character vector
#'
#' @param blank_only Only change names for blank items, i.e. keep original non-NA names.
#' @param prefix Prefix of numbered names, e.g. "v" as a prefix for "v1".
#' @export
#' @examples
#' x <- c(NA, "Gross Premium ", "Loss --- Incurred", "Premium/Limit", "# of Risks", "Adj. Rate (%)", NA, "GrossPremium")
#' makenm_numbered(x)
#' makenm_numbered(x, prefix = "...")
#' makenm_numbered(x, blank_only = TRUE)
#' makenm_unique(c("A", "B", "A", NA))
#' makenm_clean(x)
#' makenm_clean(x, style = "title")
#' makenm_clean(x, sep = "")
#' makenm_clean(x, style = "asis", sep = "", allow_na = TRUE, allow_dup = TRUE)
#' ## A more common use of makenm functions are for reading data into tibbles using packages like readr or readxl
#' list(LETTERS[1:5], 1:5) |> tibble::as_tibble(.name_repair = makenm_numbered)

makenm_numbered <- function(x, prefix = "V", blank_only = FALSE) {
  if (length(x) == 0L) {
    return(character(0L))
  } else {
    name_numbered <- paste0(prefix, seq_along(x))
  }

  if (blank_only) {
    name_final <- ifelse(x == "" | is.na(x), name_numbered, x)
  } else {
    name_final <- name_numbered
  }

  return(name_final)
}

#' @rdname makenm_numbered
#' @export
makenm_unique <- function(x, prefix = "V") {
  # clean all blanks and NAs
  x2 <- makenm_numbered(x, prefix = prefix, blank_only = TRUE)

  # return x2 if no dups
  if (!anyDuplicated(x2)) return(x2)

  # find dups
  xDT <- data.table::data.table(x = x2)
  xDT[, `:=`(N = seq_len(.N), dup = .N > 1), by = "x"]
  xDT[, newname := fifelse(dup, paste(x, N, sep = "..."), as.character(x))]

  return(xDT[["newname"]])
}

#' \code{makenm_clean} is inspired by \code{janitor::make_clean_names} and \code{snakecase::to_any_case}
#'
#' @param style Case style of names, will change the case of prefix if applicable.
#' @param allow_na If FALSE, will replace \code{NA}s to v-prefix names, e.g. "v1".
#' @param allow_dup If FALSE, duplicated occurrence will be labeled. e.g. "Var", "Var2", "Var3".  Note that it is case-sensitive so "var" and "Var" are viewed as different names.
#' @rdname makenm_numbered
#' @export
makenm_clean <- function(x, prefix = "V", sep = "_", style = c("asis", "lower", "title", "upper"), allow_na = FALSE, allow_dup = FALSE) {
  style <- match.arg(style)
  f_style <- switch (style,
                     "asis" = function(x) x,
                     "lower" = stringi::stri_trans_tolower,
                     "title" = stringi::stri_trans_totitle,
                     "upper" = stringi::stri_trans_toupper
  )

  na_pos <- is.na(x) ##NA position for future use

  v_pattern <- c("'", "\"", "%", "#", "+", "$")
  v_replace  <- c("", "", "_percent_", "_number_", "_plus_", "_dollar_")
  replaced_names <- stringi::stri_replace_all_fixed(str = x, v_pattern, v_replace, vectorize_all = FALSE)

  good_start <- replaced_names |>
    stringi::stri_replace_first_regex(pattern = "\\A[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]*(.*)$",
                         replacement = "$1") |> ## stringi uses $1 instead of \\1
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_trim_both() |>
    makenm_numbered(prefix = prefix, blank_only = TRUE)

  if (style == "asis") {
    made_names <- makenm_numbered(good_start, blank_only = TRUE, prefix = prefix)
  } else {
    made_names <- make.names(good_start)
  }

  cased_names_list <- made_names |>
    stringi::stri_replace_all_regex("[^[:alnum:]]", "_") |>
    stringi::stri_split_fixed(pattern = "_")

  cased_names <- cased_names_list |>
    lapply(function(x) {return(x[x != ""])}) |>
    lapply(f_style) |>
    vapply(paste, collapse = sep, character(1L), USE.NAMES = FALSE)

  if (!allow_dup) cased_names <- makenm_unique(cased_names, prefix = prefix)

  if (allow_na) cased_names[na_pos] <- NA_character_

  cased_names
}

