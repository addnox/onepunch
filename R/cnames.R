#' Helper function to get matched cols of a data.table
#'
#' @param DT A `data.table` or `data.frame` object
#' @param cols Fully compatible with `data.table`'s `.SDcols` parameter
#'
#' @examples
#' cnames(mtcars) # same as names(mtcars)
#' cnames(mtcars, 1:3) # same as names(mtcars)[1:3]
#' cnames(mtcars, !c("mpg", "cyl"))
#' cnames(mtcars, is.character)
#' cnames(mtcars, !is.character)
#' cnames(mtcars, vs:carb)
#' cnames(mtcars, !patterns("mpg|cyl")) # regular expression
#' cnames(mtcars, list(cyl:hp, patterns("carb"), !is.integer)) # union of results for list input
#' cnames(mtcars, .(cyl:hp, patterns("carb"), !is.integer)) # `.` is also interpreted as list
#' ctypes(mtcars)
#' ctypes(mtcars, as.DT = TRUE)
#' # use cnames in a function
#' cnames2 <- function(DT, col = NULL) {
#'   cc <- substitute(col)
#'   cnames(DT, !!cc)
#' }
#' cnames2(mtcars)
#' cnames2(mtcars, vs:carb)
#' cnames2(mtcars, !patterns("mpg|cyl")) # regular expression

#' @export
cnames <- function(DT, cols = NULL) {
  if (missing(cols)) return(names(DT))
  cols_q <- substitute(cols)
  res <- cnames_q(DT, cols_q)
  res
}
#' Escape version of `cnames_q`, usually used inside a function
#'
#' Must work with quoted arguments inside a function
#' e.g. cnames_q(DT, substitute(cols))
cnames_q <- function(DT, cols = NULL) {
  if (!is.data.frame(DT)) stop("DT must be a data.table or data.frame.", call. = FALSE)
  DT <- as.data.table(DT)

  # case when cols is not a list
  if (is.null(cols)) return(names(DT)) # substitute of NULL is still NULL
  if (!as.character(as.list(cols)[[1L]]) %in% c("list", ".")) cols <- substitute(list(x), env = list(x = cols))

  lcols <- as.list(cols)[-1L] ## 1st element is `list`

  # get names for each set of criterion
  lres <- lapply(
    lcols,
    function(expr) eval(substitute(DT[, names(.SD), .SDcols = cols], env = list(cols = expr)))
  )
  # union into one vector
  res <- Reduce(union, lres)
  # restore col sequence
  res <- intersect(names(DT), res)

  res
}


#' @export
ctypes <- function(DT, style = c("mode", "typeof"), as.DT = FALSE) {
  if (!is.data.frame(DT)) stop("DT must be a data.table or data.frame.", call. = FALSE)
  DT <- as.data.table(DT)
  style <- match.arg(style)
  res <- DT[, vapply(.SD, style, character(1L))]

  if (as.DT) {
    res <- data.table(name = names(res), type = res)
  }

  res
}

