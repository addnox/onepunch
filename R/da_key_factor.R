#' Data Analysis function to find key factors
#'
#' @param x,y A `data.table`, where `y` can be `NULL`
#' @param by Column names that can uniquely define `x` and `y`, in character vector
#' @param f Unquoted formula, applied to `x` amd `y`.  The result of `f` will be evaluate to find key factors
#' @param by_row TRUE/FALSE, whether to change all `var` in one row for testing
#' @export
#' @examples
#' DT_Year1 <- data.table::data.table(Branch = LETTERS[1:5], Premium = 100 * (1:5), LossRatio = c(.25, .2, .8, .3, .2))
#' DT_Year2 <- data.table::data.table(Branch = LETTERS[1:5], Premium = c(100, 200, 100, 400, 600), LossRatio = c(.25, .2, .2, .3, .2))
#' # test what drives Year2 loss ratio
#' qKeyFactor(DT_Year2, by = "Branch", f = sum(Premium * LossRatio) / sum(Premium), base_type = "nil")
#' qKeyFactor(DT_Year2, by = "Branch", f = sum(Premium * LossRatio) / sum(Premium), base_type = "mean")
#' # test that drives the change from Year1 to Year2
#' qKeyFactor(DT_Year1, DT_Year2, by = "Branch", f = sum(Premium * LossRatio) / sum(Premium))
qKeyFactor <- function(x, y = NULL, by, f, base_type = c("nil", "mean")) {
  f <- substitute(f)
  base_type <- match.arg(base_type)
  var <- all.vars(f)

  if (data.table::uniqueN(x, by = by) != nrow(x)) stop("`by` variables cannot uniquely identify x", call. = FALSE)
  if (!is.null(y) && data.table::uniqueN(y, by = by) != nrow(y)) stop("`by` variables cannot uniquely identify y", call. = FALSE)
  if (is.null(y)) {
    if (base_type == "nil") {
      y <- data.table::copy(x)[, (var) := 0]
    } else if (base_type == "mean") {
      y <- data.table::copy(x)[, (var) := lapply(.SD, mean, na.rm = TRUE), .SDcols = var]
    }
  }

  DT_ <- merge(
    x[, c(by, var), with = FALSE],
    y[, c(by, var), with = FALSE],
    by = by, all = TRUE, sort = TRUE
  )

  DT1_ <- DT_[, c(by, paste0(var, ".x")), with = FALSE]
  data.table::setnames(DT1_, paste0(var, ".x"), var)
  DT2_ <- DT_[, c(by, paste0(var, ".y")), with = FALSE]
  data.table::setnames(DT2_, paste0(var, ".y"), var)
  base1 <- eval(substitute(DT1_[, f], env = list(f = f)))

  if (length(base1) != 1) stop("`f` must yield single-value result.", call. = FALSE)

  res1_ <- data.table::copy(DT1_)[, ..by][, value := NA_real_]
  for (i in 1:nrow(DT_)) {
    DT3_ <- data.table::copy(DT1_)
    data.table::set(DT3_, i = i, j = var, value = DT2_[i, ..var])
    x <- eval(substitute(DT3_[, f], env = list(f = f)))
    data.table::set(res1_, i = i, j = "value", value = x)
  }
  res2_ <- res1_

  res2_[, base := base1][, diff := value - base]
  res <- res2_[order(-abs(diff))]

  res
}

