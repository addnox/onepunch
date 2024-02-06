#' Data Analysis function to find key factors
#'
#' @param x,y A `data.table`, where `y` can be `NULL`
#' @param by Column names that can uniquely define `x` and `y`, in character vector
#' @param f Unquoted formula, applied to `x` amd `y`.  The result of `f` will be evaluate to find key factors
#' @param auto_sum TRUE/FALSE, whether to sum all vars if `by` cannot uniquely define each row for `x` and `y`
#' @param marginal TRUE/FALSE, whether to calculate marginal effect for each row of records.  Note that calculating marginal effect
#'                 can be high computationally expensive
#' @export
#' @examples
#' DT_Year1 <- data.table::data.table(Branch = LETTERS[1:5], Premium = 100 * (1:5), LossRatio = c(.25, .2, .8, .3, .2))
#' DT_Year2 <- data.table::data.table(Branch = LETTERS[1:5], Premium = c(100, 200, 100, 400, 600), LossRatio = c(.25, .2, .2, .3, .4))
#' # test what drives Year2 loss ratio
#' qKeyFactor(DT_Year2, by = "Branch", f = sum(Premium * LossRatio) / sum(Premium), base_type = "nil")
#' qKeyFactor(DT_Year2, by = "Branch", f = sum(Premium * LossRatio) / sum(Premium), base_type = "mean")
#' # test that drives the change from Year1 to Year2
#' qKeyFactor(DT_Year1, DT_Year2, by = "Branch", f = sum(Premium * LossRatio) / sum(Premium))
qKeyFactor <- function(x, y = NULL, by, f, base_type = c("nil", "mean"), auto_sum = TRUE, marginal = TRUE) {
  f <- substitute(f)
  base_type <- match.arg(base_type)
  var <- all.vars(f)

  if (data.table::uniqueN(x, by = by) != nrow(x)) {
    if (auto_sum) {
      x <- x[, lapply(.SD, sum, na.rm = TRUE), .SDcols = var, by = by]
    } else {
      stop("`by` variables cannot uniquely identify x", call. = FALSE)
    }
  }

  if (!is.null(y) && data.table::uniqueN(y, by = by) != nrow(y)) {
    if (auto_sum) {
      y <- y[, lapply(.SD, sum, na.rm = TRUE), .SDcols = var, by = by]
    } else {
      stop("`by` variables cannot uniquely identify y", call. = FALSE)
    }
  }

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
  base2 <- eval(substitute(DT2_[, f], env = list(f = f)))
  if (length(base1) != 1) stop("`f` must yield single-value result.", call. = FALSE)



  loop_DT <- function(DT1, DT2) {
    res <- data.table::copy(DT1)[, ..by][, value := NA_real_]

    for (i in 1:nrow(DT1)) {
      DT1_copy <- data.table::copy(DT1)
      data.table::set(DT1_copy, i = i, j = var, value = DT2[i, ..var]) ## change DT1's value to DT2's
      x <- eval(substitute(DT1_copy[, f], env = list(f = f)))
      data.table::set(res, i = i, j = "value", value = x)
    }

    res
  }

  if (marginal == TRUE) {
    res1_ <- data.table::copy(DT1_)[, ..by][, `:=`(id = NA_integer_, value = NA_real_)]
    moving_base <- base1
    for (i in 1:nrow(DT1_)) {
      if (all.equal(DT1_, DT2_) == TRUE) {
        res1_[is.na(id), id := 1 + max(res1_[, id], na.rm = TRUE)]
        res1_[is.na(value), value := base2]
        break
      }

      tmp_res <- loop_DT(DT1_, DT2_)
      change_row <- tmp_res[, which.max(abs(value - moving_base))]
      data.table::set(DT1_, i = change_row, j = var, value = DT2_[change_row, ..var])
      data.table::set(res1_, i = change_row, j = c("id", "value"), value = list(i, tmp_res[change_row, value]))
      moving_base <- eval(substitute(DT1_[, f], env = list(f = f)))
    }

    res <- res1_[order(id)]
    res[, `:=`(base = base1, new = base2)][, `:=`(diff = diff(c(0, value - base)), cumdiff = value - base, id = NULL)] # still calc diff first to keep the colorder
  } else {
    res <- loop_DT(DT1_, DT2_)[order(-abs(value - base1))]
    res[, `:=`(base = base1, new = base2)][, `:=`(diff = value - base)]
  }

  res[]
}

