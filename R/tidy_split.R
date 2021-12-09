
#' Split data.table into list
#'
#' tidy split functions return a list of split data blocks
#'
#' \describe{
#'   \item{position}{
#'       \itemize{
#'         \item \code{start} The matched position marks the START of one block
#'         \item \code{end} The matched position marks the END of one block
#'       }
#'   }
#' }
#'
#' @param x A data.frame
#' @param cols Numeric vector (indicating position) or character vector (indicating colnames)
#' @param trim Use \code{tidy_trim} for each element of output list
#' @param na.rm When TRUE, those rows with all \code{cols} being \code{NA} will be deleted before splitting
#' @export
#' @examples
#' DT <- data.table(ID1 = c(NA, "A", "A", NA, NA, "B"), ID2 = c(rep("1", 4), "2", "2"), X1 = NA, X2 = c(NA, 1:2, NA, NA, 3), X3 = c(rep(NA, 5), .1))
#' tidy_hsplit(DT[, -2L])
#' tidy_hsplit(DT[, -2L], trim = TRUE)
#' tidy_hsplit(DT, cols = c("ID1", "ID2"))

#' @keywords internal
tidy_hsplit_ <- function(x, cols = NULL, trim = FALSE, na.rm = TRUE) {
  # tidy_hsplit_q is the escape version.  It preserves rownames mainly for tidy_vsplit

  x1 <- as.data.table(x)
  selected_cols <- cnames_q(x1, cols) ## NULL means all columns

  idx_blank <- rowSums(!is.na(as.matrix(x1))) == 0
  x1[, ...rn... := rownames(x1)] ## once added, there will be no all-blank rows

  ## get break positions
  if (is.null(cols)) {
    ## use blank rows to split
    idx_blocks <- data.table::data.table(data.table::rleidv(idx_blank))
  } else {
    ## use repeat values
    idx_blocks <- x1[, ..selected_cols]
  }

  # clean up blanks
  by_vars <- paste0("...by", seq_along(idx_blocks))
  setnames(idx_blocks, by_vars)

  ## split
  x2 <- cbind(x1, idx_blocks)[!idx_blank] ## all-blank rows will be deleted automatically

  if (na.rm) {
    all_NA_ids <- rowSums(is.na(x2[, ..by_vars])) == length(by_vars)
    x2 <- x2[!all_NA_ids]
  }

  res <- split(x2, by = by_vars, drop = TRUE, keep.by = FALSE) ## use by.  Using f will result in re-ordered list elements

  ## delete names for blank cuts
  if (is.null(cols)) {
    setattr(res, "names", NULL)
  }

  ## delete rowname column
  res <- lapply(res, function(x) {setattr(x, "row.names", x[["...rn..."]]); x[, ...rn... := NULL]; x})

  ## trim each element, mainly for all-blank columns
  if (trim) res <- lapply(res, tidy_trim)
  res
}

#' Split data into list of \code{data.table}s horizontally or vertically
#' @export
tidy_hsplit <- function(x, cols = NULL, trim = FALSE) {
  res <- tidy_hsplit_(x, cols = cols, trim = trim)
  res <- lapply(res, function(x) {setattr(x, "row.names", seq_len(nrow(x))); x})
  res
}

#' @rdname tidy_hsplit
#' @export
tidy_vsplit <- function(x, rows = NULL, trim = FALSE) {
  stopifnot(is.integer(rows) | is.null(rows))
  x <- as.data.table(x)
  x1 <- data.table::transpose(x)
  rownames(x1) <- names(x)

  res_t <- tidy_hsplit_(x1, cols = rows,  trim = trim)
  res <- lapply(res_t, function(x) setnames(data.table::transpose(x), rownames(x)))
  res <- lapply(res, function(x) {setattr(x, "row.names", seq_len(nrow(x))); x})
  res
}
