#' By-ref equivalent of \code{tidyr::separate}
#'
#' @param dt A \code{data.table} or \code{data.frame}.
#' @param col Column name to be split.
#' @param into Names of new variables to create as character vector.
#' @param sep Separator between columns.
#' @param remove If \code{TRUE}, remove input column from output data frame.
#' @param extra Controls when there are too many pieces after split.
#'
#' @export
#' @examples
#' df <- data.table(x = c("A_B_C", "D_E_F"))
#' dt_separate(copy(df), col = "x")[]
#' dt_separate(copy(df), col = "x", into = paste0("v", 1:2))[]
#' dt_separate(copy(df), col = "x", into = paste0("v", 1:2), extra = "merge")[]
#' dt_separate(copy(df), col = "x", into = paste0("v", 1:2), extra = "drop")[]
#' dt_separate(copy(df), col = "x", into = paste0("v", 1:4))[]

dt_separate <- function(dt, col, into = NULL, sep = "[^[:alnum:]]+", remove = TRUE, extra = c("complete", "merge", "drop")) {
  dt <- setDT(dt)
  extra <- match.arg(extra)

  data_split <- as.data.table(tstrsplit(dt[[col]], split = sep))
  n_split <- ncol(data_split)
  n_into <- length(into)

  if (n_split < n_into) {
    warning("Split columns are more than the length of `into`.  Unused `into` names will be dropped.", call. = FALSE)
    into <- into[seq_len(n_split)]
    n_into <- length(into)
  }

  raw_names <- paste0("...", seq_len(n_split))
  raw_names[seq_along(into)] <- into
  setnames(data_split, raw_names)

  if (n_split > n_into & n_into > 0) {
    if (extra == "merge") {
      cols_merge <-  seq(n_into, n_split)
      dt_unite(data_split, cols = cols_merge, into = "...merged...")
      setnames(data_split, into)
    } else if (extra == "drop") {
      cols_drop <- seq(n_into + 1, n_split)
      data_split[, (cols_drop) := NULL]
    }
  }

  dt[, colnames(data_split) := data_split]

  if (remove & !(col %in% into)) dt[, (col) := NULL]

  dt
}
#' By-ref equivalent of \code{tidyr::unite}
#'
#' @param DT A \code{data.table} or \code{data.frame}.
#' @param cols A char vector containing the colnames to be united.
#' @param into The name of the new column, as a string.
#' @param sep Separator to use between values.
#' @param remove If \code{TRUE}, remove input columns from output data frame.
#' @export
#' @examples
#' df <- as.data.table(expand.grid(x = c("a", NA), y = c("b", NA)))
#' dt_unite(copy(df), c("x", "y"), into = "new")[]
#' dt_unite(copy(df), c("x", "y"), into = "new", sep = "...", remove = FALSE)[]
#' dt_unite(copy(df), into = "new")[]
#' dt_unite(copy(df), into = "new", na.rm = TRUE)[]
dt_unite <- function(DT, cols = NULL, into, sep = "_", remove = TRUE, na.rm = TRUE) {
  DT <- setDT(DT)

  if (is.null(cols)) cols <- paste(names(DT)) # use paste, otherwise colnames is by-ref and changing

  if (na.rm) {
    tDT <- transpose(DT[, ..cols])
    united <- vapply(tDT, function(x) paste0(x[!is.na(x)], collapse = sep), character(1L))
    DT[, (into) := fifelse(united == "", NA_character_, united)]
  } else {
    DT[, (into) := do.call(paste, c(.SD, sep = sep)), .SDcols = cols]
  }

  if (remove & length(setdiff(cols, into)) > 0) DT[, setdiff(cols, into) := NULL]

  DT
}

#' Helper function to apply a function to multiple cols
#'
#' @inheritParams cnames
#' @export
#' @examples
#' x <- data.table::data.table(A = LETTERS[1:3], B = 5:7, C = 9:11)
#' dt_setcols(copy(x), c("B", "C"), function(x) x * 100)[]
#' dt_setcols(copy(x), 2:3, function(x) x * 100)[]
#' dt_setcols(copy(x), B:C, function(x) x * 100)[]
#' dt_setcols(copy(x), is.numeric, function(x) x * 100)[]
#' dt_setcols(copy(x), patterns("B|C"), function(x) x * 100)[]
#' dt_setcols(copy(x), !is.character, function(x) x * 100)[]
#' dt_setcols(copy(x), .(2:3, "B"), function(x) x * 100)[]
dt_setcols <- function(DT, cols, FUN, ...) {
  setDT(DT)

  cols_final <- cnames_q(DT, substitute(cols))

  if (length(cols_final) > 0) DT[, (cols_final) := lapply(.SD, FUN, ...), .SDcols = cols_final]

  DT[]
}
#' Lazy-evaluation version of `:=`
#'
#' Variable in DT is prefered than that in parent.frame
#'
#' @export
#' @examples
#' DT <- as.data.table(mtcars)
#' A <- 2
#' X <- 3
#' dt_set(copy(DT), A = mpg, B = A * 2, C = X / 3)
#' dt_set(copy(DT), A = mpg, B = mean(max(A * 2)), A = NULL, mpg = NULL)
dt_set <- function(DT, ...) {
  exprs <- as.list(substitute(...())) ## undocumented feature of substitute and ...
  nm <- names(exprs)

  for (i in seq_along(exprs)) {
    eval(substitute(DT[j = nm := vv], env = list(nm = nm[[i]], vv = exprs[[i]])))
  }

  return(DT[])
}

#' Patch new data y to old dataset x
#'
#' Essentially a safer way of doing left_join
#' @param x Data to be patched
#' @param y Patching dataset
#' @param by A character vector specifying common join ids
#' @param vars A character vector specifying common fields to be patched
#' @param ties Specify what to do when both \code{x} and \code{y} has valid values for \code{vars}
#' @export
#' @examples
#' dt1 <- data.table::data.table(ID1 = c(1, 1, 2, 2, 3), ID2 = c("A", "B", "A", "C", "B"), v1 = 1:5, v2 = 5:1)
#' dt2 <- data.table::data.table(ID1 = c(1, 2, 3), ID2 = "A", v1 = 11L, v2 = 22L)
#' dt_patch(dt1, dt2, by = c("ID1", "ID2"), vars = c("v1"))
#' dt_patch(dt1, dt2, by = c("ID1", "ID2"), vars = c("v1", "v2"))

dt_patch <- function(x, y, by, vars, ties = c("y", "x")) {
  x <- as.data.table(x)
  y <- as.data.table(y)
  ties <- match.arg(ties)
  if (!all(by %in% colnames(x))) stop("Not all variables in `by` are in x", call. = FALSE)
  if (!all(by %in% colnames(y))) stop("Not all variables in `by` are in y", call. = FALSE)
  if (!all(vars %in% colnames(x))) stop("Not all variables in `vars` are in x", call. = FALSE)
  if (!all(vars %in% colnames(x))) stop("Not all variables in `vars` are in y", call. = FALSE)
  if (anyDuplicated(y, by = by)) stop("Variables in `by` cannot uniquely defined data `y`", call. = FALSE)

  ## Joining
  y_cols <- c(by, vars)
  x_full <- merge(x, y[, ..y_cols], by = by, all.x = TRUE, all.y = FALSE, suffixes = c("...x", "...y"))

  ## Coalescing
  for (patch_var in vars) {
    var_x <- paste0(patch_var, "...x")
    var_y <- paste0(patch_var, "...y")
    value_x <- x_full[[var_x]]
    value_y <- x_full[[var_y]]

    if (typeof(value_x) != typeof(value_y)) {
      cli::cli_abort("Different types: {.field {var_x}} - {typeof(value_x)}; {.field {var_y}} - {typeof(value_y)}")
    }

    if (ties == "x") {
      data.table::set(x_full, j = patch_var, value = fcoalesce(value_x, value_y))
    } else {
      data.table::set(x_full, j = patch_var, value = fcoalesce(value_y, value_x))
    }
  }

  ## Delete ...x and ...y vars
  var_pairs <- data.table::CJ(var = vars, xy = c("...x", "...y"))[, paste0(var, xy)] ## e.g. A...x, A...y
  x_full[, (var_pairs) := NULL]

  ## Restore dataset x col sequence
  data.table::setcolorder(x_full, colnames(x))

  return(x_full[])
}
