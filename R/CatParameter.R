#' CatParameter S3 class
#'
#' Create S3 class for CatParameter data type
#'
#' CatParameter usually include parameters as follows:
#' \itemize{
#'   \item Occupancy
#'   \item Coverage
#'   \item ConstructionType
#'   \item BuildingHeight
#'   \item YearBuilt
#'   \item LossTerm (e.g. Deductible and Sublimit)
#' }
#'
#' Above are also valid names for input \code{x}
#'
#' @param x A named list of \code{data.frames}, each being one parameter table (see Details for more information)
#'
#' @examples
#' prm1 <- CatParameter(param_long)
#' prm1
#' prm2 <- CatParameter(param_wide)
#' all.equal(prm1, prm2)
#' filter_Occupancy(prm1, c("Commercial", "Industrial"))
#'
#' @export
CatParameter <- function(x) {
  res <- validate_CatParameter(new_CatParameter(x))
  return(res)
}

#' @describeIn CatParameter Get a subset of \code{CatParameter} with specified Occupancy types
#' @export
filter_occupancy <- function(x, i) {
  x <- checkmate::assert_class(x, "CatParameter")
  i <- checkmate::assert_character(i)

  res <- lapply(x, function(prm) prm[Occupancy %in% i])
  res[["Occupancy"]][, p := p / sum(p, na.rm = TRUE)][]
  res <- CatParameter(res)
  return(res)
}

new_CatParameter <- function(x) {
  x <- checkmate::assert_list(x, names = "unique", .var.name = paste0("input ", deparse(substitute(x))))
  names_seq <- c("Occupancy", "Coverage", "ConstructionType", "BuildingHeight", "YearBuilt", "LossTerm")
  seq_new <- intersect(names_seq, names(x))
  res <- x[seq_new]
  class(res) <- c("CatParameter", "list")
  return(res)
}

validate_CatParameter <- function(x) {
  checkmate::assert_names(
      names(x),
      type = "unique",
      subset.of = c("Occupancy", "Coverage", "ConstructionType", "BuildingHeight", "YearBuilt", "LossTerm"),
      must.include = c("Occupancy", "Coverage")
    )

  # get occupancy types
  if ("Occupancy" %in% names(x$Occupancy)) {
    types_occupancy <- unique(x$Occupancy$Occupancy)
  } else {
    ## wide form
    types_occupancy <- unique(names(x$Occupancy))
  }

  for (i in seq_along(x)) {
    prm_name <- names(x)[i]
    df <- data.table::as.data.table(checkmate::assert_data_frame(x[[i]]))
      #if (prm_name == "LossTerm") browser()

    ## if in wide-form, convert to long
    if (all(types_occupancy %in% names(df))) {
      ##  convert to long form
      df <- data.table::melt(df, id.var = prm_name, variable.name = "Occupancy", variable.factor = FALSE, value.name = "p")
    }

    if ("p" %in% names(df)) df[, p := as.numeric(p)]
    if ("p" %in% names(df) & prm_name == "LossTerm") df <- data.table::dcast(df, ... ~ LossTerm, value.var = "p")
    ## checks names
    if (prm_name == "LossTerm") {
      checkmate::assert_names(names(df), subset.of = c("Occupancy", "Deductible_Amount", "Deductible_Percentage", "Sublimit_Percentage"), must.include = "Occupancy")
    } else {
      checkmate::assert_names(names(df), permutation.of = c("Occupancy", prm_name, "p"))
    }

    ## check sum
    tol <- .0001
    if (prm_name == "Occupancy") {
      sum_df <- df[, .(p = sum(p, na.rm = TRUE))]
      if (sum_df[, abs(p - 1) > tol]) cli::cli_abort("{prm_name}'s breakdown percentages cannot add up to 1")
    } else if (prm_name %in% c("Coverage", "ConstructionType", "BuildingHeight", "YearBuilt")) {
      sum_df <- df[, .(p = sum(p, na.rm = TRUE)), by = "Occupancy"]
      if (sum_df[, any(abs(p - 1) > tol)]) cli::cli_abort("{prm_name}'s {sum_df[abs(p - 1) > tol, Occupancy]} cannot add up to 1")
    }

    ## write back
    x[[i]] <- df
  }

  return(x)
}

#' @export
print.CatParameter <- function(x, verbose = FALSE, ...) {
  if (verbose) {
    NextMethod("print", x)
  } else {
    cli::cat_line("<CatParameter>")
    cli::cat_line("  - Parameter sets: [", length(x), "]")
    for (ii in seq_along(x)) {
      cli::cat_line("     ", ii, ". ", names(x)[ii])
    }
    cli::cat_line("  - Occupancy: ", x[["Occupancy"]][, paste(Occupancy, collapse = ", ")])
    invisible(x)
  }
}
