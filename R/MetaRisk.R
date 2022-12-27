#' Read `.eld` file as a list of `data.table`s
#' @export
mr_eld_read <- function(file) {
  # Read as vector
  rawtxt <- data.table::fread(file, sep = NULL, header = FALSE, showProgress = FALSE, integer64 = "numeric")[[1L]]

  rownumber_hash <- which(stringi::stri_startswith_fixed(rawtxt, "#"))
  if (length(rownumber_hash) == 0) return(NULL)

  row_from <- rownumber_hash + 1 ## omit the metarow #SeverityMetadata
  row_to <- c(rownumber_hash[-1] - 1, length(rawtxt))

  list_names <- stringi::stri_replace_first_fixed(rawtxt[rownumber_hash], "#", "") |>
    stringi::stri_trim_both()

  rawList <- Map(function(from, to) rawtxt[from:to], from = row_from, to = row_to) |>
    setNames(list_names)
  # Clean up each component
  .to_DT <- function(x, col.names = NULL) {
    text <- stringi::stri_c(x, collapse = "\n") ## much faster to split single char with \n, than with char vector
    if (is.null(col.names)) {
      res <- data.table::fread(text = text, sep = "\t", header = TRUE, integer64 = "numeric", showProgress = FALSE)
    } else {
      res <- data.table::fread(text = text, sep = "\t", header = FALSE, col.names = col.names, integer64 = "numeric", showProgress = FALSE)
    }
    res
  }

  res <- vector("list")
  if ("Severities" %in% list_names) {
    .text <- rawList[["Severities"]]
    header <- stringi::stri_split_fixed(.text[1L], "\t")[[1]]
    n_LC <- as.integer(header[1L])
    header <- header[header != ""][-1L] ## 1st is the number of LossCauses
    header <- onepunch::makenm_clean(header)

    n_col <- length(unlist(stringi::stri_split_fixed(.text[2L], "\t")))

    if (n_col == n_LC + 1) {
      # AIR case
      header_final <- c("EventID", header)
    } else if (n_col - 1 == n_LC * 4) {
      header_final <- data.table::CJ(header = header, var = c("Mean", "SDI", "SDC", "TSI"), sorted = FALSE)[, paste0(header, "_", var)]
      header_final <- c("EventID", header_final)
    } else {
      stop("No. of severity columns is not consistent with no. of loss causes", call. = FALSE)
    }

    res[["severity"]] <- .to_DT(.text[-1L], col.names = header_final)
    .text <- NULL
  }

  if ("Rates" %in% list_names) res[["rate"]] <- .to_DT(rawList[["Rates"]], col.names = c("EventID", "Rate"))

  if ("Scenarios" %in% list_names) res[["scenario"]] <- .to_DT(rawList[["Scenarios"]])

  if ("SeverityMetadata" %in% list_names) {
    .text <- rawList[names(rawList) == "SeverityMetadata"]
    .res <- lapply(.text, .to_DT, col.names = c("v1", "v2")) |>
      lapply(data.table::transpose, make.names = "v1") |>
      data.table::rbindlist()
    .res[, NewName :=  onepunch::makenm_clean(Name)]
    data.table::setcolorder(.res, c("Name", "NewName"))
    .res[, AAL := as.numeric(AAL)]
    if ("Count" %in% names(.res)) .res[, Count := as.integer(Count)]

    res[["meta.severity"]] <- .res
    .res <- NULL
    .text <- NULL
  }

  if ("RatesMetadata" %in% list_names) {
    res[["meta.rate"]] <- .to_DT(rawList[["RatesMetadata"]], col.names = c("v1", "v2")) |>
      data.table::transpose(make.names = "v1")
  }

  res
}

#' @export
mr_build_script <- function(x, ...) {
  UseMethod("mr_build_script")
}
