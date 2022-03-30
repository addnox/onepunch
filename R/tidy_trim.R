#' Trim all-blank row and cols of a data.table
#'
#' @param DT A \code{data.table}
#' @export
#' @examples
#' DT <- data.table(A = c(NA, 1, 2, NA, NA, 3), B = NA, C = c(NA, "A", NA, NA, NA, "B"), D = NA)
#' tidy_trim(DT)

tidy_trim <- function(DT) {
  m <- as.matrix(data.table::as.data.table(DT))
  m_na <- is.na(m)

  rows_allna <- rowSums(!m_na) == 0
  cols_allna <- colSums(!m_na) == 0

  res <- DT[!rows_allna, !..cols_allna]
  rownames(res) <- rownames(DT)[!rows_allna]
  res
}
