#' Convert a `data.table` object to `triangle`
#' @export
#' @examples
#' prem <- as_triangle(TRI$Premium)
#' loss <- as_triangle(TRI$Incurred)
#' loss / prem
#' DT_Prem_withNA <- data.table::copy(TRI$Premium)[Year == 2009, `1` := NA]
#' as_triangle(DT_Prem_withNA)
#' as_triangle(DT_Prem_withNA, fill = FALSE)

as_triangle <- function(DT, origin = 1L, fill = TRUE) {
  DT <- data.table::as.data.table(DT)
  data.table::setnames(DT, 2:ncol(DT), paste0(1:(ncol(DT) - 1)))
  mat <- data.table:::as.matrix.data.table(DT, rownames = origin)
  names(attr(mat, "dimnames")) <- c("Year", "Dev")

  if (fill & (nrow(mat) == ncol(mat))) {
    idx <- lower.tri(mat, diag = TRUE)[ncol(mat):1, ]
    mat[idx & (is.na(mat))] <- 0
  }

  res <- structure(mat, class = "triangle")
  res
}

as.data.table.triangle <- function(tri, ...) {
  res <- data.table:::as.data.table.matrix(keep.rownames = "Year")
  res
}

#' Calculate link ratio (i.e. age-to-age increment development factor)
#' @export
#'
dev_link_ratio <- function(tri, style = c("weighted", "simple")) {
  if (!inherits(tri, "triangle")) stop("`tri` must be a `triangle` object.  See `?as_triangle` for more information.", call. = FALSE)
  style <- match.arg(style)

  tri_from <- tri[, -ncol(tri)]
  tri_to <- tri[, -1L]

  if (style == "simple") {
    yty <- tri_to / tri_from
    yty[is.infinite(yty)] <- NA_real_
    res <- colMeans(yty, na.rm = TRUE)
  } else if (style == "weighted") {
    idx <- is.na(tri_to) | is.na(tri_from)
    tri_to[idx] <- NA_real_
    tri_from[idx] <- NA_real_
    res <- colSums(tri_to, na.rm = TRUE) / colSums(tri_from, na.rm = TRUE)
  }

  res
}

#' Complete the input triangle using ChainLadder method
#' @export
#' @examples
#' loss <- as_triangle(TRI$Incurred)
#' dev_ChainLadder(loss)
dev_ChainLadder <- function(tri, link_ratio = "weighted", floor = 1, tail_factor = 1) {
  stopifnot(inherits(tri, "triangle"))

  # determine age-to-age factor (ata)
  if (is.numeric(link_ratio)) {
    ata <- link_ratio
    length(ata) <- ncol(tri) - 1
    ata[is.na(ata)] <- 1
  } else if (is.function(link_ratio)) {
    ata <- link_ratio(tri)
    if (!is.numeric(ata) | length(ata) != ncol(tri) - 1) stop("When provided as a function, `link_ratio` must output a numeric vector with length of `ncol(tri) - 1`")
  } else {
    if (link_ratio == "weighted") {
      ata <- dev_link_ratio(tri, "weighted")
    } else if (link_ratio == "simple") {
      ata <- dev_link_ratio(tri, "simple")
    }
  }

  ata[is.na(ata)] <- 1
  inc_ata <- c(pmax(floor, ata), tail_factor) ## incremental with tail factor

  # cum_ata <- rev(cumprod(rev(inc_ata))) ## cumulative

  # loop through columns (i.e. dev)
  res <- tri_fill_lower(tri, inc_ata)

  # rename
  names(inc_ata) <- paste0(colnames(res)[-ncol(res)], ":", colnames(res)[-1L])

  # attr
  attr(res, "link.ratio") <- inc_ata
  res
}

#' Complete the input triangle using CapeCod B-F method
#' @export
#' @examples
#' prem <- as_triangle(TRI$Premium)
#' ultprem <- dev_ChainLadder(prem)[, ncol(prem) + 1]
#' loss <- as_triangle(TRI$Incurred)
#' dev_CapeCod(loss, ultprem)
dev_CapeCod <- function(tri_loss, ultimate_premium, ELR = NULL, ...) {
  stopifnot(inherits(tri_loss, "triangle"))
  if (nrow(tri_loss) != length(ultimate_premium)) stop("nrow(tri_loss) and length(tri_premium) must the same", call. = FALSE)

  # loss
  rect_loss <- dev_ChainLadder(tri_loss, ...)
  ult_loss <- rect_loss[, ncol(rect_loss)]
  link_ratio <- attr(rect_loss, "link.ratio")
  cum_factor <- rev(cumprod(rev(link_ratio)))
  report_pct <- rev(1 / cum_factor) # rev so that the earlist year comes first

  # ELR for CapeCod
  if (is.null(ELR)) {
    latest_loss <- tri_latest(tri_loss)
    ELR_capecod <- sum(latest_loss) / sum(ultimate_premium * report_pct)
  } else if (is.numeric(ELR)) {
      if (length(ELR) == 1 | length(ELR) == nrow(tri_loss)) {
        ELR_capecod <- EL
      } else {
        stop("If `ELR` is provided as a numeric vector, it must has length 1 or the same as nrow of `tri_loss`", call. = FALSE)
      }
  } else {
      stop("`ELR` only accept `NULL` or numeric vector", call. = FALSE)
  }

  # equivalent ChainLadder LDF
  # for B-F: UltLoss = ActLoss + (1 - ReportedPerc) * ELR * UltPrem
  # for CL:  UltLoss = ActLoss * CDF
  # so CDF = 1 + (1 - Perc) * ELR * UltPrem / ActLoss

  ult_loss <- latest_loss + (1 - report_pct) * ELR_capecod * ultimate_premium

  res <- tri_fill_lower(tri_loss, report_pct = report_pct, ultimate_loss = ult_loss)

  # attr
  attr(res, "ELR") <- ELR_capecod
  attr(res, "ultimate.premium") <- ultimate_premium
  attr(res, "link.ratio") <- link_ratio
  res
}

#' Get the latest values from a triangle
#' @export

tri_latest <- function(tri) {
  stopifnot(inherits(tri, "triangle"))
  res <- unlist(apply(tri, 1L, function(x) data.table::last(na.omit(x)), simplify = FALSE))
  res
}

tri_fill_lower <- function(tri, inc_ata = NULL, report_pct = NULL, ultimate_loss = NULL) {
  if (!is.null(inc_ata)) {
    # loop through columns (i.e. dev)
    res <- cbind(tri, NA_real_)
    for (j in seq_len(ncol(tri))) {
      col_from <- res[, j]
      col_to <- res[, j + 1]
      res[is.na(col_to), j + 1] <- (col_from * inc_ata[j])[is.na(col_to)]
    }
  } else if (!is.null(report_pct) & !is.null(ultimate_loss)) {
    res <- cbind(tri, ultimate_loss)
    for (j in rev(seq_len(ncol(tri)))) {
      col_from <- res[, j + 1]
      col_to <- res[, j] ## backward fill
      res[is.na(col_to), j] <- (col_from * rev(report_pct)[j])[is.na(col_to)]
    }
  }

  # rename
  colnames(res) <- c(colnames(tri), "ultimate")
  res
}
