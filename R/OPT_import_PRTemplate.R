#' Read from GC Pro-Rata Template to get Dev table
#'
#' @param wb Path of GC PR Treaty template workbook
#' @param LOB LOB name defined in template
#' @param LRSummary Name for LossRatioSummary sheet
#' @param Projection Name for Projection sheet
#' @param Large Name for LargeLossList sheet
#' @param Event Name for EventLossList sheet
#' @export
#' @examplesIf FALSE
#' import_PRTemplate_Dev(wb, "Property", "Loss Ratios")
#' import_PRTemplate_Dev(wb, c("Property", "MB"), "Loss Ratios")

import_PRTemplate_Dev <- function(wb, LOB, LRSummary, Projection = LOB, Large = "Large", Event = "CAT") {
  .lobs <- LOB

  DT_LRSummary <- import_PRTemplate_LRSummary(wb, LRSummary)[LOB %in% .lobs, .(LOB, UY, UltPrem, UltIncurred, UltLarge, UltEvent)] |>
    data.table::setkeyv(c("LOB", "UY"))

  raw_Projection <- setNames(nm = .lobs) |>
    lapply(import_PRTemplate_Projection, wb = wb) |>
    data.table::rbindlist(idcol = "LOB")

  DT_Projection <- raw_Projection[, .(LOB, UY, ActualPrem, ActualPaid, ActualIncurred)] |>
    data.table::setkeyv(c("LOB", "UY"))

  DT_Large <- import_PRTemplate_LargeLossSummary(wb, Large)[LOB %in% .lobs, .(LOB, UY, ActualLarge)] |>
    data.table::setkeyv(c("LOB", "UY"))

  DT_Event <- import_PRTemplate_EventLossSummary(wb, Event)[LOB %in% .lobs, .(LOB, UY, ActualEvent)] |>
    # rbind(data.table::data.table(UY = "Projected"), fill = TRUE) |>
    data.table::setkeyv(c("LOB", "UY"))

  res <- DT_Projection[DT_Large][DT_Event][DT_LRSummary]
  data.table::setcolorder(res, c("LOB", "UY", "ActualPrem", "ActualPaid", "ActualIncurred", "ActualLarge", "ActualEvent", "UltPrem", "UltIncurred", "UltLarge", "UltEvent"))
  # res[, LOB := factor(LOB, .lobs)]
  # data.table::setorderv(res, c("LOB", "UY"))
  res[]
}

#' @export
import_PRTemplate_Projection <- function(wb, ws) {
  DT_raw <- readxl_raw(wb, ws, rows = c(8, NA), cols = "B:V")
  DT_raw[, V1 := stringi::stri_trim_both(V1)]
  year_range <- DT_raw[grepl("^\\d+$", V1), as.integer(unique(V1))]
  n_year <- length(year_range)

  # Read 3 blocks
  Block_Prem <- DT_raw[seq(4, length.out = n_year)]
  Block_exclMega <- DT_raw[seq(4 + (n_year + 6), length.out = n_year)]
  Block_inclMega <- DT_raw[seq(4 + 2 * (n_year + 6), length.out = n_year)]

  res <- cbind(
    Block_Prem[, .(UY = V1, ActualPrem = V2, UltPrem = V6)],
    Block_inclMega[, .(ActualPaid = V12, ActualIncurred = V14, UltIncurred = V18)]
  )

  num_cols <- res[, names(.SD), .SDcols = -1L]
  res[, (num_cols) := lapply(.SD, as.double), .SDcols = num_cols]
  res[]
}

#' @export
import_PRTemplate_LRSummary <- function(wb, ws) {
  DT_raw <- readxl_raw(wb, ws, rows = c(8, NA), cols = "B:M")
  DT_raw <- DT_raw[, V1 := stringi::stri_trim_both(V1)][!is.na(V1) & V1 != ""]
  year_range <- DT_raw[grepl("^\\d+$", V1), as.integer(unique(V1))]

  DT_raw[, grp := vec_pick(V1, "UW Year", offset = -1L, fill_direction = "down")]
  res <- DT_raw[
    V1 %in% c(year_range, "Projection"),
    .(LOB = grp, UY = V1, UltPrem = V2, UltIncurred = V3, UltLarge = V6, UltEvent = V4,
      ULR = V8, LargeLR = V11, EventLR = V9)
  ]

  num_cols <- res[, names(.SD), .SDcols = -(1:2)]
  res[, (num_cols) := lapply(.SD, as.double), .SDcols = num_cols]

  res[UY == "Projection", `:=`(UltIncurred = UltPrem * ULR, UltLarge = UltPrem * LargeLR, UltEvent = UltPrem * EventLR)]
  res[]
}

#' @export
import_PRTemplate_EventLossSummary <- function(wb, ws = "CAT") {
  DT_raw <- readxl_raw(wb, ws, rows = c(8, NA), cols = "B:I")
  DT_raw <- DT_raw[, V1 := stringi::stri_trim_both(V1)][!is.na(V1) & V1 != ""]

  DT_raw[, grp := vec_pick(V1, "UW Year", offset = -1L, fill_direction = "down")]

  res <- DT_raw[
    grepl("^\\d+$", V1),
    .(LOB = grp, UY = V1, UltPrem = V2, ActualEvent = V3, UltEvent = V8, ClaimCount = V4, Severity = V5)
  ]

  num_cols <- res[, names(.SD), .SDcols = -(1:2)]
  res[, (num_cols) := lapply(.SD, as.double), .SDcols = num_cols]
  res[]
}

#' @export
import_PRTemplate_LargeLossSummary <- function(wb, ws = "Large") {
  DT_raw <- readxl_raw(wb, ws, rows = c(8, NA), cols = "B:I")
  DT_raw <- DT_raw[, V1 := stringi::stri_trim_both(V1)][!is.na(V1) & V1 != ""]

  DT_raw[, grp := vec_pick(V1, "UW Year", offset = -1L, fill_direction = "down")]
  DT_raw[, threshold := vec_pick(V1, "UW Year", y = V4, offset = -1L, fill_direction = "down")]

  res <- DT_raw[
    grepl("^\\d+$", V1),
    .(LOB = grp, UY = V1, UltPrem = V2, ActualLarge = V3, UltLarge = V8, ClaimCount = V4, Severity = V5, threshold)
  ]

  num_cols <- res[, names(.SD), .SDcols = -(1:2)]
  res[, (num_cols) := lapply(.SD, as.double), .SDcols = num_cols]
  res[]
}

#' @export
import_PRTemplate_Term <- function(wb, ws) {
  terms_Structure <- readxl(wb, ws, range = "B3:H7")[, .SD[, -2], .SDcols = !patterns("^V\\d+$")] |>
    data.table::transpose(keep.names = "LOB", make.names = 1L) |>
    data.table::setnames(c("LOB", "GrossRetention", "QS", "Lines", "TreatyCapacity"))

  terms_EventLimit <- readxl_list(wb, ws, range = "B8:C10") |> lapply(as.numeric) |> setNames(c("WF", "EQ", "Other"))

  tmp_RightClosed <- readxl_vector(wb, ws, "N3")
  tmp_Provisional <- readxl_vector(wb, ws, "N2", "numeric")
  terms_Comm <- list(
    FixedComm = readxl_vector(wb, ws, "C23", "numeric"),
    Tax = readxl_vector(wb, ws, "C24", "numeric"),
    RightClosed = length(tmp_RightClosed) == 0 || tmp_RightClosed == "Up to and Incl.",
    Provisional = ifelse(length(tmp_Provisional), NA_real_, tmp_Provisional)
  )

  terms_PC <- readxl_list(wb, ws, range = "B21:C22") |> lapply(as.numeric) |> setNames(c("PC", "ME"))

  tmp_LPC <- readxl(wb, ws, range = "B17:D20", col_names = FALSE)[-2, 2:3] |>
    data.table::transpose() |>
    data.table::setnames(c("LPC", "Lbound", "Ubound")) |>
    na.omit()

  terms_LPC <- rbind(
    data.table::data.table(LPC = 0, Lbound = 0, Ubound = tmp_LPC[, min(Lbound)]),
    tmp_LPC,
    data.table::data.table(LPC = 0, Lbound = tmp_LPC[, max(Ubound)], Ubound = Inf)
  ) |> data.table::setcolorder(c("Lbound", "Ubound", "LPC"))

  terms_SS <- readxl(wb, ws, rows = c(5, NA), cols = "L:N", col_names = c("LowerLR", "UpperLR", "Comm")) |> na.omit()

  res <- list(
    Structure = terms_Structure,
    Comm = terms_Comm,
    Sliding = terms_SS,
    PC = terms_PC,
    LPC = terms_LPC,
    EventLimit = terms_EventLimit
  )

  res
}

import_PRTemplate_Portfolio <- function(wb, ws = "Portfolio") {

}
