#' Common contract terms
#'
#' @description
#' `term_Excess` calculates LossInLayer based on specified Limit and Deductible structure
#'
#' `term_Commission` calculates commssion figures based on given sliding scale table
#'
#' `term_Reinstatement` returns a list of two vectors, one being reinstated loss and the other is reinstated premium
#'
#' `term_ProfitCommission` calculates profit commission
#'
#' `term_LossParcipation` calculates loss participation (i.e. loss corridor) from cedent's perspective
#' @details
#' Many terms are plain-vanilla and are recommended to use base R calculations.  Only complicated terms are implemented.
#' @export
#' @param Loss,Premium Numeric vector for loss and premium
#' @param Limit,Deductible Limit and Deductible terms
#' @param Participation Used to multiply with final loss to layer
#' @param is.FranchiseLimit Logical, franchise limit means when LossAboveDeducible is greater than Limit, then no reimbursement
#' @param is.FranchiseDed Logical, franchise deductible means when Loss is greater than Deductible, full f.g.u. loss is applicable for reimbursement
#' @param is.StopLoss Logical, when TRUE, `Limit` and `Deductible` are deemed as in loss ratio form.  But `Loss` and final output are still be amount form
term_Excess <- function(Loss, Premium = NULL, Limit, Deductible, Participation = 1, is.FranchiseLimit = FALSE, is.FranchiseDed = FALSE, is.StopLoss = FALSE) {
  # for agg Excess, sum by year and then use term_Excess again
  if (is.StopLoss) {
    # Loss will be in LR form, and Limit/Ded in % form
    if (is.null(Premium)) stop("`Premium` cannot be NULL when `is.StopLoss` is TRUE")
    Limit <- Limit * Premium
    Deductible <- Deductible * Premium
  }

  # above Ded
  if (is.FranchiseDed) {
    x1 <- Loss * (Loss > Deductible) ## same as MR behavior: Loss == Deductible won't trigger
  } else {
    x1 <- pmax(0, Loss - Deductible)
  }

  # cap by Limit
  # Franchise Limit means if beyond fguLimit, loss to layer will be 0
  if (is.FranchiseLimit) {
    x2 <- pmin(Limit, x1) * (x1 <= Limit) ## same as MR: LossInLayer == Limit will still be reimbursed
  } else {
    x2 <- pmin(Limit, x1)
  }

  res <- x2 * Participation
  res
}

#' @export
#' @rdname term_Excess
#' @param Commission A 3-column `data.frame`, with the sequence of `LowerLR`, `UpperLR` and `Commission`
#' @param right.closed Logical, default to be TRUE.  RightBound inclusive
#'
term_Commission <- function(Loss, Premium, Commission, right.closed = TRUE) {
  # Comm can be a number, then fixed Comm
  # Comm can be a 3-column DF, with name LowerLR, UpperLR and Commission
  if (!is.numeric(Premium)) stop("`Premium` must be a numeric vector", call. = FALSE)
  #
  if (is.data.frame(Commission) & ncol(Commission) == 3 & all(lapply(Commission, class) == "numeric")) {
    if (!(all(abs(Commission[[1]][-1L] - data.table::shift(Commission[[2]])[-1L]) <= .0001))) {
      stop("LowerLR must be the same as the preceding UpperLR", call. = FALSE)
    }

    .SS <- data.table::as.data.table(Commission) |> data.table::setnames(c("LowerLR", "UpperLR", "Commission"))
    .LR <- Loss / Premium
    band_idx <- cut(.LR, breaks = c(0, Commission[[2]]), labels = FALSE, include.lowest = TRUE, right = right.closed)
    res <- Commission[[3]][band_idx] * Premium
  } else {
    stop("`Commission` must be a 3-column data.frame, each representing LowerLR, UpperLR and Commission.")
  }

  res
}

term_Reinstatement <- function(Loss, Premium, Scheme) {

}
#' @export
#' @rdname term_Excess
#' @param ProfitShare The figure of `ProfitShare`.  Or a 3-column `data.frame`, with the sequence of `LowerProfit`, `UppoerProfit` and `ProfitShare`
#' @param ExpenseAllowance i.e. Management Expense
#' @param digits To round loss ratio percentage.  e.g. 54.1234% will be rounded up to 54.12% if `digits = 2`
term_ProfitCommission <- function(Loss, Premium, ProfitShare, ExpenseAllowance, digits = 2) {
  # ProfitShare can be a number
  # or a 3-col DF
  if (is.numeric(ProfitShare) & length(ProfitShare) == 1L) {
    res <- Premium * pmax(0, Loss / Premium - ExpenseAllowance) * ProfitShare
  } else if (is.data.frame(ProfitShare) & ncol(ProfitShare) == 3 & all(lapply(ProfitShare, class) == "numeric")) {
    tmpDT <- data.table::as.data.table(ProfitShare) |> data.table::setnames(c("LowerProfit", "UpperProfit", "ProfitShare"))
    tmpProfitRate <- round(pmax(0, Loss / Premium - ExpenseAllowance), digits = digits + 2) # convert to digits in %
    PC_ratio <- vapply(
      tmpLR,
      function(.Profit) tmpDT[, sum(pmin(UpperProfit - LowerProfit, pmax(0, .Profit - LowerProfit)) * ProfitShare)],
      numeric(1L)
    )
    res <- LPC_ratio * Premium
  } else {
    stop("`ProfitShare` must be either a number a 3-column data.frame, each representing LowerProfit, UpperProfit and ProfitShare")
  }
  res
}
#' @export
#' @rdname term_Excess
#' @param Scheme A 3-column `data.frame`, with the sequence of `LowerLR`, `UpperLR` and `Participation`
#' @param is.CedentPerspective Logical.  If `TRUE`, the output is negative representing the loss amount that the cedent will bear
term_LossParticipation <- function(Loss, Premium, Scheme, is.CedentPerspective = TRUE, digits = 2) {
  # Scheme is 3-col DF, LowerLR, UpperLR, Participation
  # MR is RI's persp
  if (is.data.frame(Scheme) & ncol(Scheme) == 3 & all(lapply(Scheme, class) == "numeric")) {
    tmpDT <- data.table::as.data.table(Scheme) |> data.table::setnames(c("LowerLR", "UpperLR", "Participation"))
    tmpLR <- round(Loss / Premium, digits = digits + 2) # convert to digits in %
    LPC_ratio <- vapply(
      tmpLR,
      function(.LR) tmpDT[, sum(pmin(UpperLR - LowerLR, pmax(0, .LR - LowerLR)) * Participation)],
      numeric(1L)
    )
    res <- - LPC_ratio * Premium
  } else {
    stop("`Scheme` must be 3-column data.frame, each representing LowerLR, UpperLR and Participation")
  }

  if (!is.CedentPerspective) res <- Los + res

  res
}
