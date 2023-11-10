#' Common contract terms
#'
#' @description
#' `ct_Excess` calculates LossInLayer based on specified Limit and Deductible structure
#' `ct_Commission` calculates commission figures based on given sliding scale table
#' `ct_Reinstatement` returns a list of two vectors, one being reinstated loss and the other is reinstated premium
#' `ct_ProfitCommission` calculates profit commission
#' `ct_LossParcipation` calculates loss participation (i.e. loss corridor) from cedent's perspective
#' @export
#' @param Loss,Premium Numeric vector for loss and premium
#' @param Limit,Deductible Limit and Deductible terms
#' @param is.FranchiseLimit Logical, franchise limit means there is no reimbursement when LossAboveDeducible is greater than Limit
#' @param is.FranchiseDed Logical, franchise deductible means when Loss is greater than Deductible, full f.g.u. loss is applicable for reimbursement
#' @section `Term` parameter for different functions:
#' * Commission:  parameter `Term` can either be a value (for fixed commission), or a 3-column `data.frame`, with the sequence of `LowerLR`, `UpperLR` and `Commission`
#' * LossParticipation: parameter `Term` is 3-column `data.frame`, with the sequence of `LowerLR`, `UpperLR` and `Participation`
#' * ProfitCommission: parameter `Term` either a number a 3-column data.frame, each representing `LowerProfit`, `UpperProfit` and `ProfitShare`.
ct_Excess <- function(Loss, Premium = NULL, Limit, Deductible, is.FranchiseLimit = FALSE, is.FranchiseDed = FALSE) {
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

  x2
}


#' @export
#' @rdname ct_Excess
#' @param is.CedentPerspective Logical.  If `TRUE`, the output is negative representing the loss amount that the cedent will bear
ct_LossParticipation <- function(Loss, Premium, Term, is.CedentPerspective = TRUE) {
  # Term is 3-col DF, LowerLR, UpperLR, Participation
  # MR is RI's persp
  if (is.data.frame(Term) & ncol(Term) == 3 & all(lapply(Term, class) == "numeric")) {
    tmpDT <- data.table::as.data.table(Term) |> data.table::setnames(c("LowerLR", "UpperLR", "Participation"))
    tmpLR <- Loss / Premium
    LPC_ratio <- vapply(
      tmpLR,
      function(.LR) tmpDT[, sum(pmin(UpperLR - LowerLR, pmax(0, .LR - LowerLR)) * Participation)],
      numeric(1L)
    )
    res <- - LPC_ratio * Premium
  } else {
    stop("`Term` must be 3-column data.frame, each representing LowerLR, UpperLR and Participation", call. = FALSE)
  }

  res
}

#' @export
#' @rdname ct_Excess
#' @examples
#' ct_ProfitCommission(c(.2, .6, .8), ProfitShare = .2, ExpenseAllowance = .2)
#'
ct_ProfitCommission <- function(LossAndExpense, Premium, Term, ExpenseAllowance) {
  if (is.numeric(Term) && length(Term) == 1L) {
    res <- Premium * pmax(0, 1 - LossAndExpense / Premium - ExpenseAllowance) * Term
  } else if (is.data.frame(Term) & ncol(Term) == 3 & all(lapply(Term, class) == "numeric")) {
    tmpDT <- data.table::as.data.table(Term) |> data.table::setnames(c("LowerProfit", "UpperProfit", "ProfitShare"))
    tmpProfitRate <- pmax(0, 1 - LossAndExpense / Premium - ExpenseAllowance)
    PC_ratio <- vapply(
      tmpProfitRate,
      function(.Profit) tmpDT[, sum(pmin(UpperProfit - LowerProfit, pmax(0, .Profit - LowerProfit)) * ProfitShare)],
      numeric(1L)
    )
    res <- PC_ratio * Premium
  } else {
    stop("`Term` must be either a number a 3-column data.frame, each representing LowerProfit, UpperProfit and ProfitShare")
  }
  res
}

#' @export
#' @rdname ct_Excess
#' @param Term See Details
#' @param right.closed Logical, default to be TRUE.  RightBound inclusive
#' @examples
#' SS <- data.table::data.table(LowerLR = c(0, .15, .2, .25), UpperLR = c(.15, .2, .25, Inf), Commission = c(.3, .25, .2, .15))
#' ct_Commission(Loss = c(.19, .2, .21), Premium = 1, SS)
#' ct_Commission(Loss = c(.19, .2, .21), Premium = 1, Term = .33)
ct_Commission <- function(Loss = NULL, Premium, Term, right.closed = TRUE) {
  # Term can be a number, then fixed Comm
  # Term can be a 3-column DF, with name LowerLR, UpperLR and Commission

  if (length(Loss) > 1 && length(Premium) == 1) {
    Premium <- rep(Premium, times = length(Loss))
    Premium[is.na(Loss)] <- NA_real_
  }

  if (is.data.frame(Term) && ncol(Term) == 3 && all(lapply(Term, class) == "numeric")) {
    if (!(all(abs(Term[[1]][-1L] - data.table::shift(Term[[2]])[-1L]) <= .0001))) {
      stop("LowerLR must be the same as the preceding UpperLR", call. = FALSE)
    }

    .SS <- data.table::as.data.table(Term) |> data.table::setnames(c("LowerLR", "UpperLR", "Commission"))
    if (!is.numeric(Loss)) stop("`Loss` must be a numeric vector", call. = FALSE)
    if (!is.numeric(Premium)) stop("`Premium` must be a numeric vector", call. = FALSE)
    .LR <- Loss / Premium
    band_idx <- cut(.LR, breaks = c(0, Term[[2]]), labels = FALSE, include.lowest = TRUE, right = right.closed)
    res <- Term[[3]][band_idx] * Premium
    res
  } else if (is.numeric(Term) && length(Term) == 1) {
    res <- Term * Premium
  } else {
    stop("`Commission` must be a single value, or a 3-column data.frame, each representing LowerLR, UpperLR and Commission.", call. = FALSE)
  }

  res
}
