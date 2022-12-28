#' Create `S3` class for Property Exposure Rating (i.e. First Loss Scale method)
#' @param RiskProfile A `data.frame` containing at least the following 3 columns: `N`, `SI` and `Premium`
#' @param SubjectPremium Numeric, Subject premium to scale `RiskProfile`'s total premium
#' @param ExpectLossRatio Numeric, loss ratio to convert `SubjectPremium` to `ExpectLoss`
#' @param RiskCurve Created by `RiskCurve` function, or a risk curve function
#' @details
#'   The `PropExposureRating` class support the following methods:
#'   - `loss_in_layer`
#'   - `build_cdf` (to do)
#' @export
#'

PropExposureRating <- function(RiskProfile, SubjectPremium, ExpectLossRatio, RiskCurve, CurveType = NULL) {
  if (!is.data.frame(RiskProfile)) stop("`RiskProfile` must be a data.frame", call. = FALSE)
  RiskProfile <- data.table::as.data.table(RiskProfile)
  if (!all(c("N", "SI", "Premium") %in% names(RiskProfile))) stop("`RiskProfile` must have 3 columns: `N`, `SI` and `Premium`")
  RiskProfile[, AvgSI := SI / N]

  curve_type <- attr(RiskCurve, "type", exact = TRUE)

  if (is.null(curve_type) & is.null(CurveType)) {
    # not created by RiskCurve function, need to mannually input CurveType
    stop("`CurveType` must be input, with options being `Property` or `Casualty`", call. = FALSE)
  }

  res <- structure(
    list(
      RiskProfile = RiskProfile,
      SubjectPremium = SubjectPremium,
      ExpectLossRatio = ExpectLossRatio,
      RiskCurve = RiskCurve,
      Type = curve_type
    ),
    class = "PropExposureRating"
  )
}

#' @exportS3Method
loss_in_layer.PropExposureRating <- function(x, Limit, Deductible, ...) {
  FLS_Calc <- x$RiskCurve
  SubjectPrem <- x$SubjectPremium
  ELR <- x$ExpectLossRatio
  risk_profile <- data.table::copy(x$RiskProfile)

  risk_profile[, idx := .I]

  n_bands <- nrow(risk_profile)
  total_prem <- sum(risk_profile$Premium, na.rm = TRUE)

  layer_structure <- data.table::data.table(layerid = seq_along(Limit), Limit = Limit, Deductible = Deductible)

  data <- data.table::CJ(idx = risk_profile[["idx"]], layerid = layer_structure[["layerid"]]) |>
    merge(risk_profile, by = "idx", all.x = TRUE) |>
    merge(layer_structure, by = "layerid", all.x = TRUE)

  data2 <- data[, {
    stepSize = pmax(pmin(Deductible * 0.00000001, Limit), 0.01)
    AdjPrem = Premium * SubjectPrem / total_prem
    FLS_fguLimit = FLS_Calc(pmin(1, (Limit + Deductible) / AvgSI))
    FLS_Ded = FLS_Calc(pmin(1, Deductible / AvgSI))
    FLS_DedAddStep = FLS_Calc(pmin(1, (Deductible + stepSize) / AvgSI))
    Perc_LossExposed = FLS_fguLimit - FLS_Ded
    E_LossCount = (FLS_DedAddStep - FLS_Ded) * ELR * AdjPrem / stepSize
    E_LossSev = ELR * Perc_LossExposed * AdjPrem / E_LossCount

    list(layerid = layerid, E_LossCount = E_LossCount, E_LossSev = E_LossSev)
  }]

  res <- data2[, by = "layerid",
               {
                 ExpectedLayerCount = sum(E_LossCount, na.rm = TRUE)
                 ExpectedLayerLoss = sum(E_LossCount * E_LossSev, na.rm = TRUE)
                 ExpectedLayerSeverity = ExpectedLayerLoss / ExpectedLayerCount
                 list(
                   ExpectedLayerLoss = ExpectedLayerLoss,
                   ExpectedLayerSeverity = ExpectedLayerSeverity,
                   ExpectedLayerCount = ExpectedLayerCount
                 )
               }]

  return(res)
}

#' @export
exposure_in_layer <- function(RiskProfile, Limit, Deductible) {
  expo_in_singlelayer <- function(.limit, .ded){
    dt <- copy(RiskProfile)
    dt[, EIL := pmin(.limit, pmax(0, AvgSI - .ded)) * N]
    res <- dt[, .(Limit = .limit, Deductible = .ded, ExposureInLayer = sum(EIL, na.rm = TRUE))]

    return(res)
  }

  res <- Map(expo_in_singlelayer, .limit = Limit, .ded = Deductible) |>
    rbindlist()

  return(res)
}

#' @export
RiskCurve <- function(...) {
  input_names <- ...names()
  input_length <- ...length()
  prm <- list(...)

  if (setequal(input_names, c("b", "g"))) {
    # MB Curve
    f <- function(x) ecMBBEFD(x, b = prm[["b"]], g = prm[["g"]])
    curve_type <- "Property"
  } else if (setequal(input_names, c("base", "z"))) {
    # Riebesell
    f <- function(x) ecRiebesell(x, base = prm[["base"]], z = prm[["z"]])
    curve_type <- "Casualty"
  } else if (input_length == 1 & is.data.frame(..1)) {
    # table format Risk Curve
    RiskCurveDT <- as.data.table(..1)
    if (setequal(names(RiskCurveDT), c("Percent", "FLS"))) {

      curve_type <- "Property"
    } else if (setequal(names(RiskCurveDT), c("Limit", "ILF"))) {

      curve_type <- "Casualty"
    } else {
      ## error message
    }
  } else if (input_length == 1 & as.character(..1)) {
    #
  }

  attr(f, "type") <- curve_type
  f
}

#' @export
ecMBBEFD = function(x, b, g) {
  if (!(g >= 1 && b >= 0))
    return(rep(NaN, length(x)))
  if (g == 1 || b == 0) {
    res <- x
  }
  else if (g == 1/b && b < 1) {
    res <- (1 - b^x)/(1 - b)
  }
  else if (g > 1 && b == 1) {
    res <- log(1 + (g - 1) * x)/log(g)
  }
  else {
    res <- log((g - 1) * b/(1 - b) + (1 - g * b) * b^x/(1 - b))/log(g * b)
  }
  res[x < 0] <- 0
  res[x > 1] <- 1
  res
}

#' @export
ecRiebesell = function(x, base, z) {
  res <- (x / base) ^ log2(1 + z)
  res
}
#
# list_MB <- list(
#
# )
#
# list_Cas <- list(
#
# )
