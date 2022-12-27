RP <- data.table(Lbound = c(0, 100, 200), Ubound = c(100, 200, Inf), N = 10, SI = c(50, 150, 250) * 10, Premium = c(50, 150, 250) * 10 * .001)
MBBEFD <- RiskCurve(b = 0.0218339035107131, g = 1096.98894971097)

test_that("Exposure Rating: Mean Loss in Layer",{
  Layers <- data.table(Limit = c(50, 200), Deductible = c(50, 100), LayerName = c("L1", "L2"))
  Expo <- PropExposureRating(RP, .45, 9, MBBEFD)
  res <- loss_in_layer(Expo, Layers$Limit, Layers$Deductible)

  # correct_output from GC Prop Expo Rating Tool
  correct_output <- data.table(
    layerid = c(1L, 2L),
    ExpectedLayerLoss = c(0.353446803879989,0.174087164715577),
    ExpectedLayerSeverity = c(27.8198525949019, 46.8104976499506),
    ExpectedLayerCount = c(0.0127048410006586,0.00371897701275048),
    key = "layerid"
  )

  # compare
  expect_equal(res, correct_output, tolerance = .0001)
})

# test_that("Exposure build CDF", {
#   ECDF0 <- PropExpoRating$new(RP, MBBEFD, .45, 9)$build_CDF(Truncation = 50, UpperLimit = 250, N_Increment = 200)$CDF
#
#   res_sev <- ECDF0$Severity[c(4, 96, 200),]  # row 4, 96 and 200
#
#   correct_sev <- data.table(
#     Loss = c(51.2217667795136, 107.394182488543, 247.996275597472),
#     CDF = c(0.0356009793813233, 0.749298085304252, 0.981176369979008)
#   )
#
#   res_freq <- ECDF0$Frequency
#   correct_freq <- 0.0127048410006686
#
#   expect_equal(res_sev, correct_sev, tolerance = .0001)
#   expect_equal(res_freq, correct_freq, tolerance = .0001)
# })
