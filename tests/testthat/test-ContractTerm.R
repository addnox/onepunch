DT_SS <- data.table::data.table(
  LowerLR =    c(0,seq(0.15, .6, by = .01)),
  UpperLR =   c(seq(0.15, .6, by = .01), Inf),
  Commission = c(0.6858,0.6796,0.6734,0.6672,0.661,0.6548,0.6486,0.6423,0.6361,0.6298,0.6236,0.6173,0.6111,0.6048,0.5986,0.5923,0.5861,
                 0.5798,0.5698,0.5598,0.5498,0.5398,0.5298,0.5198,0.5098,0.4998,0.4898,0.4798,0.4698,0.4616,0.4533,0.4451,0.4369,0.4286,0.4204,0.4122,0.404,
                 0.3957,0.3875,0.3793,0.371,0.3628,0.3546,0.3464,0.3382,0.33,0.3218)
)

DT_LPC <- data.table::data.table(
  LowerLR = c(0, .85, 1.25),
  UpperLR = c(.85, 1.25, Inf),
  Participation = c(0, .2, 0)
)

Premium <- c(24001911.47, 56432308.04, 118046952.4, 192014058.3, 214192737.1, 188705953.9, 197571383.2, 228121724.2, 220886728.8, 273882969.9, 403702524.3, 354203497.5, 419957070.7)
Loss <- c(6620786.45, 27831193.48, 61713563.81, 44243780.32, 56565703.64, 31758742.14, 90807115.15, 120037889.9, 69398251.9, 285384294.7, 100209246.3, 89574575.17, 125820105.2)

test_that("Sliding Scale Commission", {
  res_SS <- c(14516356.06, 23261397.37, 45743194.06, 120930453.9, 130893181.6, 127074589.3, 87939022.68, 88397168.14, 128070125.3, 88135539.73, 251748894.2, 218649819, 248740573)
  expect_equal(term_Commission(Loss, Premium, DT_SS), res_SS)
})

test_that("Loss Participation" ,{
  res_LPC <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, -10517106.05, 0, 0, 0)
  expect_equal(term_LossParticipation(Loss, Premium, DT_LPC), res_LPC)
})

test_that("Excess of Loss", {
  Loss2 <- c(0, 50, 100, 200, 201, 299, 300, 301, 500)
  expect_equal(term_Excess(Loss2, Limit = 100, Deductible = 200), c(0, 0, 0, 0, 1, 99, 100, 100, 100))
  expect_equal(term_Excess(Loss2, Limit = 100, Deductible = 200, is.FranchiseDed = TRUE), c(0, 0, 0, 0, 100, 100, 100, 100, 100))
  expect_equal(term_Excess(Loss2, Limit = 100, Deductible = 200, is.FranchiseLimit = TRUE), c(0, 0, 0, 0, 1, 99, 100, 0, 0))
  expect_equal(term_Excess(Loss2, Limit = 100, Deductible = 200, is.FranchiseDed = TRUE, is.FranchiseLimit = TRUE), c(0, 0, 0, 0, 0, 0, 0, 0, 0)) ## always 0
})
