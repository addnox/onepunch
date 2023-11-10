
test_that("Fixed Commission", {
  ## fixed commision with loss info
  expect_equal(ct_Commission(c(NA, 200, 300, 400), Premium = 1000, Term = .345), c(NA, rep(.345 * 1000, 3)))
  ## without loss
  expect_equal(ct_Commission(Premium = rep(1000, 3), Term = .543), rep(.543 * 1000, 3))
})

test_that("Sliding Commission", {
  SS <- data.table::data.table(LowerLR = c(0, .15, .2, .25), UpperLR = c(.15, .2, .25, Inf), Commission = c(.3, .25, .2, .15))
  LR <- c(0, .16, .2, .4, .9, NA)
  ans1 <- c(.3, .25, .25, .15, .15, NA)
  ans2 <- c(.3, .25, .2, .15, .15, NA)
  expect_equal(ct_Commission(LR * 1000, 1000, SS), ans1 * 1000)
  expect_equal(ct_Commission(LR[3] * 1000, 1000, SS), ans1[3] * 1000) # check single value
  expect_equal(ct_Commission(LR * 1000, 1000, SS, right.closed = FALSE), ans2 * 1000)
})

test_that("Loss Participation", {
  expect_equal(
    ct_LossParticipation(c(.79, .85, 1.2, NA, 1.3), Premium = 1, Term = data.table::data.table(.8, 1.2, .2)),
    c(0, -.05 * .2, -.4 * .2, NA, -.4 * .2)
  )

  expect_equal(
    ct_LossParticipation(c(.79, .85, 1.2, NA, 1.3), Premium = 1, Term = data.table::data.table(.8, Inf, .25)),
    c(0, -.05 * .25, -.4 * .25, NA, -.5 * .25)
  )

  expect_equal(
    ct_LossParticipation(c(.79, .85, 1.1, NA, 1.3), Premium = 1, Term = data.table::data.table(c(.8, 1), c(1, 1.2), c(.2, .25))),
    c(0, -.05 * .2, -.2 * .2 - .1 * .25, NA, -.2 * .2 - .2 * .25)
  )
})

test_that("Profit Commission", {
  expect_equal(
    ct_ProfitCommission(c(.2, .6, .9, 1.2, NA), Premium = 1, Term = .3, ExpenseAllowance = .2),
    c(.6 * .3, .2 * .3, 0, 0, NA)
  )

  expect_equal(
    ct_ProfitCommission(c(.2, .6, .9, 1.2, NA), Premium = 1, Term = data.table::data.table(0, .5, .3), ExpenseAllowance = .2),
    c(.5 * .3, .2 * .3, 0, 0, NA)
  )
})


test_that("Excess", {
  expect_equal(ct_Excess(c(100, 201, 250, 400, NA), Limit = 100, Deductible = 200), c(0, 1, 50, 100, NA))
  expect_equal(ct_Excess(c(100, 201, 250, 400, NA), Limit = 100, Deductible = 200, is.FranchiseLimit = TRUE), c(0, 1, 50, 0, NA))
  expect_equal(ct_Excess(c(100, 201, 250, 400, NA), Limit = 100, Deductible = 200, is.FranchiseDed = TRUE), c(0, 100, 100, 100, NA))
})


