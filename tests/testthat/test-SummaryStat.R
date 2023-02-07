set.seed(123)
x <- rlnorm(1e5, 10, 5)
probs <- c(.1, .5, .8, .9, .95, .99, .995, .996, .999)
#data.table::data.table(Scenario = 1:1e5, EventID = 1:1e5, Loss = x) |> data.table::fwrite("c:/temp/a.eld", sep = "\t")

test_that("Mean and SD are correct", {
  expect_equal(opMean(x), 1684258500.48, tolerance = .00001) # from MR
  expect_equal(opSD(x), 180143001214.07, tolerance = .00001) # from MR
})

test_that("VaR at different probs", {
  actual_VaR <- c(35, 22128, 1483700, 13362000, 81726000, 2.593e+09, 8.139e+09, 1.1492e+10, 1.0361e+11)
  res_VaR <- vapply(probs, function(p) opVaR(x, p), double(1L))
  expect_equal(res_VaR, actual_VaR, tolerance = .01) ## there are still around .1 diff from MR results
})

test_that("TVaR at different probs", {
  actual_TVaR <- c(1871400000, 3368500000, 8420800000, 1.6837e+10, 3.3638e+10, 1.6622e+11, 3.2792e+11, 4.0746e+11, 1.5296e+12)
  res_TVaR <- vapply(probs, function(p) opTVaR(x, p), double(1L))
  expect_equal(res_TVaR, actual_TVaR, tolerance = .0001) ## TVaR diff is much smaller, i.e. 0.01%
})
