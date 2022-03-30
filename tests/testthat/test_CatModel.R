prm1 <- CatParameter(param_long)
prm2 <- CatParameter(param_wide)

test_that("CatParameter", {
  expect_equal(prm1, prm2)

  prm1_1 <- filter_occupancy(prm1, c("Commercial", "Industrial"))
  expect_true(all(vapply(prm1_1, function(x) x[, setequal(Occupancy, c("Commercial", "Industrial"))], logical(1L))))

})

