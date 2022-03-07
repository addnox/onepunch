test_that("num_near", {
  expect_true(num_near(sqrt(2)^2, 2))
})

test_that("num_is_integer", {
  x <- c(1.0000001, -1.9999999999, 5.99999999, -2, 2.123456, 2.0001, pi)
  expect_equal(num_is_integer(x), c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
})
