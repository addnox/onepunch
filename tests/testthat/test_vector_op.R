test_that("vec_nafill", {
  x <- c("A", NA, "B", NA, NA, "C", NA)
  expect_equal(vec_nafill(x, fill = "X"), c("A", "X", "B", "X", "X", "C", "X"))
  expect_equal(vec_nafill(x, direction = "down"), c("A", "A", "B", "B", "B", "C", "C"))
  expect_equal(vec_nafill(x, direction = "forwards"), c("A", "A", "B", "B", "B", "C", "C"))
  expect_equal(vec_nafill(x, direction = "up"), c("A", "B", "B", "C", "C", "C", NA))
  expect_equal(vec_nafill(x, direction = "backwards"), c("A", "B", "B", "C", "C", "C", NA))
  expect_equal(vec_nafill(x, direction = "down", breaks = c(F, F, F, F, T, F, F)), c("A", "A", "B", "B", NA, "C", "C"))
})

test_that("vec_lookup", {
  x <- c("A", "B", "C", "CBA", "abc")
  key <- c("a", "b", "A")
  value <- c("Alice", "Bob", "Apple")

  expect_equal(vec_lookup(x, key, value), c("Alice", "Bob", rep(NA, 3)))
  expect_equal(vec_lookup(x, key, value, ignore_case = FALSE), c("Apple", rep(NA, 4)))
  expect_equal(vec_lookup(x, key, value, default = "unknown"), c("Alice", "Bob", rep("unknown", 3)))
  expect_equal(vec_lookup(x, key, value, default = x), c("Alice", "Bob", x[3:5]))
  # expect_equal(vec_lookup(x, key, value, method = "regex"), c("Alice", "Bob", NA, "Alice", "Alice"))
})

test_that("vec_detect", {
  x <- c(NA, NA, "Layer", "no", 1, 2, 3, "layer info:")
  expect_equal(x[vec_detect(x, c("layer|no"))], c("Layer", "no", "layer info:"))
  expect_equal(x[vec_detect(x, c("layer|no"), ignore_case = FALSE)], c("no", "layer info:"))
  expect_equal(x[vec_detect(x, c("layer|no"), na.rm = FALSE)], c(NA, NA, "Layer", "no", "layer info:"))
  expect_equal(x[vec_detect(x, c("layer", "info"))], c("layer info:"))
  expect_equal(x[vec_detect(x, c("layer|no"), exclusions = "info")], c("Layer", "no"))
  expect_equal(x[vec_detect(x, c("layer$|no"), negate = TRUE)], c(1:3, "layer info:"))

  y <- c(NA, NA, "2020", "(Jan-1)", "Company A", NA, NA, "2018/19", "0701", "Comp A")
  expect_equal(y[vec_detect(y, "20\\d{2}", offset = 1)], c("(Jan-1)", "0701"))
  expect_equal(y[vec_detect(y, "^comp", offset = -1)], c("(Jan-1)", "0701"))
})

test_that("vec_pick", {
  x <- c(NA, NA, "Layer", "no", 1, 2, 3, "layer info:")
  y <- 1:8

  expect_equal(vec_pick(x, c("layer|no")), c(NA, NA, "Layer", "no", NA, NA, NA, "layer info:"))
  expect_equal(vec_pick(x, c("layer|no"), y = y), c(NA, NA, 3:4, NA, NA, NA, 8))
  expect_equal(vec_pick(x, c("layer|no"), y = y, offset = -1), c(NA, 2:3, NA, NA, NA, 7, NA))
})

test_that("vec_extract", {
  x <- c("", "Property_NoOfRisk", "SI", "Prem", "MB_NoOfRisk", "SI", "Prem", "% of Prem")
  expect_equal(vec_extract(x, c("property|mb")), c(NA, "Property", NA, NA, "MB", rep(NA, 3)))
  expect_equal(vec_extract(x, "(.*)_NoOfRisk"), c(NA, "Property", NA, NA, "MB", rep(NA, 3)))
  expect_equal(vec_extract(x, c("property|mb"), fill_direction = "down"), c(NA, rep("Property", 3), rep("MB", 4)))
  expect_equal(vec_extract(x, c("property", "(.*)_NoOfRisk")), c(NA, "Property", rep(NA, 6)))
  expect_equal(vec_extract(x, c("SI|Prem"), exclusions = "% of Prem"), c(NA, NA, rep(c("SI", "Prem", NA), 2)))
})
