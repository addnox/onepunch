DT <- data.table(
  num1 = 1:10,
  logical1 = c(TRUE, rep(FALSE, 8), NA),
  char1 = LETTERS[1:10],
  num2 = 11:20 + .1,
  char2 = letters[11:20]
)

test_that("colum operations", {
  expect_equal(cnames(DT), names(DT))
  expect_equal(cnames(DT, 2:5), names(DT)[2:5])
  expect_equal(cnames(DT, !1:2), names(DT)[-(1:2)])
  expect_equal(cnames(DT, -(1:2)), names(DT)[-(1:2)])
  expect_equal(cnames(DT, is.numeric), c("num1", "num2"))
  expect_equal(cnames(DT, !is.numeric), c("logical1", "char1", "char2"))
  expect_equal(cnames(DT, logical1:char2), c("logical1", "char1", "num2", "char2"))
  expect_equal(cnames(DT, patterns("num")), c("num1", "num2"))
  expect_equal(cnames(DT, !patterns("1$")), c("num2", "char2"))
  expect_equal(cnames(DT, list(is.character, num2:char2, patterns("logical"))), c("logical1", "char1", "num2", "char2"))
  expect_equal(cnames(DT, .(is.character, num2:char2, patterns("logical"))), c("logical1", "char1", "num2", "char2"))
  expect_equal(ctypes(DT), setNames(c("numeric", "logical", "character", "numeric", "character"), names(DT)))
  expect_equal(ctypes(DT, as.DT = TRUE), data.table(name = names(DT), type = c("numeric", "logical", "character", "numeric", "character")))
})


