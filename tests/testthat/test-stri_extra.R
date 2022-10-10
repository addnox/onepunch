
test_that("NA values in input pass through unchanged", {
  expect_equal(
    stri_trunc(NA_character_, width = 5),
    NA_character_
  )
  expect_equal(
    stri_trunc(c("foobar", NA), 5),
    c("fo...", NA)
  )
})

test_that("truncations work for all elements of a vector", {
  expect_equal(
    stri_trunc(c("abcd", "abcde", "abcdef"), width = 5),
    c("abcd", "abcde", "ab...")
  )
})

test_that("truncations work for all sides", {

  trunc <- function(direction) stri_trunc(
    "This string is moderately long",
    direction,
    width = 20
  )

  expect_equal(trunc("right"),   "This string is mo...")
  expect_equal(trunc("left"),    "...s moderately long")
  expect_equal(trunc("center"),  "This stri...ely long")
})

test_that("does not truncate to a length shorter than elipsis", {
  expect_snapshot(error = TRUE, {
    stri_trunc("foobar", 2)
    stri_trunc("foobar", 3, ellipsis = "....")
  })
})

test_that("str_squish removes excess spaces from all parts of string", {
  expect_equal(stri_squish("ab\t\tc\t"),   "ab c")
  expect_equal(stri_squish("\ta  bc"),   "a bc")
  expect_equal(stri_squish("\ta\t bc\t"), "a bc")
})
