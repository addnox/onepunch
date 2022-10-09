# data.trable
test_that("data.trable() constructs 'data.table' as expected", {
  result <- data.trable(
    ~colA, ~colB,
    "a", 1,
    "b", 2
  )

  compared <- data.table(colA = c("a", "b"), colB = c(1, 2))
  expect_equal(result, compared)

  ## wide
  wide <- data.trable(
    ~colA, ~colB, ~colC, ~colD,
    1, 2, 3, 4,
    5, 6, 7, 8
  )

  wide_expectation <- data.table(
    colA = c(1, 5),
    colB = c(2, 6),
    colC = c(3, 7),
    colD = c(4, 8)
  )

  expect_equal(wide, wide_expectation)

  ## long
  long <- data.trable(
    ~colA, ~colB,
    1, 6,
    2, 7,
    3, 8,
    4, 9,
    5, 10
  )

  long_expectation <- data.table(
    colA = as.numeric(1:5),
    colB = as.numeric(6:10)
  )

  expect_equal(long, long_expectation)
})

test_that("data.trable() handles columns with a class", {
  date_time_col <- data.trable(
    ~dt, ~dttm,
    as.Date("2023-11-12"), as.POSIXct("2024-12-05 02:13:59"),
    as.Date("2003-01-02"), as.POSIXct("2004-04-05 13:45:17")
  )

  date_time_col_expectation <- data.table(
    dt = c(as.Date("2023-11-12"), as.Date("2003-01-02")),
    dttm = c(as.POSIXct("2024-12-05 02:13:59"), as.POSIXct("2004-04-05 13:45:17"))
  )
  attr(date_time_col_expectation$dttm, "tzone") <- NULL

  expect_equal(date_time_col, date_time_col_expectation)
})
