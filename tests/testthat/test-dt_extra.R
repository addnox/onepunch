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

  expect_equal(date_time_col, date_time_col_expectation)
})

test_that("dt_patch produces correct results", {
  dt1 <- data.table::data.table(ID1 = c(1, 1, 2, 2, 3), ID2 = c("A", "B", "A", "C", "B"), v1 = c(NA, 2:5), v2 = c(NA, LETTERS[4:1]), v3 = lubridate::ymd("2021-01-01", "2022-01-01", "2023-01-01",  "2022-01-01", "2023-01-01"))
  dt2 <- data.table::data.table(ID1 = c(1, 2, 3), ID2 = "A", v1 = 11L, v2 = "K", v3 = lubridate::ymd(20111231))

  res_expected_1 <- data.table::data.table(ID1 = c(1, 1, 2, 2, 3), ID2 = c("A", "B", "A", "C", "B"), v1 = c(11, 2, 11, 4, 5), v2 = c("K", "D", "K", "B", "A"),  v3 = lubridate::ymd(20111231, 20220101, 20111231, 20220101, 20230101))
  res_expected_2 <- data.table::data.table(ID1 = c(1, 1, 2, 2, 3), ID2 = c("A", "B", "A", "C", "B"), v1 = c(11, 2, 3, 4, 5), v2 = c("K", "D", "C", "B", "A"),  v3 = lubridate::ymd(20210101, 20220101, 20230101, 20220101, 20230101))

  res_actual_1 <- dt_patch(dt1, dt2, by = c("ID1", "ID2"), vars = c("v1", "v2", "v3"), ties = "y")
  res_actual_2 <- dt_patch(dt1, dt2, by = c("ID1", "ID2"), vars = c("v1", "v2", "v3"), ties = "x")

  expect_equal(res_actual_1, res_expected_1)
  expect_equal(res_actual_2, res_expected_2)
})
