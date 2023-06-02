test_that("date_duration", {
  d <- as.Date(c("2010-03-31", "2010-06-30", "2011-09-30", "2013-12-31", "2014-03-31", NA))
  expect_equal(date_duration(as.Date("2010-01-01"), d, unit = "month", round_digits = 0), c(3, 6, 21, 48, 51, NA))
  expect_equal(date_duration(d, as.Date("2014-12-31"), unit = "month", round_digits = 0), c(57, 54, 39, 12, 9, NA))
  expect_equal(date_duration(as.Date("2010-01-01"), d, unit = "quarter"), c(3, 6, 21, 48, 51, NA) / 3)
})

test_that("date_parse_month", {
  d <- c("Dec. 2021", "2021/03", "22 March", "0719", "aaa", NA)
  expect_equal(date_parse_month(d), as.Date(c("2021-12-01", "2021-03-01", "2022-03-01", "2019-07-01", NA, NA)))
  expect_equal(date_parse_month(d, period_end = TRUE), as.Date(c("2021-12-31", "2021-03-31", "2022-03-31", "2019-07-31", NA, NA)))
})


test_that("date_parse_quarter", {
  d <- c("2021q3", "2021Q4", "Q1-2021", "0719", "aaa", NA)
  expect_equal(date_parse_quarter(d), as.Date(c("2021-09-30", "2021-12-31", "2021-03-31", NA, NA, NA)))
  expect_equal(date_parse_quarter(d, period_end = FALSE), as.Date(c("2021-07-01", "2021-10-01", "2021-01-01", NA, NA, NA)))
})

test_that("date_parse_excel", {
  expect_equal(date_parse_excel(c(NA, 43831, 43555)), lubridate::ymd(NA, 20200101, 20190331))
})
