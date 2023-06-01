
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

test_that("stri_squish removes excess spaces from all parts of string", {
  expect_equal(stri_squish("ab\t\tc\t"),   "ab c")
  expect_equal(stri_squish("\ta  bc"),   "a bc")
  expect_equal(stri_squish("\ta\t bc\t"), "a bc")
})

test_that("stri_extract_Chinese works properly", {
  x <- c("Sunflower Insurance (葵花保险) （新型）", "Sugarbeet Insurance（甜菜保险）", "Corn Full Cost Insurance (Irrigated Land)（水地玉米完全成本保险）", "Corn Full Cost Insurance (Dry Land)（旱地玉米完全成本保险）")
  # simplify
  res1 <- c("葵花保险-新型", "甜菜保险", "水地玉米完全成本保险", "旱地玉米完全成本保险")
  expect_equal(stri_extract_Chinese(x, sep = "-"), res1)

})

test_that("stri_amap works propertly", {
  x1 <- c("水稻种植保险\\nRice Insurance", "水地玉米种植保险\\nIrrigated Land Corn Insurance", "旱地玉米种植保险\\nDryland Corn Insurance", "水地小麦种植保险\\nIrrigated Land Wheat Insurance ", "旱地小麦种植保险\\nDryland Wheat Insurance", "水地马铃薯保险\\nIrrigated Potato Insurance ", "旱地马铃薯保险\\nDryland Potato Insurance", "油菜种植保险\\nRape Insurance")
  x2 <- c("Irrigated Land Corn Insurance (水地玉米种植保险)", "Rice Insurance (水稻种植保险)", "Irrigated Land Wheat Insurance（水地小麦种植保险）", "Rape Insurance （油菜种植保险)", "Dryland Potato Insurance (旱地马铃薯保险)", "Dryland Wheat Insurance（旱地小麦种植保险）", "Irrigated Potato Insurance (水地马铃薯保险)", "Dryland Corn Insurance (旱地玉米种植保险)")
  res1 <- c("Rice Insurance (水稻种植保险)", "Irrigated Land Corn Insurance (水地玉米种植保险)", "Dryland Corn Insurance (旱地玉米种植保险)", "Irrigated Land Wheat Insurance（水地小麦种植保险）", "Dryland Wheat Insurance（旱地小麦种植保险）", "Irrigated Potato Insurance (水地马铃薯保险)", "Dryland Potato Insurance (旱地马铃薯保险)", "Rape Insurance （油菜种植保险)")
  expect_equal(stri_amap(x1, x2), res1)
})
