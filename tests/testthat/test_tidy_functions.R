library(data.table)

test_that("tidy_trim", {
  x <- data.table(V1 = c(NA, 1, NA, NA), V2 = c(NaN, NA, NA, NA), V3 = c(NA, "A", "B", NA))
  x_trim <- tidy_trim(x)
  data.table::setattr(x_trim, "row.names", seq_len(nrow(x_trim)))
  expect_equal(x_trim, data.table(V1 = c(1, NA), V3 = c("A", "B")))
})

DT <- data.table(X1 = NA, X2 = c(NA, 1:2, NA, NA, 3), X3 = c(rep(NA, 5), .1))
DT1 <- cbind(data.table(ID = c(NA, "A", "A", NA, NA, "B")), DT)
DT2 <- cbind(data.table(ID = c("A", rep(NA, 3), "B", NA)), DT)
DT3 <- cbind(data.table(ID = c(rep(NA, 2), "A-Total", rep(NA, 2), "B-Total")), DT)

tDT <- transpose(DT)
tDT1 <- transpose(DT1)
tDT2 <- transpose(DT2)
tDT3 <- transpose(DT3)

test_that("tidy_hsplit", {
  expect_equal(tidy_hsplit(DT), list(DT[2:3], DT[6]))
  expect_equal(tidy_hsplit(DT1, by = 1L), list(A = DT1[2:3], B = DT1[6]))
  expect_equal(tidy_hsplit(DT1, by = c("A", "A", "A", "B", "B", "B"), factor.by = TRUE), list(A = DT1[2:3], B = DT1[6]))
})

test_that("tidy_vsplit", {
  expect_equal(tidy_vsplit(tDT), list(tDT[, 2:3], tDT[, 6]))
  expect_equal(tidy_vsplit(tDT1, by = 1L), list(A = tDT1[, 2:3], B = tDT1[, 6]))
  expect_equal(tidy_vsplit(tDT1, c("A", "A", "A", "B", "B", "B"), factor.by = TRUE), list(A = tDT1[, 2:3], B = tDT1[, 6]))
})

DT4 <- data.table::data.table(
          V1 = c("CompA Data", "RiskProfile", NA, "Lbound", "0", "100"),
          V2 = c(NA, NA, NA, NA, NA, NA),
          V3 = c(NA, NA, NA, "Ubound", "100", "500"),
          V4 = c(NA, "Property", "Gross", "No. of Risks", "10", "2"),
          V5 = c(NA, NA, NA, "Sum Insured", "1000", "2000"),
          V6 = c(NA, NA, NA, "Premium", "1", "2"),
          V7 = c(NA, NA, "Net", "N", "10", "2"),
          V8 = c(NA, NA, NA, "Sum Insured", "500", "200"),
          V9 = c(NA, NA, NA, "Premium", "0.5", "0.2"),
         V10 = c(NA, "CAR/EAR", "Gross", "No. of Risks", "5", "0"),
         V11 = c(NA, NA, NA, "Sum Insured", "500", "0"),
         V12 = c(NA, NA, NA, "Premium", "10", "0"),
         V13 = c(NA, NA, "Net", "N", "5", "0"),
         V14 = c(NA, NA, NA, "Sum Insured", "100", "0"),
         V15 = c(NA, NA, NA, "Premium", "2", "0"),
         V16 = c(NA, NA, NA, NA, NA, NA),
         V17 = c(NA, NA, NA, "Company", "A", "A")
  )

test_that("tidy_table & tidy_lift_header", {
  DT4_tidy1 <- tidy_table(DT4, header_rows = 2:4, wide_cols = 4:16, wide_names = c("LOB", "Account", "Var"), cols_delete = NA, wide_as_numeric = TRUE)
  expect_equal(DT4_tidy1[LOB == "CAR/EAR" & Account == "Net" & Var == "N", sum(value)], 5)
  expect_equal(DT4_tidy1[LOB == "Property" & Account == "Gross" & Var == "Sum Insured", mean(value)], 1500)

  DT4_tidy2 <- suppressWarnings(tidy_table(DT4, header_rows = 2:4, wide_cols = 4:16)) # NA columns is of type logical, so there will be warnings
  expect_equal(names(DT4_tidy2), c("RiskProfile_Lbound", "V2", "Ubound", "Company", paste0(".wide", 1:3), "value"))
  expect_equal(DT4_tidy2[!is.na(.wide1), unique(.wide1)], c("Property", "CAR/EAR"))

  DT4_tidy3 <- tidy_lift_header(DT4[, 1:9], header_rows = 2:4, long_cols = 1:3, cols_delete = NA)
  DT4_tidy3_expected <- data.table::data.table(
          RiskProfile_Lbound = c("0", "100"),
                      Ubound = c("100", "500"),
  Property_Gross_No_of_Risks = c("10", "2"),
  Property_Gross_Sum_Insured = c("1000", "2000"),
      Property_Gross_Premium = c("1", "2"),
              Property_Net_N = c("10", "2"),
    Property_Net_Sum_Insured = c("500", "200"),
        Property_Net_Premium = c("0.5", "0.2")
  )

  expect_equal(DT4_tidy3, DT4_tidy3_expected)
})

test_that("tidy_table with wide_split", {
  y <- data.table::data.table(
      V1 = c("Gross_SI", 10),
      V2 = c("Gross_Prem", 1),
      V3 = c("Net_SI", 8)
  )

  res1 <- tidy_table(y, 1, wide_cols = 1:3, wide_names = c("Account", "Var"), wide_split = "_", wide_as_numeric = TRUE)
  res_expected <- data.table::data.table(Account = c("Gross", "Gross", "Net"), Var = c("SI", "Prem", "SI"), value = c(10, 1, 8))
  expect_equal(res1, res_expected)
  res2 <- tidy_table(y, 1, wide_cols = 1:3, wide_split = "_", wide_as_numeric = TRUE)
  expect_equal(res2, setNames(res_expected, c(".wide1", ".wide2", "value")))
})
