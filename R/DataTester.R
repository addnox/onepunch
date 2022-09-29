#' A container for all data tests
#' @export
#' @examples
#' dt1 <- DataTester$new()
#' DT <- data.table::as.data.table(mtcars)
#' dt1$test_that("cyl is below 20", {DT[cyl > 20]})
#' dt1$test_that("cyl cannot be 6", {DT[cyl == 6]}, "cyl2")
#' dt1$test_that("There are 32 rows", {nrow(DT) == 32})
#' ## or wrap them all into a function
#' test_f <- function() {
#'   dt1 <- DataTester$new()
#'   dt1$test_that("cyl is below 20", {DT[cyl > 20]})
#'   dt1$test_that("cyl cannot be 6", {DT[cyl == 6]}, "cyl2")
#' }
#' test_f()

DataTester <- R6::R6Class(
  "DataTester",
  public = list(
    #' @description
    #' Create a new DataTester object
    #' @param cols_keep A character vector for columns to keep
    initialize = function(cols_keep = NULL) {
      self$keep_cols(cols_keep)
    },
    #' @description
    #' Create a new test
    #' @param desc Human-readble description for the test
    #' @param code Code for testing, can be embraced with curly bracket (i.e. {...})
    #' @param test_name Concise test id, usually for outputing as Excel sheetname.  If left blank, an id like "test-001" will be created automatically
    #' @param cols_keep Columns in output dataset (for invalid records).  This will over-rule the cols_keep input during object initialization
    test_that = function(desc, code, test_name = NULL, cols_keep = NULL) {
      code <- substitute(code) ## borrow from test_that
      test_res <- eval(code, parent.frame())

      if (is.logical(test_res) & length(test_res) == 1) {
        if (test_res) {
          cli::cli_alert_success(desc)
        } else {
          cli::cli_alert_danger(desc)
        }
      } else {
        # data.frame case
        if (is.null(test_name)) test_name <- paste0("test-", stringi::stri_pad_left(length(private$.results) + 1, width = 3, pad = "0"))
        msg_test_result <- "all valid"
        msg_color <- "blue"
        n_invalid <- nrow(test_res)
        if (n_invalid > 0) {
          msg_test_result <- paste0(n_invalid, " invald rows")
          msg_color <- "red"
        }

        # screen output
        desc_label <- paste0("test that `", desc, "` ") |> stringi::stri_pad_right(100, ".")
        cli::cat_line(desc_label, " ", msg_test_result, col = msg_color)

        # pick columns
        if (is.null(cols_keep)) {
          cols_keep <- colnames(test_res)
        } else {
          cols_keep <- base::intersect(c(private$cols_to_keep, cols_keep), colnames(test_res))
        }

        # output to results (only DT results are recorded)
        private$.results[[test_name]] <- list(name = test_name, desc = desc, data = test_res[, ..cols_keep])
      }

      invisible(self)
    },
    #' @description
    #' Summarise the results for all tests
    #' @param invalid_only Logical, only show invalid tests
    #' @param sorted Logical, whether to reorder the tests by \code{test_name}
    summary = function(invalid_only = TRUE, sorted = FALSE) {
      toc <- lapply(
        private$.results,
        function(.x) data.table(Test = .x$desc, Sheet = .x$name, InvalidRows = nrow(.x$data))
      ) |> rbindlist()

      if (invalid_only) {
        toc <- toc[InvalidRows > 0]
      }

      if (sorted) {
        toc <- toc[order(Sheet)]
      }

      toc
    },
    #' @description
    #' Output test summary and invalid datasets to Excel
    #' @param file File path for excel output
    write_to_excel = function(file) {
      toc <- self$summary()

      failed_tests <- private$.results |>
        Filter(f = function(x) !is.null(x$data)) |>
        lapply(function(x) x$data)

      failed_tests <- failed_tests[toc$Sheet]

      hs <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                                  halign = "center", valign = "center", textDecoration = "Bold",
                                  border = "TopBottomLeftRight")

      sheets <- c(list(TOC = toc), failed_tests)
      names_sheets <- names(sheets)

      wb <- openxlsx::createWorkbook()

      for (ii in seq_along(sheets)) {
        ws_data <- sheets[[ii]]
        ws_name <- names_sheets[[ii]]

        openxlsx::addWorksheet(wb, sheet = ws_name)
        openxlsx::writeData(wb, ws_name, ws_data, headerStyle = hs)
        openxlsx::setColWidths(wb, sheet = ws_name, cols = seq_along(ws_data), widths = "auto")
      }

      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
      invisible(self)
    },
    #' @description
    #' For S3 print method
    print = function() {
      tbl_summary <- self$summary(invalid_only = FALSE)

      cat("<DataTester>: \n")
      if (nrow(tbl_summary) > 0) {
        cat("  number of tests: ", nrow(tbl_summary), "\n", sep = "")
        cat("  number of failed tests: ", tbl_summary[InvalidRows > 0, .N], "\n", sep = "")
      } else {
        cat("  0 test")
      }

      invisible(self)
    },
    #' @description
    #' Overwrite keep_cols on the flight
    #' @param cols_keep A character vector for columns to keep
    keep_cols = function(cols_keep = NULL) {
      private$.cols_to_keep <- cols_keep
      invisible(self)
    }
  ),
  private = list(
    .results = list(),
    .cols_to_keep = c()
  )
)
