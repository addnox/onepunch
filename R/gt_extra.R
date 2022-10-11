#' Basic format for `gt` table
#'
#' @export
#' @details
#' Some tips of using `gt` package
#' * use `gt::gtsave` to save table as png
#' * check `gt::cols_label` for column header renaming
#' * check `gt::cols_merge_range` to combine Lbound and Ubound into one
#' @examples
#' x <- data.table::data.table(
#'   " " = as.character(1:3),
#'   "Terms_Limit" = c(10e6, 10e6, 20e6),
#'   "Terms_Deductible" = c(10e6, 20e6, 30e6),
#'   "Terms_Reinstatement" = c("2@100%", "2@100%", "1@100%"),
#'   "Price_ROL" = c(.4, .35, NA)
#'   )
#'   gt_op(x, title = "Price Table")
#'   gt_op(x, title = "Price Table", title_block_color = "blue")
#'   gt_op(x, cols_pct = "Price_ROL", spanner_split = "_", theme = "green", title = "Price Table")

gt_op <- function(DT, theme = c("blue", "blue.fill", "green", "green.fill"), cols_pct = NULL, spanner_split = NA, title = NULL, title_block_color = "#ee1c25", missing_text = "",...) {
  DT <- data.table::as.data.table(DT)
  .cols_num_all <- DT[, names(.SD), .SDcols = is.numeric]
  .cols_num <- setdiff(.cols_num_all, cols_pct)
  .cols_pct <- intersect(names(x), cols_pct)

  theme <- match.arg(theme)
  .theme_color <- ifelse(grepl("^blue", theme), "blue", "cyan")
  .theme_style <- ifelse(grepl("fill$", theme), 6, 1)

  gt0 <- gt::gt(DT, ...) |>
    gt::fmt_number(.cols_num, suffixing = TRUE) |>
    gt::fmt_percent(.cols_pct) |>
    gt::sub_missing(missing_text = missing_text) |>
    gt::opt_stylize(style = .theme_style, color = .theme_color, add_row_striping = TRUE)

  # add spanner (i.e. column groups)
  if (!is.na(spanner_split))  {
    gt0 <- gt0 |>
      gt::tab_spanner_delim(delim = spanner_split)
  }

  if (!is.null(title)) {
    if (!is.na(title_block_color)) {
      gt0 <- gt0 |>
        gt::tab_header(title = gt::html("<div>", "<span style='background-color:", title_block_color, ";color:", title_block_color, ";'>...</span>","<span><b>", title, "</b></span></div>"))
    } else {
      gt0 <- gt0 |>
        gt::tab_header(title = gt::html("<div>","<span><b>", title, "</b></span></div>"))
    }
  }

  # final touch
  gt0 <- gt0 |>
    gt::tab_options(
      table.border.top.color = "transparent",
      heading.align = "left",
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold"
    )

  gt0
}
