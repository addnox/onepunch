#' Basic format for kable
#'
#' @export
#' @examples
#' x <- data.table::data.table(
#'   " " = "2020 Benchmark Price",
#'   "Terms_Limit" = c(10e6, 10e6, 20e6),
#'   "Terms_Deductible" = c(10e6, 20e6, 30e6),
#'   "Terms_Reinstatement" = c("2@100%", "2@100%", "1@100%"),
#'   "Price_ROL" = c(.4, .35, .25)
#'   )
#'   kable_op(x) |> print()
#'   kable_op(x, header_sep = "_") |> print()
#'   kable_op(x, header_sep = "_", auto_num_format = TRUE) |> print()
#'   kable_op(x, split_header = TRUE) |> kableExtra::collapse_rows(columns = 1) |> print()
# To-dos:
#
# kable_op(x) |> kableExtra::column_spec(2:3, background = "orange", extra_css = "text-align:left") |> print()
#
kable_op <- function(DT, theme = c("green.fill", "blue.fill"),
                     font_size = 14, header_sep = NULL, na_replace = "", cols_right = NULL) {
  if (!is.null(na_replace) & !is.na(na_replace)) options(knitr.kable.NA = na_replace)
  DT <- as.data.table(DT)
  ## theme settings
  theme <- match.arg(theme)
  if (theme == "blue.fill") {
    h_bold <- TRUE
    h_fill <- "#4a9396"
    h_color <- "#ffffff"
    row_color <- "black"
    row_color_stripe <- "black"
    row_fill <- "#fefefe"
    row_fill_stripe <- "#eaf6f6"
  } else {
    # default: green.fill
    h_bold <- TRUE
    h_fill <- "#4a9396"
    h_color <- "#ffffff"
    row_color <- "black"
    row_color_stripe <- "black"
    row_fill <- "#fefefe"
    row_fill_stripe <- "#eaf6f6"
  }

  ## headers
  dheader <- data.table(...name... = colnames(DT))
  if (!is.null(header_sep)) {
    dt_separate(dheader, col = "...name...", sep = header_sep)
    n_header <- ncol(dheader)
  } else {
    n_header <- 1L
  }
  bottom_header <- dheader[[n_header]]
  bottom_header[is.na(bottom_header)] <- ""

  ## align
  cols_right <- c(cols_right, onepunch::cnames(DT, is.numeric))
  v_align <- rep("l", ncol(DT)) ## default to be left-aligned
  v_align[colnames(DT) %in% cols_right] <- "r" ## numbers align to the right

  n_row <- nrow(DT)

  x <- knitr::kable(DT,
                    format = "html",
                    col.names = bottom_header,
                    align = v_align,
                    format.args = list(big.mark = ",", scientific = FALSE)
  ) |>
    kableExtra::kable_styling(
      #bootstrap_options = c("striped"),
      full_width = FALSE,
      position = "left", ## pos of table in html page
      font_size = font_size
      #stripe_color = "black"
    ) |>
    # header
    kableExtra::row_spec(0, bold = h_bold, color = h_color, background = h_fill, align = "center") |>
    kableExtra::row_spec(seq(1, n_row, by = 2), color = row_color, background = row_fill) |>
    kableExtra::row_spec(seq(2, n_row, by = 2), color = row_color_stripe, background = row_fill_stripe)

  if (n_header > 1) {
    for (i in seq(n_header - 1, 1)) {
      tmp_header <- dheader[[i]]
      tmp_header[is.na(tmp_header) | tmp_header == ""] <- " "
      tmp_dt <- data.table(name = tmp_header, id = data.table::rleid(tmp_header))
      tmp_sum <- tmp_dt[, .N, by = c("id", "name")]

      tmp_header_kable <- setNames(tmp_sum[["N"]], tmp_sum[["name"]])
      x <- x |> kableExtra::add_header_above(tmp_header_kable, bold = h_bold, color = h_color, background = h_fill)
    }
  }

  return(x)
}



#' Save formatted kable into png
#'
#' @export
kable_save <- function(x, file, zoom = 2, bs_theme = "simplex", extra_dependencies = NULL, ...) {
  dependencies <- list(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap(theme = bs_theme),
    html_dependency_kePrint()
  )
  if (!is.null(extra_dependencies)) {
    dependencies <- append(dependencies, extra_dependencies)
  }

  if (basename(file) == file) {
    file_path <- paste0(getwd(), "/fig/")
    if (!dir.exists(file_path)) dir.create(file_path)
    file <- file.path(file_path, file)
  }
  file_name <- basename(file)

  html_header <- htmltools::tags$head(dependencies)
  html_table <- htmltools::HTML(as.character(x))
  html_result <- htmltools::tagList(html_header, html_table)

  temp_dir <- tempdir()
  temp_dir_lib <- file.path(temp_dir, "lib")

  file_temp_html <- tempfile(pattern = tools::file_path_sans_ext(file_name), tmpdir = temp_dir, fileext = ".html")
  fs::file_create(file_temp_html)
  fs::file_create(file)

  htmltools::save_html(html_result, file = file_temp_html, libdir = temp_dir_lib)
  result <- webshot::webshot(file_temp_html, file, zoom = zoom, ...)

  fs::file_delete(file_temp_html)
  fs::dir_delete(temp_dir_lib)

  if (is.null(result)) {
    # A webshot could not be created. Delete newly created files and issue msg
    fs::file_delete(file)
    message('save_kable could not create image with webshot package. Please check for any webshot messages')
  } else {
    if (requireNamespace("magick", quietly = TRUE)) {
      img_rework <- magick::image_read(file)
      img_rework <- magick::image_trim(img_rework)
      img_rework <- magick::image_border(img_rework, color = "white", geometry = "10x10")
      img_info <- magick::image_info(img_rework)
      magick::image_write(img_rework, file)
      attr(file, "info") <- img_info
    }
  }

  invisible(file)
}

#' HTML dependency for js script to enable bootstrap tooltip and popup message
#'
html_dependency_kePrint <- function() {
  htmltools::htmlDependency(name = "kePrint",
                            version = "0.0.1",
                            src = system.file("kePrint-0.0.1",
                                              package = "kableExtra"),
                            script = "kePrint.js")
}

