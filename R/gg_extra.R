#' Basic ggplot2 theme
#'
#' @importFrom ggplot2 %+replace%
#' @export

theme_op <- function(base_size = 12, base_family = "",
                     legend_position = "top",
                     show_axis = c("x", "xy", "y", "none"),
                     show_axis_text = c("xy", "x", "y", "none"),
                     show_grid = c("y", "xy", "x", "none"),
                     show_border = FALSE,
                     x_text_angle = 0,
                     x_text_hjust = NULL,
                     ...) {

  show_axis <- match.arg(show_axis)
  show_grid <- match.arg(show_grid)
  show_axis_text <- match.arg(show_axis_text)

  .theme <- ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.2), margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(color = "#7e7e7e", hjust = 0, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(color = "#7e7e7e", hjust = .1, face = "italic"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color = "#d4dddd"),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#F2F2F2", color = "#d4dddd", size = 0.7),
      #plot.margin = margin(base_size / 2, base_size / 2, base_size / 2, base_size / 2),
      legend.position = legend_position,
      complete = TRUE
    )

  if (x_text_angle != 0 && grepl("x", show_axis)) {
    if (x_text_angle > 5 && is.null(x_text_hjust)) x_text_hjust <- 1

    .theme <- .theme + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = x_text_angle, hjust = x_text_hjust, color = "#4a4a4a"))
  }

  if (show_axis == "x") {
    .theme <- .theme +
      ggplot2::theme(
        axis.line.x = ggplot2::element_line(),
        axis.ticks.x = ggplot2::element_line(color = "#4a4a4a")
      )
  } else if (show_axis == "y") {
    .theme <- .theme +
      ggplot2::theme(
        axis.line.y = ggplot2::element_line(),
        axis.ticks.y = ggplot2::element_line(color = "#4a4a4a")
      )
  } else if (show_axis == "xy") {
    .theme <- .theme +
      ggplot2::theme(
        axis.line = ggplot2::element_line(),
        axis.ticks = ggplot2::element_line(color = "#4a4a4a")
      )
  }

  if (show_axis_text == "x") {
    .theme <- .theme +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(color = "#4a4a4a")
      )
  } else if (show_axis_text == "y") {
    .theme <- .theme +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(color = "#4a4a4a")
      )
  } else if (show_axis_text == "xy") {
    .theme <- .theme +
      ggplot2::theme(
        axis.text = ggplot2::element_text(color = "#4a4a4a")
      )
  }

  if (show_grid == "x") {
    .theme <- .theme +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_line())
  } else if (show_grid == "y") {
    .theme <- .theme +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line())
  } else if (show_grid == "xy") {
    .theme <- .theme +
      ggplot2::theme(panel.grid.major = ggplot2::element_line())
  }

  if (!show_border) .theme <- .theme + ggplot2::theme(panel.border = ggplot2::element_blank())

  .theme <- .theme + ggplot2::theme(...)
  .theme
}

#' output a ggplot chart
#' @param plt Plot object
#' @param file File name (with path)
#' @param width,height Width and height in inches
#' @param scale Scale factor for `width` and `height`.  If `scale` is 2, and `width` is 10, then scaled width will be 20
#' @export
gg_save <- function(plt, file, width = 9, height = 6, scale = 1) {
  if (basename(file) == file) {
    file_path <- paste0(getwd(), "/fig/")
    if (!dir.exists(file_path)) dir.create(file_path)
    file <- file.path(file_path, file)
  }

  ggplot2::ggsave(file, plot = plt, width = width, height = height, scale = scale)
  (plt)
}

#' @export
#' @rdname gg_save

plot_save <- gg_save

#' A selection of color palettes
#'
#' @export
#' @examples
#' palette_op() |> plot()
#' palette_op("economist", c(1, 3, 5, 6)) |> plot()

palette_op <- function(name = c("economist", "gc"), idx = NULL) {
  name <- match.arg(name)
  paletteList <- list(
    "economist" = c("#01a2d9", "#014d64", "#6794a7", "#7ad2f6", "#00887d", "#76c0c1", "#7c260b", "#ee8f71", "#a18376", "#ecc265", "#f15a40")
    # "gc" = c("#002e5c", "#1490ad", "#046f80", "#6dc0c4", "#00977d", "#009e3c", "#76949d", "#8ccbb8", "#768278", "#a6b29f"),
  )

  paletteChosen <- paletteList[[name]]
  if (is.null(idx)) idx <- seq_along(paletteChosen)
  paletteChosen <- paletteChosen[idx]

  structure(paletteChosen, class = "ColorScheme", idx = idx)
}

#' @exportS3Method
plot.ColorScheme <- function(p, ...) {
  idx <- attr(p, "idx")
  barplot(height = rep(1, times = length(p)), col = p, names.arg = idx, border = NA, axes = FALSE)
}

#' Helper function for dual-y-axis plot
#' @param y_left,y_right Numeric vector.
#'   If with two elements, linear transformation will be used
#'   If longer than two, min/max (or zero/max, if `force_zero` is `TRUE`) are used for linear transformation
#' @export
#' @examples
#' a <- data.table(Year = 2021:2023, Value = c(10, 12, 9), Growth = c(-.1, .2, -.25))
#' trans1 <- transform_dual_axis(c(10, 20), c(0, .2))
#' trans2 <- transform_dual_axis(a$Value, a$Growth)
#' trans3 <- transform_dual_axis(a$Value, a$Growth, force_zero = TRUE)
#' ggplot(a, aes(Year, Value)) +
#' geom_col() +
#' geom_line(aes(y = trans2$to_left(Growth))) +
#' scale_y_continuous(sec.axis = sec_axis(trans = trans2$trans))
#'
transform_dual_axis <- function(y_left, y_right, force_zero = FALSE) {
  min_primary <- min(y_left, na.rm = TRUE)
  min_secondary <- min(y_right, na.rm = TRUE)
  max_primary <- max(y_left, na.rm = TRUE)
  max_secondary <- max(y_right, na.rm = TRUE)

  if (force_zero) {
    min_primary <- 0
    min_secondary <- 0
  }

  b <- (max_secondary - min_secondary) / (max_primary - min_primary)
  a <- max_secondary - b * max_primary

  to_left <- function(x) (x - a) / b
  to_right <- function(x) a + b * x # sec_axis's trans function is converting primary to secondary

  res <- list(
    to_left = to_left,
    trans = to_right,
    to_right = to_right
  )

  return(res)
}

#' Plot waterfall chart
#'
#' @param DT A `data.frame` with at least two columns, usually the 1st for labels and the 2nd for incremental values
#' @param balance_start A named character vector with length 1.  The `value` will be the initial balance, and the `name` will be the label
#' @param calc_balance A named character vector.  The `value`s will be the position (after which) to calculate on-going balance.  The `name`s will be the labels
#' @param label_final The label for final balance
#' @param color Colors to fill the rectangles, with the sequence for "increase", "decrease" and "balance"
#' @param color_border Color for the rectangle border.  Use `NA` if no color is wanted
#' @param xvar,yvar Column names for `x` (i.e. label) and `y` (i.e. value).  If `NULL`, then the 1st column of `DT` will be used as label (if it is of character type), and the 2nd as value (if it is of numeric type)
#' @param bar_width Width of rectangles
#' @param show_hline Show the link lines from the end of previous rectangle to the start of the next
#' @param show_value Show values as rectangle labels
#' @param labeller A function to format the value labels
#' @param ... To be passed to `theme_op`
#' @export
#' @examples
#' df <- data.table(x = c("LR", "LPC", "Comm", "PC"), y = -c(.7, -.1, .5, 0))
#' plot_waterfall(df, show_hline = T, show_value = T)
#' plot_waterfall(df, balance_start = c("Premium" = 1), show_hline = T, show_value = T, labeller = num_auto)
#' plot_waterfall(df, balance_start = c("Premium" = 1), calc_balance = c("Bal after LPC" = "LPC", "Treaty Balance" = "PC"), show_hline = T, color = c("green", "red", "grey"), x_text_angle = 45, show_axis_text = "x")

plot_waterfall <- function(
    DT,
    balance_start = NULL, calc_balance = NULL, label_final = "Final Balance",
    color = c("#9ae5de", "#efe8d1", "#acc8d4"), color_border = "#7f7f7f",
    x_var = NULL, y_var = NULL,
    bar_width = .5, show_hline = FALSE, show_value = FALSE, labeller = NULL, ...
  ) {
  dt <- data.table::as.data.table(DT)
  dt_colnames <- colnames(dt)
  # guess x and y var names
  if (is.null(x_var) & is.character(dt[[1]])) x_var <- dt_colnames[1]
  if (is.null(y_var) & is.numeric(dt[[2]])) y_var <- dt_colnames[2]
  data.table::setnames(dt, old = c(x_var, y_var), new = c("x", "y"))
  # prepare balance
  dt[, `:=`(id = .I, type = data.table::fifelse(y >= 0, "increase", "decrease"))]

  if (!is.null(balance_start)) {
    dt <- rbind(dt, data.table::data.table(id = .5, x = names(balance_start), y = balance_start, type = "balance"))
    dt <- dt[order(id)]
  }
  dt[, cum := cumsum(y)]

  if (is.null(calc_balance)) calc_balance <- setNames(tail(dt[["x"]], 1L), label_final)

  if (is.null(names(calc_balance))) names(calc_balance) <- paste0("Balance after ", calc_balance)
  dt_add <- dt[x %in% calc_balance][, `:=`(id = id + .5, x = names(calc_balance), type = "balance")]
  dt <- rbind(dt, dt_add)[order(id)]
  # start and end
  dt[,
     `:=`(
       xpos = .I,
       ystart = data.table::fifelse(type == "balance", 0, data.table::shift(cum, fill = 0)),
       yend = cum
     )
  ]

  # plot spec
  ## spec: fill
  if (length(color) >= 3) fill_colors <- color[1:3]
  if (length(color) < 3) stop("`color` must have 3 elements", call. = FALSE)
  if (is.null(names(fill_colors))) names(fill_colors) <- c("increase", "decrease", "balance")

  # plot
  p <- ggplot2::ggplot(dt, ggplot2::aes(xpos))

  p <- p + ggplot2::geom_rect(ggplot2::aes(fill = type, xmin = xpos - bar_width / 2, xmax = xpos + bar_width / 2, ymin = ystart, ymax = yend),
                              color = color_border) +
    ggplot2::scale_x_discrete(limits = dt$x) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    theme_op(legend_position = "none", ...)

  if (show_hline) {
    dt2 <- dt[, .(x = xpos + bar_width / 2, xend = data.table::shift(xpos, -1L) -  bar_width / 2, y = cum)][!is.na(xend)]
    p <- p + ggplot2::geom_segment(data = dt2, ggplot2::aes(x = x, xend = xend, y = y, yend = y), color = "#4a4a4a", linetype = "dotted")
  }

  if (show_value) {
    if (is.null(labeller)) labeller <- function(x) x

    p <- p + ggplot2::geom_text(ggplot2::aes(y = (ystart + yend) / 2, label = data.table::fifelse(type == "balance", labeller(cum), labeller(y))))
  }
  p
}
