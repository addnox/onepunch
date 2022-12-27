#' Save base plot as png files
#' @export
#' @inheritParams plot_save
plot_save_base <- function(plt, file, width = 9, height = 6) {
  if (basename(file) == file) {
    file_path <- paste0(getwd(), "/fig/")
    if (!dir.exists(file_path)) dir.create(file_path)
    file <- file.path(file_path, file)
  }

  png(file, width = width, height = height, units = "in", res = 300, type = "cairo")
  plt
  suppressMessages(dev.off())
  invisible(NULL)
}

#' Plot doughnut chart in base R (with specs mirroring the `pie` function)
#'
#' Slightly modified by based on codes in [https://r-graph-gallery.com/130-ring-or-donut-chart.html]
#' @export
#' @examples
#' doughnut(c(3, 5, 9, 12), labels = LETTERS[1:4], col = palette_op("economist")[c(1, 3, 5, 7)])
doughnut <- function (x, labels = names(x), edges = 200, outer.radius = 0.8,
            inner.radius=0.5, clockwise = FALSE,
            init.angle = if (clockwise) 90 else 0, density = NULL,
            angle = 45, col = NULL, border = FALSE, lty = NULL,
            main = NULL, ...){
  if (!is.numeric(x) || any(is.na(x) | x < 0))
    stop("'x' values must be positive.")
  if (is.null(labels))
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)

  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L])
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim

  plot.window(xlim, ylim, "", asp = 1)

  if (is.null(col))
    col <- if (is.null(density)) palette() else par("fg")

  col <- rep(col, length.out = nx)
  border <- rep(border, length.out = nx)
  if (!is.null(lty)) lty <- rep(lty, length.out = nx)
  angle <- rep(angle, length.out = nx)
  if (!is.null(density)) density <- rep(density, length.out = nx)
  twopi <- if (clockwise) -2 * pi else 2 * pi

  t2xy <- function(t, radius) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p),
         y = radius * sin(t2p))
  }

  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
              outer.radius)
    polygon(c(P$x, 0), c(P$y, 0), density = density[i],
            angle = angle[i], border = border[i],
            col = col[i], lty = lty[i])
    Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
      text(1.1 * Pout$x, 1.1 * Pout$y, labels[i],
           xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0),
           ...)
    }
    ## Add white disc
    Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                inner.radius)
    polygon(Pin$x, Pin$y, density = density[i],
            angle = angle[i], border = border[i],
            col = "white", lty = lty[i])
  }

  title(main = main, ...)
  invisible(NULL)
}

