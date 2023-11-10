#' Plot Dev components
#'
#' @export
#' @examples
#' DT1 <- import_PRTemplate_Dev(wb = r"(C:\GC Projects\2024\PICC\NM SP\20231102_TreatyAnalysis_2023Q3_FinalData\PICC_2024_NMSP_Trending_20231105.xlsm)", LOB = c("Property", "MB", "CEAR"), LRSummary ="Smry_BeforeDed")[LOB == "Property"]
#' op_plot_Dev(DT1)
#' op_plot_Dev(DT1, label_format = num_format("P"), label_projected = "Proj.")
#' op_plot_Dev(DT1, label_format = num_format("P"), geom_text_param = list(nudge_y = .1, color = "#5bbcd6"))
#' op_plot_Dev(DT1, var = "ALC.Actual", label_projected = "Proj.")
#' op_plot_Dev(DT1, var = "ALC.Actual", var_plot_type = "facet.with.total", label_format = num_format("P", 1))
#' op_plot_Dev(DT1, var = "POI")
#' op_plot_Dev(DT1, var = "POI", scale_y_param = list(limits = c(0, 1.2), breaks = scales::breaks_width(.2)))
#' op_plot_Dev(DT1, DT1, var = "POI", xy_label = c("Q3 Study", "Q2 Study"), x_text_angle = 45)

op_plot_Dev <- function(
    x,
    y = NULL,
    var = NULL,
    palette = NULL,
    div_by = "UltPrem", ## can be NULL
    num_scale = NULL,
    label_projected = NULL,
    label_format = NULL,
    geom_text_param = list(),
    scale_y_param = list(),
    var_plot_type = c("stack", "facet", "facet.with.total"),
    xy_label = c("Prior", "Current"),
    ...
) {
  var_plot_type <- match.arg(var_plot_type)

  # Plot one dataset or two
  if (is.null(y)) {
    pDT <- data.table::copy(x)
    id_cols <- "UY"
  } else {
    pDT <- data.table::rbindlist(setNames(list(x, y), nm = xy_label), idcol = "AsAt", fill = TRUE)
    pDT[, AsAt := factor(AsAt, xy_label)]
    id_cols <- c("AsAt", "UY")
  }

  # Calculate Attr, OS and IBNR
  pDT[
    j = `:=`(
      UltAttritional = UltIncurred - UltLarge - UltEvent,
      DummyAttritional = UltIncurred - ActualLarge - ActualEvent,
      OS = ActualIncurred - ActualPaid,
      IBNR = UltIncurred - ActualIncurred
    )]

  # stack variables: they will be melt into long-data
  if (is.null(var)) {
    stack_vars <- "UltIncurred"
  } else if (var == "POI") {## P(aid)O(S)I(BNR)
    stack_vars <- c("Paid" = "ActualPaid", "O/S" = "OS", "IBNR" = "IBNR")
  } else if (var %in% c("ALC.Ult", "ALE.Ult", "ALC.Ultimate", "ALE.Ultimate")) { ## A(ttritional)L(arge)C(aT)
    stack_vars <- c("Attritional" = "UltAttritional", "LargeRisk" = "UltLarge", "Event" = "UltEvent")
  } else if (var %in% c("ALC.Actual", "ALE.Actual")) {
    stack_vars <- c("Attritional (and IBNR)" = "DummyAttritional", "LargeRisk" = "ActualLarge", "Event" = "ActualEvent")
  } else if (var %in% c("ALC", "ALE")) {
    stack_vars <- c("Attritional (and IBNR)" = "DummyAttritional", "LargeRisk" = "ActualLarge", "Event" = "ActualEvent")
    warning("Actual ", var, " will be plot.  Please use ALC.Actual or ALE.Actual instead.")
  } else {
    stack_vars <- var
  }

  stack_vars <- vec_setnames(stack_vars, overwrite = FALSE)
  hide_fill_legend <- length(stack_vars) == 1 ## for ggplot setting

  num_cols <- unname(stack_vars)
  stack_labels <- names(stack_vars)

  # fill palette for ggplot2
  if (is.null(palette)) {
    if (length(stack_vars) == 1) {
      palette <- "#8abbd0"
    } else if (any(stringi::stri_detect_regex(num_cols, "Paid|OS|IBNR"))) {
      palette <- c("#5bbcd6", "#abddde", "#d3dddc")
    } else if (any(stringi::stri_detect_regex(num_cols, "Large|Cat|Event"))) {
      palette <- palette_op(idx = c(1, 6, 5))
    } else {
      palette <- palette_op()
    }
  }

  palette <- setNames(palette[seq_along(stack_labels)], stack_labels)

  # Divided by any basis
  if (is.null(div_by)) {
    pDT[, div_base := 1]
  } else if (div_by %in% colnames(pDT)) {
    pDT[, div_base := get(div_by)]
  } else {
    stop("Cannot find `", div_by, "` in DT_Dev.", call. = FALSE)
  }

  pDT_div <- pDT[, lapply(.SD, function(x) x /  div_base), by = id_cols, .SDcols = num_cols]

  # Decide if to show "Projected"
  name_Proj <- c("Projected", "Projection", "Proj", "Proj.")
  if (is.null(label_projected)) {
    pDT_div <- pDT_div[!UY %in% name_Proj]
  } else {
    pDT_div[UY %in% name_Proj, UY := label_projected]
  }

  # Melt stack_vars
  DT_plot <- data.table::melt(pDT_div, id.vars = id_cols)[
    j = `:=`(
      variable = factor(variable, num_cols, stack_labels)
  )]

  # Deal with facet setting
  if (var_plot_type != "stack") DT_plot[, variable := forcats::fct_rev(variable)]

  if (var_plot_type == "facet.with.total") {
    DT_plot <- rbind(DT_plot, DT_plot[, .(variable = "Total", value = sum(value, na.rm = TRUE)), by = id_cols]) # data.table automatically add Total as the last level
  }

  # Plot skeleton
  if (is.null(y)) {
    p <- DT_plot |>
      ggplot(aes(UY, value, fill = variable)) +
      geom_col(position = position_stack(reverse = TRUE), width = .75, na.rm = TRUE)

    if (var_plot_type != "stack") p <- p + facet_grid(variable ~ .)
  } else {
    p <- DT_plot |>
      ggplot(aes(AsAt, value, fill = variable)) +
      geom_col(position = position_stack(reverse = TRUE), width = .75, na.rm = TRUE)


    if (var_plot_type == "stack") {
      p <- p + facet_wrap("UY", nrow = 1)
    } else {
      p <- p + facet_grid(variable ~ "UY")
    }
  }

  ## Plot Label
  if (!is.null(label_format) && is.function(label_format)) {
    total_var <- "UltIncurred"
    if (length(stack_vars) == 1) total_var <- stack_vars

    if (var_plot_type == "stack") {
      ## stack vars only need to show total (e.g. not show Paid/OS/IBNR, but show Ultimate)
      DT_label <- DT_plot[, .(value = sum(value)), by = id_cols]
    } else {
      DT_label <- DT_plot
    }

    if (!"vjust" %in% names(geom_text_param)) geom_text_param <- c(list(vjust = 0), geom_text_param)

    p <- p +
      do.call(
        geom_text,
        c(
          list(
            data = DT_label,
            mapping = aes(fill = NULL, label = label_format(value))
          ),
          geom_text_param
        ))
  }

  # Plot wrap-up
  # y-scale formmater
  if (is.null(num_scale)) {
    if (is.null(div_by)) {
      num_scale <- "M"
    } else {
      num_scale <- "P"
    }
  }

  if (!"labels" %in% names(scale_y_param)) scale_y_param <- c(list(labels = num_format(num_scale)), scale_y_param)

  p <- p +
    scale_fill_manual(values = palette, guide = ifelse(hide_fill_legend, "none", "legend")) +
    do.call(scale_y_continuous, scale_y_param) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_op(show_axis = "x", show_grid = "y", show_border = !(is.null(y) && var_plot_type == "stack"), ...)

  p
}

#' Use `gt` package to lay out terms and structures
#'
#' @param data A list of terms produced by `import_PRTemplate_Term`
#' @export
op_plot_Term <- function(data, theme = "blue.fill") {
  theme <- "blue.fill"

  cols_format <- function(x) {
    res <- x |>
      gt::cols_width(1 ~ gt::px(160), everything() ~ gt::px(150)) |>
      gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_body(columns = 1))
    res
  }

  ## Structure
  p1 <- tempfile(fileext = ".png")
  data$Structure[, .(LOB, GrossRetention, QS, Lines, TreatyCapacity)] |>
    gt_op(theme = theme) |>
    gt::fmt_number(c(2, 5), suffixing = c(NA, "M", NA, NA), decimals = 0) |> # Retention and Capacity
    gt::fmt_number(4, decimals = 1) |> # Lines
    gt::fmt_percent(3, decimals = 0) |>
    gt::cols_label(.list = list(everything() ~ c("", "Gross\nRetention", "QS %", "Total Capacity"))) |>
    cols_format() |>
    gt::gtsave(p1)

  ## Commission
  p2 <- tempfile(fileext = ".png")

  if (data$Comm$FixedComm > 0) {
    data.table::data.table("V1" = "Fixed Commission", "Comm. %" = data$Comm$FixedComm, "Tax & Surcharge" = data$Comm$Tax) |>
      gt_op(theme = theme) |>
      gt::fmt_percent(2, decimals = 1) |>
      gt::fmt_percent(-(1:2), decimals = 2) |>
      gt::cols_label(V1 ~ "") |>
      cols_format() |>
      gt::gtsave(p2)
  } else {
    data.table::data.table(
      "V1" = "Sliding Commission",
      "LR1" = data$Sliding[, min(UpperLR)],
      "LR2" = data$Sliding[, max(LowerLR)],
      "Comm1" = data$Sliding[, min(Comm)],
      "Comm2" = data$Sliding[, max(Comm)],
      "Provisional" = data$Comm$Provisional,
      "Tax & Surcharge" = data$Comm$Tax
    ) |>
      gt_op(theme = theme) |>
      gt::fmt_percent(2:6, decimals = 1) |>
      gt::fmt_percent(-(1:6), decimals = 2) |>
      gt::cols_merge_range(LR1, LR2, sep = " - ") |>
      gt::cols_merge_range(Comm1, Comm2, sep = " - ") |>
      cols_format() |>
      gt::cols_label(V1 ~ "", LR1 ~ "L/R Range", Comm1 = "Comm. Range") |>
      gt::gtsave(p2)
  }

  image_vector <- c(magick::image_read(p1), magick::image_read(p2))
  file.remove(p1, p2)
  ## PC
  if (data$PC$PC > 0) {
    p3 <- tempfile(fileext = ".png")
    data.table::as.data.table(data$PC)[, .(V1 = "Profit Commission", PC, ME)] |>
      gt_op(theme = theme) |>
      gt::fmt_percent(2:3, decimals = 1) |>
      cols_format() |>
      gt::cols_label(.list = list(everything() ~ c("", "PC %", "M.E."))) |>
      gt::gtsave(p3)

    image_vector <- append(image_vector, magick::image_read(p3))
    file.remove(p3)
  }

  ## Loss Participation
  if (max(data$LPC$LPC) > 0) {
    p4 <- tempfile(fileext = ".png")
    DT_LPC <- data$LPC[, c(.(V1 = NA_character_), .SD)][LPC > 0][1, V1 := "Loss Participation"]
    DT_LPC |>
      gt_op(theme = theme) |>
      gt::fmt_percent(-1, decimals = 1) |>
      cols_format() |>
      gt::cols_merge_range(Lbound, Ubound, sep = " - ") |>
      gt::cols_label(.list = list(everything() ~ c("", "L/R Range", "LPC %"))) |>
      gt::gtsave(p4)

    image_vector <- append(image_vector, magick::image_read(p4))
    file.remove(p4)
  }

  ## Event Limit
  DT_EventLimit <- data.table::as.data.table(data$EventLimit)[, .SD, .SDcols = function(x) !is.na(x)]
  if (ncol(DT_EventLimit) > 0) {
    p5 <- tempfile(fileext = ".png")

    DT_EventLimit[, c(.(V1 = "Event Limit"), .SD)] |>
      gt_op(theme = theme) |>
      gt::fmt_number(-1, suffixing = c(NA, "M", NA, NA), decimals = 0) |>
      cols_format() |>
      gt::cols_label(1 ~ "") |>
      gt::gtsave(p5)

    image_vector <- append(image_vector, magick::image_read(p5))
    file.remove(p5)
  }

  ## Combine into ggplot object
  p_final <- tempfile(fileext = ".png")
  magick::image_append(image_vector, stack = TRUE) |>
    magick::image_write(p_final)

  pgg <- ggplot2::ggplot(data = data.table::data.table(x = 1, y = 1), aes(x, y)) +    # Modify image file
    ggplot2::theme_minimal() +
    ggplot2::annotation_custom(
      grid::rasterGrob(png::readPNG(p_final)),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    ggplot2::labs(x = NULL, y = NULL)

  file.remove(p_final)

  pgg
}
