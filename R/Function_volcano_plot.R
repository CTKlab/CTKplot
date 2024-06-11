#' Generate Volcano Plot
#'
#' This function generates a volcano plot for visualizing differential expression
#' data, with various customization options.
#'
#' @param df A data frame containing the differential expression data.
#' @param var_x A character string specifying the column name for the x-axis variable (e.g., log2 fold change).
#' @param var_y A character string specifying the column name for the y-axis variable (e.g., p-value adjust).
#' @param Symbol An optional character string specifying the column name for gene symbols or identifiers.
#' @param title An optional character string specifying the main title of the plot.
#' @param subtitle An optional character string specifying the subtitle of the plot.
#' @param x_axis_label An optional character string specifying the label for the x-axis.
#' @param x_limits A numeric vector of length 2, specifying the limits for the x-axis.
#' @param y_axis_label An optional character string specifying the label for the y-axis.
#' @param y_limits A numeric vector of length 2, specifying the limits for the y-axis.
#' @param FC_cut A numeric vector of length 2, specifying the cut-off values for fold change.
#' @param FC_cut_line A logical value indicating whether to draw dashed lines for fold change cut-offs.
#' @param Q_cut A numeric value specifying the cut-off value for the q-value or adjusted p-value.
#' @param Q_cut_line A logical value indicating whether to draw a dashed line for the q-value cut-off.
#' @param pt_color A character vector of length 4, specifying the colors for up-regulated, down-regulated, non-significant, and highlighted points.
#' @param pt_size A numeric value specifying the size of the points.
#' @param pt_alpha A numeric value between 0 and 1, specifying the transparency of the points.
#' @param Highlight An optional vector specifying the gene symbols or identifiers to be highlighted, or a numeric value specifying the number of top up/down-regulated genes to be highlighted.
#' @param legend A logical value indicating whether to show the legend.
#' @param label A logical value indicating whether to label the highlighted points.
#' @param label_size A numeric value specifying the size of the labels for highlighted points.
#' @param overlaps A numeric value specifying the maximum number of overlapping labels allowed.
#' @param ticks A numeric vector of length 2, specifying the number of ticks for the x and y axes.
#' @param minor_ticks A numeric vector of length 2, specifying the number of minor ticks for the x and y axes.
#' @param na_omit A logical value indicating whether to remove rows with missing values.
#'
#' @return A ggplot object representing the volcano plot.
#'
#' @examples
#' # Generate a simple volcano plot
#' ctk.volcano(df, var_x = "log2FoldChange", var_y = "pvalue")
#'
#' # Generate a volcano plot with customized settings
#' ctk.volcano(df, var_x = "log2FC", var_y = "padj",
#'             x_axis_label = "Log2 Fold Change",
#'             y_axis_label = "-Log10 Adjusted P-value",
#'             FC_cut = c(-1, 1), Q_cut = 0.05,
#'             Highlight = c("GENE1", "GENE2", "GENE3"),
#'             label = TRUE, label_size = 4)
#'
#' @import ggplot2 ggprism ggrepel dplyr
#' @export
ctk.volcano <- function(
    df,
    var_x,
    var_y,
    Symbol = NULL,
    title = NULL,
    subtitle = NULL,
    x_axis_label = NULL,
    x_limits = NULL,
    y_axis_label = NULL,
    y_limits = NULL,
    FC_cut = c(-1, 1),
    FC_cut_line = T,
    Q_cut = 0.05,
    Q_cut_line = T,
    pt_color = c("firebrick3", "navy", "grey", "orange"),
    pt_size = 2,
    pt_alpha = 0.5,
    Highlight = NULL,
    legend = F,
    label = T,
    label_size = 3,
    overlaps = 50,
    ticks = c(5, 5),
    minor_ticks = c(1, 1),
    na_omit = T
) {
  # Check
  ggplot2_version <- utils::packageVersion("ggplot2")
  if (ggplot2_version < "3.5.0") {
    stop("This function requires ggplot2 version 3.5.0 or higher.")
  }

  # Subset data
  df_sub <- as.data.frame(row.names(df))
  df_sub$x <- df[, var_x]
  df_sub$y <- df[, var_y]
  df_sub$log_y <- -log10(df_sub$y)

  # Add Symbol
  df_sub$lab <- row.names(df_sub)
  if (!is.null(Symbol)) df_sub$lab <- df[, Symbol]


  # Remove NA rows
  if (na_omit) df_sub <- na.omit(df_sub)

  # Calculate parameter
  if (is.null(x_limits)) {
    x_limits = c(min(df_sub$x), max(df_sub$x))
  }

  if (is.null(y_limits)) {
    y_nonzero <- -log10(df_sub$y[df_sub$y > 0])
    y_limits = c(min(y_nonzero), max(y_nonzero))
  }

  # Create new categorical column
  df_sub <- df_sub %>%
    mutate(type = case_when(
      x >= FC_cut[2] & y <= Q_cut ~ "up",
      x <= FC_cut[1] & y <= Q_cut ~ "down",
      T ~ "ns")
    )

  df_sub <- df_sub %>%
    mutate(order = case_when(
      type == "ns" ~ 1,
      type == "down" ~ 2,
      type == "up" ~ 3
    ))

  df_sub <- df_sub %>%
    arrange(order)

  # Add color parameters
  cols <- c("up" = pt_color[1], "down" = pt_color[2], "ns" = pt_color[3])

  # Highlight
  if (!is.null(Highlight)) {
    # Top n
    if (is.numeric(Highlight) & length(Highlight) == 1) {
      HL_point <- df_sub %>%
        filter(y <= Q_cut)%>%
        top_n(Highlight, wt = x)
    }

    if (is.numeric(Highlight) & length(Highlight) == 2) {
      HL_down <- df_sub %>%
        filter(y <= Q_cut)%>%
        top_n(Highlight[1], wt = x)
      HL_up <- df_sub %>%
        filter(y <= Q_cut)%>%
        top_n(Highlight[2], wt = x)
      HL_point <- rbind(HL_down, HL_up)
    }

    # Customized
    if (is.character(Highlight)){
      HL_point <- df_sub %>%
        filter(lab %in% Highlight)
    }
  }

  # Volcano plot
  p <- df_sub %>%
    ggplot(aes(x = x, y = log_y, label = Symbol)) +
    geom_point(aes(color = type), size = pt_size, alpha = pt_alpha) +
    scale_color_manual(values = cols) +
    scale_x_continuous(limits = x_limits) +
    scale_y_continuous(limits = y_limits)

  # Add lines
  if (Q_cut_line) p <- p + geom_hline(yintercept = -log10(Q_cut), linetype = "dashed")
  if (FC_cut_line) p <- p + geom_vline(xintercept = FC_cut, linetype = "dashed")

  # Add highlight
  if (!is.null(Highlight)) {
    if (label) {
      p <- p + geom_label_repel(
        data = HL_point,
        aes(label = lab),
        min.segment.length = 0,
        box.padding = 0.5,
        size = label_size,
        fontface = 2,
        max.overlaps = getOption("ggrepel.max.overlaps", default = overlaps),
      )
    }
    p <- p + geom_point(data = HL_point, shape = 21, size = 2, fill = pt_color[4], colour = "black")
  }

  # Add Legend
  if (!legend) {
    p <- p  + guides(size = "none", color = "none")
  }

  # label original axis
  p <- p + labs(x = var_x, y = paste0("-Log10(", var_y, ")"))

  # Ticks adjustment
  x_ticks <- pretty(x_limits, n = ticks[1])
  y_ticks <- pretty(y_limits, n = ticks[2])
  x_interval <- diff(x_ticks)[1]
  y_interval <- diff(y_ticks)[1]
  p <- suppressMessages(
    p +
      guides(x = guide_axis(minor.ticks = T), y = guide_axis(minor.ticks = T)) +
      scale_x_continuous(breaks = x_ticks, minor_breaks = breaks_width(x_interval/(1 + minor_ticks[1]))) +
      scale_y_continuous(breaks = y_ticks, minor_breaks = breaks_width(y_interval/(1 + minor_ticks[2]))))

  # Add titles and labels
  if (!is.null(x_axis_label)) {
    p <- p + labs(x = x_axis_label)
  }

  if (!is.null(y_axis_label)) {
    p <- p + labs(y = y_axis_label)
  }

  if (!is.null(title)) {
    p <- p + labs(title = title)
  }

  if (!is.null(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }

  # Set theme
  p <- p +
    theme_prism(base_size = 12) +
    theme(
      plot.margin = unit(c(10, 10, 10, 10), "mm"),
      axis.ticks.length = unit(5, "pt"),
      axis.minor.ticks.length = rel(0.5)
    )

  if (!is.null(title)) {
    p <- p + theme(plot.title = element_text(hjust = 0))
  }

  if (!is.null(subtitle)) {
    p <- p + theme(plot.subtitle = element_text(hjust = 0))
  }

  return(p)

}

