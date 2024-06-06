#' Create a Scatter Plot with Correlation
#'
#' This function creates a scatter plot with optional calculations of correlation
#' and regression line. Additional customizations for axis limits, labels, titles,
#' and minor ticks are available.
#'
#' @param df Data frame containing the variables to plot.
#' @param var_x Name of the variable for the x-axis.
#' @param var_y Name of the variable for the y-axis.
#' @param title Main title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param x_axis_label Label for the x-axis.
#' @param x_limits Limits for the x-axis (numeric vector of length 2).
#' @param y_axis_label Label for the y-axis.
#' @param y_limits Limits for the y-axis (numeric vector of length 2).
#' @param legend_position Position of the legend annotation (numeric vector of length 2).
#' @param pt_size Size of the scatter points.
#' @param cor Whether to calculate correlation and display regression line. Default is "lm" for linear model, can be set to FALSE to avoid calculation.
#' @param cor_method Method for calculating correlation. Default is "pearson". Other options are "kendall" or "spearman".
#' @param cor_line_col Color for the correlation line.
#' @param cor_se Whether to show standard error for the correlation line. Default is TRUE.
#' @param cor_se_col Color for the standard error of the correlation line.
#' @param minor_ticks Numeric vector of length 2, controlling the number of minor ticks on x and y axes respectively.
#'
#' @return A ggplot2 object representing the scatter plot.
#'
#' @examples
#' library(CTKplot)
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#' ctk.scatter(df, "x", "y", title = "Scatter Plot", subtitle = "Sample Data")
#'
#' @import dplyr ggplot2 ggprism scales
#' @export
ctk.scatter <- function(
    df,
    var_x,
    var_y,
    title = NULL,
    subtitle = NULL,
    x_axis_label = NULL,
    x_limits = NULL,
    y_axis_label = NULL,
    y_limits = NULL,
    legend_position = NULL,
    pt_size = 2,
    cor = "lm",
    cor_method = "pearson",
    cor_line_col = "firebrick2",
    cor_se = T,
    cor_se_col = "gray",
    minor_ticks = c(1, 1)
) {
  # Check
  ggplot2_version <- utils::packageVersion("ggplot2")
  if (ggplot2_version < "3.5.0") {
    stop("This function requires ggplot2 version 3.5.0 or higher.")
  }

  # Subset data
  df_sub <- select(df, all_of(c(var_x, var_y)))

  # Remove NA rows
  df_sub <- na.omit(df_sub)

  # Calculate parameter
  if (is.null(x_limits)) {
    x_limits = c(min(df_sub[[var_x]]), max(df_sub[[var_x]]))
  }

  if (is.null(y_limits)) {
    y_limits = c(min(df_sub[[var_y]]), max(df_sub[[var_y]]))
  }

  if (is.null(legend_position)) {
    legend_position = c(
      min(df_sub[[var_x]]),
      max(df_sub[[var_y]])
    )
  }

  # Scatter Plot
  p <- df_sub %>%
    ggplot(aes_string(x = var_x, y = var_y)) +
    geom_point(size = pt_size) +
    scale_x_continuous(limits = x_limits) +
    scale_y_continuous(limits = y_limits)

  # Add minor ticks
  if (!isFALSE(minor_ticks)) {
    x_ticks <- pretty(x_limits)
    x_interval <- diff(x_ticks)[1]
    y_ticks <- pretty(y_limits)
    y_interval <- diff(y_ticks)[1]

    p <- suppressMessages(
      p +
        guides(x = guide_axis(minor.ticks = T), y = guide_axis(minor.ticks = T)) +
        scale_x_continuous(breaks = x_ticks, minor_breaks = breaks_width(x_interval/(1 + minor_ticks[1]))) +
        scale_y_continuous(breaks = y_ticks, minor_breaks = breaks_width(y_interval/(1 + minor_ticks[2])))
    )
  }
  # Correlation test
  if (!isFALSE(cor)) {
    # Fit model
    var_x <- make.names(var_x)
    var_y <- make.names(var_y)
    fmla <- reformulate(termlabels = var_y, response = var_x)
    if (cor == "lm") fit <- lm(fmla, data = df_sub)

    # Extract slope and intercept
    slope <- coef(fit)[2]
    intercept <- coef(fit)[1]

    # Coefficients
    correlation <- cor.test(df_sub[[var_x]], df_sub[[var_y]], method = cor_method)
    r_value <- correlation$estimate
    p_value <- correlation$p.value

    # Create annotation text
    regression_eq <- paste0("y = ", round(slope, 2), "x + ", round(intercept, 2))
    r_text <- paste0("r = ", round(r_value, 2))
    p_text <- paste0("p-value = ", format.pval(p_value, digits = 3))
    annotation <- paste(regression_eq, r_text, p_text, sep = "\n")

    # Add to plot
    p <- suppressMessages(
      p +
        geom_smooth(
          method = "lm",
          color = cor_line_col,
          se = cor_se,
          fill = cor_se_col) +
        annotate(
          "text",
          x = legend_position[1],
          y = legend_position[2],
          label = annotation,
          hjust = "inward",
          vjust = "inward"
        )
    )
  }

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
    scale_color_prism("floral") +
    scale_fill_prism("floral") +
    theme_prism(base_size = 12) +
    theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
          axis.ticks.length = unit(5, "pt"),
          theme(axis.minor.ticks.length = rel(0.25)))

  if (!is.null(title)) {
    p <- p + theme(plot.title = element_text(hjust = 0))
  }

  if (!is.null(subtitle)) {
    p <- p + theme(plot.subtitle = element_text(hjust = 0))
  }

  return(p)
}
