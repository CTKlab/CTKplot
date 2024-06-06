#' This function is used to create a scatter plot and calculate correlation.
#'
#' @param df Data frame.
#' @param var_x Name of the variable for x-axis.
#' @param var_y Name of the variable for y-axis.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param x_axis_label Label for x-axis.
#' @param x_limits Limits for x-axis.
#' @param y_axis_label Label for y-axis.
#' @param y_limits Limits for y-axis.
#' @param legend_position Position of the legend.
#' @param pt_size Size of the points.
#' @param cor Whether to calculate correlation. Default is "lm", can be "FALSE" to avoid calculation.
#' @param cor_method Method for calculating correlation. Default is "pearson". One of "pearson", "kendall", or "spearman", can be abbreviated.
#' @param cor_line_col Color for correlation line.
#' @param cor_se Whether to show standard error for correlation line. Default is TRUE.
#' @param cor_se_col Color for standard error of correlation line.
#' @return Returns a ggplot2 plot object.
#' @export
#' @examples
#' library(CTKplot)
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#' ctk.scatter(df, "x", "y", title = "Scatter Plot", subtitle = "Sample Data")
#' @import dplyr ggplot2 ggprism
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
    cor_se_col = "gray"
) {
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
    p <- p +
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
    theme(plot.margin = unit(c(10, 10, 10, 10), "mm"))

  if (!is.null(title)) {
    p <- p + theme(plot.title = element_text(hjust = 0))
  }

  if (!is.null(subtitle)) {
    p <- p + theme(plot.subtitle = element_text(hjust = 0))
  }

  return(p)
}
