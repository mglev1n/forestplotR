#' @title Generate forest plot with aligned data table
#' @description \code{plot_forest} is a function that takes a data frame that at minimum contains columns containing an effect estimate, upper and lower confidence interval bounds, and p-values. The function returns a forest plot aligned with a data table containing this information. Additional variables may be displayed in a table to the left of the plot by passing their names to the \code{cols_left} argument.
#'
#' @param df a data frame
#' @param cols_left Names of columns to be included in a table that will be displayed to the left of the forest plot, passed within the \code{vars()} function. These may be descriptors that differentiate the values displayed in the forest plot.
#' @param ci_lower Name of column containing values of lower limit of confidence interval
#' @param ci_upper Name of column containing values of upper limit of confidence interval
#' @param ci_signif An integer passed to \code{signif} used to round the confidence interval to the specified number of significant digits
#' @param color Name of column that should be used to color points in forest plot. The default value is \code{NULL}
#' @param effect Name of column containing effect estimates (eg. Odds ratio or Beta)
#' @param effect_signif An integer passed to \code{signif} used to round the effect estimate to the specified number of significant digits
#' @param p_value Name of column containing P-value
#' @param size Integer or name of column used to scale points on forest plot
#' @param width_ratio Vector of integers representing relative widths of pieces of final plot. There should be one integer for each column passed to cols_left, the forest plot, and the results table.
#' @param xmin Integer representing the lower limit of the forest plot. If greater than the lowest value of the lower confidence interval, forest plot values will be truncated with an arrow. The lower confidence interval column can be passed in the following code to ensure no truncation occurs \code{xmin = min(c(1,.$or_lci95))}.
#' @param xmax Integer representing the lower limit of the forest plot. If less than the greatest value of the upper confidence interval, forest plot values will be truncated with an arrow. The upper confidence interval column can be passed in the following code to ensure no truncation occurs \code{xmin = min(c(1,.$or_lci95))}
#' @param hline Integer representing the location of vertical lines to be placed on the forest plot, useful for describin the origin (eg. \code{hline = 0} when plotting Odds Ratios, and \code{hline = 0} for betas)
#'
#' @return
#' @export
#'
#' @examples
#'
plot_forest <- function(df, cols_left, ci_lower = ci_lower, ci_upper = ci_upper, ci_signif = 2, color = NULL, effect = effect, effect_signif = 2, p_value = p_value, size = 2, width_ratio = c(1, 1, 1), xmin = 0.5, xmax = 2, hline = 1){

  color <- enquo(color)
  pointsize <- enquo(size)
  or <- enquo(effect)
  ci_lower <- enquo(ci_lower)
  ci_upper <- enquo(ci_upper)
  p_value <- enquo(p_value)

  # Create arrows for CI if outside plotting area
  df <- df %>%
    mutate(ci_lower_arrow = if_else(!!ci_lower < xmin, 0.25, NA_real_),
           ci_lower = if_else(!!ci_lower < xmin, xmin, !!ci_lower),
           ci_upper_arrow = if_else(!!ci_upper > xmax, 0.25, NA_real_),
           ci_upper = if_else(!!ci_upper > xmax, xmax, !!ci_upper))

  # Create row numbers
  df <- df %>%
    mutate(row = as.factor(row_number()))

  # Table plotting function
  plot_table <- function(x, name){
    ggplot(data = df, aes(y = row)) +
      geom_text(aes(label = x, x = name)) +
      theme_minimal() +
      theme(
        # aspect.ratio = 1/2,
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(face = "bold", color = "black", size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0,0,0, "cm")) +
      labs(x = "", y = "") +
      scale_x_discrete(position = "top")
  }

  # Create left table
  table_list_left <- df %>%
    select(!!!cols_left) %>%
    imap(plot_table)

  # return(cowplot::plot_grid(plotlist = c(table_list_left, table_list_left), align = c("h"), nrow = 1))
  # return(map(table_list_left, ggplotGrob))
  # return(grid.arrange(unlist(table_list_left), nrow = 1))

  # Create right table
  table_right <- df %>%
    mutate(CI = glue("[{signif(ci_lower, ci_signif)}-{signif(ci_upper, ci_signif)}]")) %>%
    ggplot(aes(y = row)) +
    # geom_text(aes(label = label, x = "PRS")) +
    # geom_text(aes(label = exposure_name, x = "Exposure")) +
    # geom_text(aes(label = PRS_Threshold, x = "PRS P-value\nThreshold")) +
    geom_text(aes(label = signif(!!or, !!effect_signif), x = "OR")) +
    geom_text(aes(label = CI, x = "95% CI")) +
    geom_text(aes(label = scientific_notation(signif(!!p_value, 1), 3), x = "P"), parse = TRUE) +
    theme_minimal() +
    theme(
      # aspect.ratio = 1/2,
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(face = "bold", color = "black", size = 12),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      # axis.text.y = element_text(face = "bold", size = 12),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(0,0,0,0, "cm"),
      panel.spacing.y = unit(2, "lines")) +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(position = "top", limits = c("OR", "95% CI", "P"))

  # Create plot
  plot <- df %>%
    ggplot(aes(row, !!or, ymin = ci_lower, ymax = ci_upper, color = !!color)) +
    # geom_errorbar(width = 0) +
    geom_point(aes(size = !!pointsize), shape = 15) +
    geom_segment(aes(x = row, xend = row, y = !!or, yend = ci_lower), arrow = arrow(type = "open", length = unit(df$ci_lower_arrow, "lines"))) +
    geom_segment(aes(x = row, xend = row, y = !!or, yend = ci_upper), arrow = arrow(type = "open", length = unit(df$ci_upper_arrow, "lines"))) +
    # geom_pointrange(shape = 15) +
    geom_hline(yintercept = hline, linetype = "dotted") +
    coord_flip() +
    # facet_wrap(~outcome) +
    scale_color_brewer(palette = "Set1", guide = FALSE) +
    scale_radius(range = c(1, 4), guide = FALSE) +
    scale_y_continuous(trans = "log2", breaks = scales::pretty_breaks(), limits = c(xmin, xmax)) +
    # scale_y_continuous(oob = scales::squish) +
    theme_light() +
    theme(
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      # axis.text.y = element_text(color = "black"),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "black"),
      plot.margin = margin(0,-0.5,0,0, "cm")
    ) +
    labs(x = "", y = "Odds Ratio (95% CI)")
  # return(table_right)
  # return(ggarrange(grid.arrange(grobs = c(table_list_left), nrow = 1), grid.arrange(table_right, nrow = 1)))
  # return(grid.arrange(grobs = c(table_list_left, table_right), nrow = 1))
  # return(egg::ggarrange(plots = c(table_list_left, list(plot), list(table_right)), nrow = 1, widths = width_ratio))
  return(cowplot::plot_grid(plotlist = c(table_list_left, list(plot), list(table_right)), align = c("h"), nrow = 1, rel_widths = width_ratio))
}
