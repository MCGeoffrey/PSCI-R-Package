plot_function <- function(data) {
  ggplot(data, aes(x = x_variable, y = y_variable, color = group_variable)) +
    geom_point() +
    theme_minimal()
}

my_summary_function <- function(data) {
  summary_stats <- summary(data)
  mean_value <- mean(data$x_variable, na.rm = TRUE)
  sd_value <- sd(data$x_variable, na.rm = TRUE)
  list(Summary = summary_stats, Mean = mean_value, SD = sd_value)
}
