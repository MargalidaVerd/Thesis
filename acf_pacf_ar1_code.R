set.seed(123)

phi_positive <- 0.8
phi_negative <- -0.8

sim_positive <- arima.sim(model = list(ar = phi_positive), n = 100)
sim_negative <- arima.sim(model = list(ar = phi_negative), n = 100)

custom_acf_plot <- function(ts_data, max_lag = 25) {
  acf_vals <- acf(ts_data, plot = FALSE)
  acf_df <- with(acf_vals, data.frame(
    Lag = lag,
    ACF_value = acf
  ))
  
  conf <- qnorm(0.975) / sqrt(length(ts_data))
  acf_df$UCL <- conf
  acf_df$LCL <- -conf
  
  ggplot(acf_df, aes(x = Lag, y = ACF_value)) +
    geom_hline(yintercept = 0, color = "#1f77b4") +
    geom_ribbon(aes(ymin = LCL, ymax = UCL), fill = "#1f77b4", alpha = 0.2) +
    geom_segment(aes(xend = Lag, yend = 0), color = "#1f77b4") +
    geom_point(size = 1.5, color = "#1f77b4") +
    coord_cartesian(ylim = c(-0.5, 1.01)) +
    scale_y_continuous(breaks = seq(-0.5, 1.01, by = 0.25)) +
    scale_x_continuous(breaks = seq(0, max_lag, by = 2)) +
    theme_minimal() +
    theme(
      panel.grid   = element_blank(),
      axis.title   = element_text(size = 12), 
      axis.text    = element_text(size = 12)
    ) +
    labs(title = "", x = "Lag", y = "Value")
}

custom_pacf_plot <- function(ts_data, max_lag = 25) {
  pacf_vals <- pacf(ts_data, plot = FALSE)
  pacf_df <- with(pacf_vals, data.frame(
    Lag = lag,
    PACF_value = acf
  ))
  
  conf <- qnorm(0.975) / sqrt(length(ts_data))
  pacf_df$UCL <- conf
  pacf_df$LCL <- -conf
  
  ggplot(pacf_df, aes(x = Lag, y = PACF_value)) +
    geom_hline(yintercept = 0, color = "#1f77b4") +
    geom_ribbon(aes(ymin = LCL, ymax = UCL), fill = "#1f77b4", alpha = 0.2) +
    geom_segment(aes(xend = Lag, yend = 0), color = "#1f77b4") +
    geom_point(size = 1.5, color = "#1f77b4") +
    coord_cartesian(ylim = c(-0.7, 1.01)) +
    scale_y_continuous(breaks = seq(-0.5, 1.01, by = 0.25)) +
    scale_x_continuous(breaks = seq(0, max_lag, by = 2)) +
    theme_minimal() +
    theme(
      panel.grid   = element_blank(),
      axis.title   = element_text(size = 12), 
      axis.text    = element_text(size = 12)
    ) +
    labs(title = "", x = "Lag", y = "Value")
}


custom_acf_plot(sim_positive)
custom_pacf_plot(sim_positive)

custom_acf_plot(sim_negative)
custom_pacf_plot(sim_negative)

