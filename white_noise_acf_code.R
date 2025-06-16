set.seed(16102001)

wn <- rnorm(200, mean = 0, sd = 1)

plot(wn, type = "l", col = "#1f77b4", lwd = 1,
     main = "",
     xlab = "Time", ylab = "Value")

y_ts <- ts(wn)
acf_vals <- acf(y_ts, plot = FALSE)
pacf_vals <- pacf(y_ts, plot = FALSE)


acf_df <- with(acf_vals, data.frame(
  Lag = lag,
  ACF_value = acf
))


pacf_df <- with(pacf_vals, data.frame(
  Lag = lag,
  PACF_value = acf
))

confidence_interval <- qnorm((1 + 0.95)/2)/sqrt(length(y))

acf_df$UCL <- conf
acf_df$LCL <- -conf

pacf_df$UCL <- conf
pacf_df$LCL <- -conf

acf_plot <- ggplot(acf_df, aes(x = Lag, y = ACF_value)) +
  geom_hline(yintercept = 0, color = "#1f77b4") +
  geom_ribbon(aes(ymin = LCL, ymax = UCL), fill = "#1f77b4", alpha = 0.2) +
  geom_segment(aes(xend = Lag, yend = 0), color = "#1f77b4") +
  geom_point(size = 1.5, color = "#1f77b4") +
  coord_cartesian(ylim = c(-0.5, 1.01)) +
  scale_y_continuous(breaks = seq(-0.5, 1.01, by = 0.25)) +
  scale_x_continuous(breaks = seq(0,25, by = 2)) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = "", x = "Lag", y = "Value")

acf_plot

pacf_plot <- ggplot(pacf_df, aes(x = Lag, y = PACF_value)) +
  geom_hline(yintercept = 0, color = "#1f77b4") +
  geom_ribbon(aes(ymin = LCL, ymax = UCL), fill = "#1f77b4", alpha = 0.2) +
  geom_segment(aes(xend = Lag, yend = 0), color = "#1f77b4") +
  geom_point(size = 1.5, color = "#1f77b4") +
  coord_cartesian(ylim = c(-0.5, 1.01)) +
  scale_y_continuous(breaks = seq(-0.5, 1.01, by = 0.25)) +
  scale_x_continuous(breaks = seq(0,25, by = 2)) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = "", x = "Lag", y = "Value")

pacf_plot
