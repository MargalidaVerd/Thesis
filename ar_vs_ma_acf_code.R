library(forecast)

set.seed(123)

ar1_process <- arima.sim(n = 100, model = list(ar = 0.8))

ma2_process <- arima.sim(n = 100, model = list(ma = c(0.5, 0.3)))

plot_acf_with_points <- function(ts_data, title) {
  acf_data <- acf(ts_data, plot = FALSE)
  acf(ts_data, main = title, ylim = c(-1, 1), col = "blue", lwd = 2)
  points(acf_data$lag, acf_data$acf, col = "red", pch = 19, cex = 0.5)}

  
plot_acf_with_points(ar1_process, "ACF of AR(1) Process (phi = 0.8)")
plot_acf_with_points(ma2_process, "ACF of MA(2) Process (theta1 = 0.5, theta2 = 0.3)")

