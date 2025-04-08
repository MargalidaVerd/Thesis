set.seed(42)

wn <- rnorm(200, mean = 0, sd = 1)

ts.plot(wn, xlab = "Time")

abline(h = 0, col = "red", lty = 2)

acf(wn, main = "ACF")

hist(wn, breaks = 20, main = "Histogram of White Noise", col = "lightblue", probability = TRUE)
curve(dnorm(x, mean = mean(wn), sd = sd(wn)), add = TRUE, col = "red")
