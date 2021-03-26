library(ggplot2)
library(distr)
set.seed(12345)

die <- 1:6
roll <- function(n) {
  mean(sample(die, size = n, replace = TRUE))
}

plot(sapply(1:1000, roll), type = "l", xlab = "No. of dice", ylab = "Average", 
     main = "Simulation of 1000 dice rolls")
abline(h = 3.5, col = "red")