# ------------------------------------------------------------------------------
# monte carlo

# R code from examples in .tex file
# FIG: Gamma distribution with histogram and density line.
# ------------------------------------------------------------------------------

library(knitr)
library(ggplot2)
set.seed(333)

# Data -------------------------------------------------------------------------

T = 10000; shape = 2; rate = 1 / 2
theta = rgamma(T, shape = shape, rate = rate)

png("../figure_man/gamdistri_monte_carlo.png", width = 800, height = 600)
hist(theta, freq = FALSE, ylim = c(0, 0.2), main = "")
lines(density(theta))
dev.off()


(Etheta = mean(theta)) # MC estimator
(se.Etheta = sqrt(var(theta) / T)) # Variance of MC estimator
shape * 1 / rate # Theoretical expectation

(Ptheta = mean(theta > 5)) # MC Estimator
(se.Ptheta = sqrt(var(theta > 5) / T)) # Variance of MC estimator
1 - pgamma(5, shape = shape, rate = rate) # theoretical value
f = function(x) { dgamma(x, shape = shape, rate = rate) }
integrate(f, 5, Inf) # Numerical integration in R
