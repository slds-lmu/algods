

library(knitr)
library(ggplot2)
library(microbenchmark)


library("random")
x = randomNumbers(5000, min = 1, max = 1e+06, col = 2) / 1e+06
summary(x)
x = load2("random.rda")

par(mfrow = c(1, 2))
colnames(x) = c("x", "y")
plot(x, pch = ".")
hist(matrix(x, ncol = 1), freq = FALSE, main = "Histogram", xlab = "c(x,y)")








rtrunc = function(n, pf = pnorm, qf = qnorm,
                  interval = c(-Inf, Inf)) {
  u = runif(n)
  y = qf(pf(interval[1]) + u*(pf(interval[2]) - pf(interval[1])))
  return(y)
}

set.seed(111)
pf = function(x) pgamma(x, shape = 2)
qf = function(x) qgamma(x, shape = 2)
x = rtrunc(10000, pf = pf, qf = qf, interval = c(0.1, 3))
range(x)
hist(x, freq = FALSE, breaks = 50)
lines(density(x))




n = 1000
p = 0.25
u = runif(n)
k = ceiling(log(1 - u) / log(1 - p)) - 1

hist(k, freq = FALSE, breaks = seq(-0.5, max(k) + 0.5, by = 1))
abline(v = 1/p, col = "green") # Mean
lines(0:30, dgeom(0:30, 1/4), col = "blue")




z = rnorm(100, mean = 0, sd = 1)
v = rchisq(100, df = 3)
t = z/sqrt(v/3)

qqplot(qt(ppoints(100), df = 3), t, xlim = c(-6, 6), ylim = c(-6, 6),
       main="QQ-Plot", xlab = "Theoretical t-Dist (df=3)",
       ylab = "Sample from Transformation")
qqline(t, distribution = function(p) qt(p, df=3))




n = 1000
x1 = rnorm(n, 0, 1)
x2 = rnorm(n, 3, 0.5)

u = runif(1000)
k = as.integer(u > 0.5)
x = k * x1 + (1 - k) * x2

hist(x, freq=FALSE, breaks=25)
lines(density(x))




n = 1000
mu = c(-0.5, 1.5)
Sigma = matrix(c(1, 0.7, 0.7, 1), ncol = 2, byrow = TRUE)
d = length(mu)
Z = matrix(rnorm(n * d), nrow = n, ncol = d)

ev = eigen(Sigma, symmetric = TRUE)
lambda = ev$values
V = ev$vectors
Q = V %*% diag(sqrt(lambda)) %*% t(V)
X = Z %*% Q + matrix(mu, n, d, byrow = TRUE)

# For comparison drawing from MVN with package mvtnorm
library(mvtnorm)
rmv <- rmvnorm(n, mu, Sigma)

  par(mfrow=c(1,3), mar=c(6, 2, 6, 1))
plot(Z[, 1], Z[, 2], xlim = c(-4,4), ylim = c(-4,4),
     cex = 0.5, main = "Std. MVN", xlab="", ylab="")
plot(X[, 1], X[, 2], xlim = c(-4,4), ylim = c(-4,4),
     cex = 0.5, main = "Transformed", xlab="", ylab="")
plot(rmv[,1], rmv[,2], xlim = c(-4,4), ylim = c(-4,4),
     cex = 0.5, main = "mvtnorm", xlab="", ylab="")
