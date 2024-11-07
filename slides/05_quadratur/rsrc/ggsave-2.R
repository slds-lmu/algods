


library(knitr)
library(ggplot2)



makepost = function(y, shape, scale) {
  function(x) {
    dgamma(x, shape = y + shape, scale = 1 / (1 + 1 / scale))
  }
}


set.seed(1234)

y = 2
prior.shape = 3
prior.scale = 3

p = makepost(y, prior.shape, prior.scale)

pmode = (y + prior.shape - 1) * (1 / (1 + 1 / prior.scale))
pmean = (y + prior.shape) * (1 / (1 + 1 / prior.scale))

a = prior.shape
b = prior.scale

fhat = deriv3(~ mu^(y + a - 1) * exp(-mu * (1 + 1/b)) / ((1/(1+1/b))^(y+a) * gamma(y + a)), "mu", function.arg = TRUE)

post.shape = y + prior.shape - 1
post.scale = 1 / (length(y) + 1 / prior.scale)

curve(p, 0, 12, n = 1000, xlab = expression(x), ylab = expression(f(x)))











par(cex = 1.5)

y0 = 0.26 * c(0, 0, rep(1, 3), rep(0, 6))
sfun = stepfun(1:10, y0, f = 0)
plot(sfun, main = "", lty = 2, do.points = FALSE)
points(3, p(3), col = "red")
text(x = 4, 0.23, expression(paste("(", x[0], ", f(", x[0], "))")), col = "red")
curve(p, 0, 12, n = 1000, xlab = expression(x), ylab = expression(f(x)), add = TRUE)







lapprox = Vectorize(function(mu, mu0 = pmode) {
  deriv <- fhat(mu0)
  grad <- attr(deriv, "gradient")
  hess <- drop(attr(deriv, "hessian"))
  f <- function(x) dgamma(x, shape = post.shape, scale = post.scale)
  hpp <- (hess * f(mu0) - grad^2) / f(mu0)^2
  exp(log(f(mu0)) + 0.5 * hpp * (mu - mu0)^2)
}, "mu")

curve(p, 0, 12, n = 1000, xlab = expression(x), ylab = expression(f(x)))
curve(lapprox, 0.001, 12, n = 1000, add = TRUE, col = 2, lwd = 2)
legend("topright",
       legend = c("f", "Laplace"),
       lty = c(1, 1), lwd = c(1, 1), col = c(1, 2))








monte_carlo = function(f, a, b, n = 500L, plot = FALSE, plotHits = TRUE,
                       psty = 20, phsty = 4, res = 0.01, ...) {
  # n (int): number of points to be drawn
  
  # calculate top and bottom of rectangle,
  # from which x and y are drawn uniformly:
  target_set = sapply(seq(a,b,res), f) # values of f in [a,b]
  top = ceiling(max(target_set))
  bot = floor(min(target_set))
  
  rect_area = (b-a)*(top-bot)
  
  x = runif(n, a, b)
  y = runif(n, bot, top)
  x_eval = sapply(x, f)
  
  hit = (y <= x_eval) & (x_eval >= 0)
  if (plot) {
    plot(x, y, type = "p", pch = psty)
    rect(a, bot, b, top, col = rgb(0,0,0,0.04))
    curve(f, a, b, add = TRUE, col = "red", ...)
    if(plotHits) points(x[hit], y[hit], pch = phsty, col = "darkred")
  }
  return(list(estimate = rect_area * mean(hit), hits = sum(hit)))
}
set.seed(2)


fun = function(x) {
  -1 * (sin(2 * (4*x - 2)) + 2 * exp(-16^2 * (x - 0.5)^2)) + 2.5
}
curve(fun, 0, 1, col = "red", ylab = "f(x)")


m = monte_carlo(fun, 0, 1, n = 500L, plot = TRUE, plotHits = FALSE)
set.seed(2)

m = monte_carlo(fun, 0, 1, n = 500L, plot = TRUE)