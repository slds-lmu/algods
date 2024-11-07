library(mvtnorm)

nsamps = 6L
post.unnorm = function(x) dbinom(x = 4, size = nsamps, prob = x) * dunif(x, 0, 1)

laplace = function(post.unnorm, init, nsamps, ...) { 
  g = function(x) log(post.unnorm(x)) / nsamps
  ctrl = list(fnscale = -1)
  or = optim(init, g, hessian = TRUE, control = ctrl, method = "Brent", 
    lower = 0, upper = 1)
  theta.hat = or$par
  gmax = or$value
  hess = or$hessian
  fx = exp(nsamps * gmax) * sqrt((2 * pi) / (- nsamps * hess))
  post.norm = function(x) post.unnorm(x) / fx
  post.approx = function(x) dnorm(x, mean = theta.hat, sd = sqrt(1 / (- nsamps * hess))) 
  list(theta.hat = theta.hat, hess = hess, post.norm = post.norm, post.approx = post.approx)
}



z = laplace(post.unnorm, init = 0.5, nsamps = nsamps)
curve(z$post.norm(x), 0, 1)
curve(z$post.approx(x), 0, 1, add = TRUE, lty = "dotted")


