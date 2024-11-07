library(knitr)
library(ecr)
library(ggplot2)
library(smoof)
library(latex2exp)
library(ggforce)
library(reshape2)
library(mlr)

#Example: Simple computation

#<<echo=FALSE, fig.keep='last', fig.height=4>>=
  x = seq(0, 10, length = 100)
  f = function(x) { 2 + cos(x) + sin(2 * x) }
  plot(f(x) ~ x, type = "l")
  text(1.5, 3.5, expression("f(x) = 2 + cos(x) + sin(2x)"), pos = 4)
  x2 = seq(0, 10, length = 10)
  for(j in seq_along(x2)) {
    lines(c(0, f(x2[j])) ~ rep(x2[j], 2), lwd = 2, col = "lightgray")
  }
  lines(f(x) ~ x)
  points(x2, f(x2), pch = 16)
  box()
  
  
#Example: Equation solving / Root finding
  
  #<<echo = FALSE, fig.keep='last'>>=
    plot(f(x) ~ x, type = "l")
  text(1.5, 3.5, expression("f(x) = 2 + cos(x) + sin(2x) = 2.5"), pos = 4)
  y = 2.5
  abline(h = 2.5, lwd = 2, col = "lightgray")
  lines(f(x) ~ x)
  f0 = function(x) { f(x) - 2.5 }
  xr = uniroot(f0, c(0, 5))
  lines(c(y, 0) ~ rep(xr$root, 2), lwd = 2, col = "lightgray")
  points(xr$root, y, pch = 16)
  lines(f(x) ~ x)
  box()
  