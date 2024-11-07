



library(knitr)
library(ggplot2)

f = function(x, k) {
  (2 * k + 1) * base::pi / 2 * sin((2 * k + 1) * base::pi * x)
}

pl = ggplot(data.frame(x = c(0, 1)), aes(x = x)) + theme_bw()
pl = pl + geom_path(aes(colour = "black"), stat = "function", fun = function(x) f(x, 1))
pl = pl + geom_path(aes(colour = "blue"), stat = "function", fun = function(x) f(x, 2))
pl = pl + geom_path(aes(colour = "green"), stat = "function", fun = function(x) f(x, 4))
pl = pl + geom_path(aes(colour = "red"), stat = "function", fun = function(x) f(x, 8))
pl = pl + scale_colour_identity(" ", guide = "legend",  labels = c("k = 1", "k = 2", "k = 4", "k = 8"),  breaks = c("black", "blue", "green", "red"))

pl

ggsave("../05-quadratur/figure_man/Example1.png", pl)





pinterpol = function(x, f, degree = 2) {
  a = min(x, na.rm = TRUE)
  b = max(x, na.rm = TRUE)
  xp = seq(a, b, length = degree + 1)
  y = f(xp)
  A = outer(xp, 0:degree, "^")
  beta = solve(A) %*% y
  y = outer(x, 0:degree, "^") %*% beta
  y
}

f = function(x) {  1 / (1 + x^2)}
x = seq(-5, 5, length = 200)
y4 = pinterpol(x, f, 4)
y10 = pinterpol(x, f, 10)
plot(f(x) ~ x, type = "l", ylim = range(f(x), y4, y10), lwd = 2)
lines(y4 ~ x, col = "blue")
lines(y10 ~ x, col = "red")
xp = seq(min(x), max(x), length = 4 + 1)
points(xp, f(xp), pch = 16, col = "blue")
xp = seq(min(x), max(x), length = 10 + 1)
points(xp, f(xp), pch = 16, col = "red")
legend("top", expression(f(x), p[4](x), p[10](x)), lwd = 1,
       col = c("black", "blue", "red"), box.col = NA, bg = NA)





newcot = function(f, a, b, n = 10, m = 3, open = FALSE, plot = FALSE, ...) {
  if(m > 10) stop("m > 10 is not allowed!")
  w = switch(m,
             "1" = c(1, 1) / 2,
             "2" = c(1, 4, 1) / 6,
             "3" = c(1, 3, 3, 1) / 8,
             "4" = c(7, 32, 12, 32, 7) / 90,
             "5" = c(19, 75, 50, 50, 75, 19) / 288,
             "6" = c(41, 216, 27, 272, 27, 216, 41) / 840,
             "7" = c(751, 3577, 1323, 2989, 2989, 1323,
                     3577, 751) / 17280,
             "8" = c(989, 5888, -928, 10496, -4540, 10496,
                     -928, 5888, 989) / 28350,
             "9" = c(2857, 15741, 1080, 19344, 5778, 5778,
                     19344, 1080, 15741, 2857) / 89600,
             "10" = c(16067, 106300, -48525, 272400, -260550,
                      427368, -260550, 272400, -48525, 106300, 16067) / 598752
  )
  
  xp = seq(a, b, length = n + 1)
  
  foo = function(w) {
    n = length(w) + open
    
    function(f, a, b) {
      pos = function(i) a + i * (b - a) / n
      xp = pos(seq.int(0, length(w) - 1))
      (b - a) / sum(w) * sum(f(xp) * w)
    }
  }
  
  bar = foo(w)
  
  area = 0
  for(i in seq_len(n)) {
    area = area + bar(f, xp[i], xp[i + 1])
  }
  
  if(plot) {
    y = NULL
    for(i in seq_len(n)) {
      x = seq(xp[i], xp[i + 1], length = 100)
      y = cbind(y, pinterpol(x, f, degree = m))
    }
    curve(f, a, b,
          ylim = range(f(seq(a, b, length = 100)), y), ...)
    
    for(i in seq_len(n)) {
      x = seq(xp[i], xp[i + 1], length = 100)
      p = cbind(c(x, rev(x)), c(y[, i], rep(0, length(x))))
      polygon(p, col = "lightgray")
    }
    curve(f, a, b, add = TRUE, col = "red", ...)
    box()
  } else area
}


fun = function(x) {
  -1 * (sin(2 * (4*x - 2)) + 2 * exp(-16^2 * (x - 0.5)^2)) + 2.5
}
curve(fun, 0, 1, col = "red", ylab = "f(x)")


newcot(fun, 0, 1, n = 4, m = 1, plot = TRUE)


newcot(fun, 0, 1, n = 4, m = 2, plot = TRUE)


newcot(fun, 0, 1, n = 4, m = 3, plot = TRUE)


newcot(fun, 0, 1, n = 4, m = 7, plot = TRUE)




