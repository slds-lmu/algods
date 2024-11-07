library(knitr)

#Distance

e = -1:1
x = list()
k = 1
for(j in 3:4) {
  x[[k]] = NA
  u = expand.grid(rep(list(0:1), length = j))
  for(i in 1:length(e)) {
    for(ii in 1:nrow(u)) {
      tx = 2^e[i] * sum(u[ii, ] * 2^(1:j * -1))
      x[[k]] = c(x[[k]], tx, -1 * tx)
    }
  }
  x[[k]] = x[[k]][x[[k]] != 0]
  k = k + 1
}

plot(x[[1]], rep(1, length(x[[1]])), ylim = c(0.5, 2.5), xlim = c(-2.5, 2.5),
     axes = FALSE, xlab = "x", ylab = "", pch = 4)
axis(2, at = c(1, 2), labels = c("m = 3", "m = 4"))
points(x[[2]], rep(2, length(x[[2]])), pch = 4)
axis(1)
box()