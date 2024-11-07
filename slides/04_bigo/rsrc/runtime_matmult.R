# Matrix multiplication runtime

library(ggplot2)
library(microbenchmark)

multiplyMatrices = function(n) {
  A = matrix(runif(n^2), n, n)
  B = matrix(runif(n^2), n, n)

  return(A %*% B)
}

runtime = data.frame(n = 100 * 1:10)

runtime$average = sapply(runtime$n, function(x) summary(microbenchmark(multiplyMatrices(x), unit = "us"))$mean)

p = ggplot(runtime, aes(x = n, y = average)) + geom_line() + theme_bw()
p = p + ylab(expression(mu~"seconds"))
ggsave(filename = "figure_man/runtime_matmult.pdf", plot = p, width = 3, height = 2)
