library(ggplot2)
library(reshape)
library(microbenchmark)

isElement = function(xs, el) {
  for (x in xs) {
    if (identical(x, el))
      return(TRUE)
  }
  return(FALSE)
}

runtime = data.frame(n = 1000 * (1:20))

# worst case runtime
runtime$worst = sapply(runtime$n, function(x) summary(microbenchmark(isElement(1:x, x), unit = "us"))$mean)

# average case runtime
runtime$average = sapply(runtime$n, function(x) summary(microbenchmark(isElement(1:x, sample(x, 1)), unit = "us"))$mean)

# best case runtime
runtime$best = sapply(runtime$n, function(x) summary(microbenchmark(isElement(1:x, 1L), unit = "us"))$mean)

melted = melt(runtime, id.vars = "n")
names(melted) = c("n", "case", "runtime")
melted$case = as.factor(melted$case)
levels(melted$case) = c("Worst Case", "Average Case", "Best Case")

p = ggplot(melted, aes(x = n, y = runtime, group = case, colour = case)) + geom_line() + theme_bw()
p = p + labs(colour = "") + xlab("n") + ylab("Runtime (in microseconds)")
ggsave("figure_man/runtime.png", p, device = "png", width = 5, height = 3)

