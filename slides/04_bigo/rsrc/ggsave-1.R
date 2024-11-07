

library(knitr)
library(microbenchmark)
library(reshape2)
library(ggplot2)

f = function(x) 3 * x^3 + x^2 + 200 * sin(x)

p = ggplot(data = data.frame(x = 0)) + theme_bw()
p = p + stat_function(fun = f, size = 1) + xlim(0, 8) + xlab("x") + ylab("y")



ggsave("../04-bigo/figure_man/Example1.png", p)





g = function(x, M) M * x^3

x = seq(0, 10, by = 0.01)
d = data.frame(x = x)

for (i in seq(1, 4, by = 1)) {
  d = cbind(d, g(x, i))
  names(d)[i + 1] = i
}

d = melt(d, id = c("x"))
levels(d$variable) = c("M = 1", "M = 2", "M = 3", "M = 4")

p = p + geom_line(data = d[d$variable == "M = 1", ], aes(x = x, y = value, group = variable, colour = variable))
p = p + labs(colour = "M * x^3")
p

ggsave("../04-bigo/figure_man/Example1b.png", p)





p = p + geom_line(data = d, aes(x = x, y = value, group = variable, colour = variable))
p = p + labs(colour = "Mx^3") + ylim(c(0, 2000))
p

ggsave("../04-bigo/figure_man/Example1c.png", p)







const = function(x) {
  rep(1, length(x))
}
lin = function(x) {
  x
}
quad = function(x) {
  x^2
}

kub = function(x) {
  x^3
}

expo = function(x) {
  2^x
}

x = seq(1, 15, 0.05)
foos = c("const", "lin", "quad", "kub", "expo")
res.df = lapply(foos, function(foo) {
  y = do.call(foo, list(x))
  fun.name = rep(foo, length(x))
  data.frame(complexity = y, fun = fun.name, n = x)
})
res.df = do.call("rbind", res.df)
levels(res.df$fun) = c("constant", "linear", "quadratic", "cubic", "exponential")
q = ggplot(res.df, aes(n, complexity, color = fun)) + geom_line()
q = q + scale_y_continuous(limits = c(0, 1500))
q = q + scale_color_discrete(guide = guide_legend(title = "function")) + theme_bw()
q


ggsave("../04-bigo/figure_man/classes.png", q)


