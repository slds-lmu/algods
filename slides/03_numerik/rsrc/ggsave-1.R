
library(knitr)
library(ggplot2)


#well-conditioned and ill-conditioned

x = seq(0, 2, 0.01)
y = 2 - 1 * x
y2 = 2.2 - 1.2 * x
y3 = 1 + 0 * x

par(mfrow=c(1,2))
pl = plot(x, y, type = "l", lwd = 2L, col = rgb(0,1,0,0.9), main = "Ill-conditioned")
#lines(x+0.15, y1, lwd = 2, col = rgb(0,1,0,0.5), lty = 3L)
#lines(x-0.15, y1, lwd = 2, col = rgb(0,1,0,0.5), lty = 3L)
lines(x, y2, lwd = 2, col = rgb(0,0,1,0.9))
lines(x+0.15, y2, lwd = 2, col = rgb(0,0,1,0.9), lty = 2L)
#lines(x-0.15, y2, lwd = 2, col = rgb(0,0,1,0.5), lty = 3L)


plot(x, y, type = "l", lwd = 2L, col = rgb(0,1,0,0.9), main = "Well-conditioned")
#lines(x+0.15, y1, lwd = 2, col = rgb(0,1,0,0.5), lty = 3L)
#lines(x-0.15, y1, lwd = 2, col = rgb(0,1,0,0.5), lty = 3L)
lines(x, y3, lwd = 2, col = rgb(0,0,1,0.9))
lines(x, y3+0.1, lwd = 2, col = rgb(0,0,1,0.9), lty = 2L)



