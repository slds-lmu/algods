###########################
# mit Polygon

x = c(0,2)
x_pol = c(0,2,2,0)
y1 = 2 - 1 * x
y2 = 2.2 - 1.2 * x
y3 = 1 + 0 * x
y1_pol = c(1.85, -0.15, .15, 2.15)
y2_pol = c(2.05, -.35, -0.05, 2.35)
y3_pol = c(0.9, 0.9, 1.1, 1.1)

par(mfrow=c(1,2))
#plot(x, y1, type = "l", lwd = 2L, col = rgb(0,1,0,0.8), xlab = "x", ylab = "y", xaxt = "n", yaxt = "n", main = "Schlecht konditioniert")
plot(x, y1, type = "l", lwd = 2L, col = rgb(0,1,0,0.8), xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "Schlecht konditioniert")
lines(x, y2, lwd = 2, col = rgb(0,0,1,0.8))
polygon(x_pol, y1_pol, col = rgb(0,1,0,0.15), border = NA)
polygon(x_pol, y2_pol, col = rgb(0,0,1,0.15), border = NA)

plot(x, y1, type = "l", lwd = 2L, col = rgb(0,1,0,1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "Gut konditioniert")
lines(x, y3, lwd = 2, col = rgb(0,0,1,1))
polygon(x_pol, y1_pol, col = rgb(0,1,0,0.15), border = NA)
polygon(x_pol, y3_pol, col = rgb(0,0,1,0.15), border = NA)


###################################################################
x = seq(0, 2, 0.01)
y1 = 2 - 1 * x
y2 = 2.2 - 1.2 * x

y3 = 1 + 0 * x

par(mfrow=c(1,2))
plot(x, y1, type = "l", lwd = 2L, col = rgb(0,1,0,0.9), xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "Schlecht konditioniert")
lines(x+0.15, y1, lwd = 2, col = rgb(0,1,0,0.5), lty = 3L)
lines(x-0.15, y1, lwd = 2, col = rgb(0,1,0,0.5), lty = 3L)
lines(x, y2, lwd = 2, col = rgb(0,0,1,0.9))
lines(x+0.15, y2, lwd = 2, col = rgb(0,0,1,0.5), lty = 3L)
lines(x-0.15, y2, lwd = 2, col = rgb(0,0,1,0.5), lty = 3L)

plot(x, y1, type = "l", lwd = 2L, col = rgb(0,1,0,1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "Gut konditioniert")
lines(x+0.15, y1, lwd = 2, col = rgb(0,1,0,0.5), lty = 3L)
lines(x-0.15, y1, lwd = 2, col = rgb(0,1,0,0.5), lty = 3L)
lines(x, y3, lwd = 2, col = rgb(0,0,1,1))
lines(x, y3+0.07, lwd = 2, col = rgb(0,0,1,0.5), lty = 3L)
lines(x, y3-0.07, lwd = 2, col = rgb(0,0,1,0.5), lty = 3L)



######################################
# Zahlenbeispiel aus den Folien:
# 0.835x + 0.667y = 0.168
# 0.333x + 0.266y = 0.067
#
# funktioniert nicht mit plotten
# Geraden zu ähnlich

b1 = -0.835 / 0.667
a1 = 0.168/0.667

x = seq(0.999995, 1.000005, .0000001)
y1 = a1 + b1 * x
y2 = a2 + b2 * x

b2 = -0.333/0.266
#b2 = -1.218
a2 = 0.067/0.266

plot(x, y1, type ="l", lwd = 2)
lines(x, y2, type ="l", lwd = 2, col = "red")


# 0 . 067 auf 0 . 066

b1_1 = -0.835 / 0.666
a1_1 = 0.168 / 0.666

x1_1 = seq(-1000, 1000, 1)
y1_1 = a1_1 + b1_1 * x1_1

plot(x1_1, y1_1, type ="l", lwd = 2)
lines(x2, y2, type ="l", lwd = 2, col = "red")

