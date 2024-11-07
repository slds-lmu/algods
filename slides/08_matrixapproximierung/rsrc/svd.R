library(gridExtra)
library(grid)
library(latex2exp)

# Setting up the matrices
A = matrix(c(1, - 3 / 2, 0.5, 1), ncol = 2)
res = svd(A)
V = res$v
U = res$u
D = diag(res$d)

# sample points within a unit circe
n = 1000
x = sample(-1000:1000, n, replace = T) / 1000
y = sample(-1000:1000, n, replace = T) / 1000
ind = which(x^2 + y^2 <= 1) # points within the unit circle
points = data.frame("x" = x[ind], "y" = y[ind])

# Unit circle
p = ggplot(data = points, aes(x = x, y = y)) + geom_point(color = "grey") + theme_bw() 
p = p + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), color = "red", size = 1, arrow = arrow(length = unit(0.03, "npc")))
p = p + geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), color = "blue", size = 1, arrow = arrow(length = unit(0.03, "npc")))
p = p + xlim(c(-2, 2)) + ylim(c(-2, 2)) + ggtitle("Unit circle")
p

# Transformation with A
points.A = A %*% t(as.matrix(points))
points.A = data.frame("x" = points.A[1, ], "y" = points.A[2, ])
unit.A = A %*% matrix(c(1, 0, 0, 1), ncol = 2)

pa = ggplot(data = points.A, aes(x = x, y = y)) + geom_point(color = "grey") + theme_bw() 
pa = pa + geom_segment(aes(x = 0, y = 0, xend = unit.A[1, 1], yend = unit.A[2, 1]), color = "red", size = 1, arrow = arrow(length = unit(0.03, "npc")))
pa = pa + geom_segment(aes(x = 0, y = 0, xend = unit.A[1, 2], yend = unit.A[2, 2]), color = "blue", size = 1, arrow = arrow(length = unit(0.03, "npc")))
pa = pa + xlim(c(-2, 2)) + ylim(c(-2, 2)) + ggtitle(TeX('(1): multiplication with $A$')) 
pa

# Transformation with V
points.V = V %*% t(as.matrix(points))
points.V = data.frame("x" = points.V[1, ], "y" = points.V[2, ])
unit.V = V %*% matrix(c(1, 0, 0, 1), ncol = 2)

pv = ggplot(data = points.V, aes(x = x, y = y)) + geom_point(color = "grey") + theme_bw() 
pv = pv + geom_segment(aes(x = 0, y = 0, xend = unit.V[1, 1], yend = unit.V[2, 1]), color = "red", size = 1, arrow = arrow(length = unit(0.03, "npc")))
pv = pv + geom_segment(aes(x = 0, y = 0, xend = unit.V[1, 2], yend = unit.V[2, 2]), color = "blue", size = 1, arrow = arrow(length = unit(0.03, "npc")))
pv = pv + xlim(c(-2, 2)) + ylim(c(-2, 2))  + ggtitle(TeX('(1): multiplication with $V^T$'))
pv

# Transformation with D
points.D = D %*% V %*% t(as.matrix(points))
points.D = data.frame("x" = points.D[1, ], "y" = points.D[2, ])
unit.D = D %*% V %*% matrix(c(1, 0, 0, 1), ncol = 2)


pd = ggplot(data = points.D, aes(x = x, y = y)) + geom_point(color = "grey") + theme_bw() 
pd = pd + geom_segment(aes(x = 0, y = 0, xend = unit.D[1, 1], yend = unit.D[2, 1]), color = "red", size = 1, arrow = arrow(length = unit(0.03, "npc")))
pd = pd + geom_segment(aes(x = 0, y = 0, xend = unit.D[1, 2], yend = unit.D[2, 2]), color = "blue", size = 1, arrow = arrow(length = unit(0.03, "npc")))
pd = pd + xlim(c(-2, 2)) + ylim(c(-2, 2)) + ggtitle(TeX('(2): multiplication with $\\Sigma$'))
pd

# Transformation with U
points.U = U %*% D %*% V %*% t(as.matrix(points))
points.U = data.frame("x" = points.U[1, ], "y" = points.U[2, ])
unit.U = U %*% D %*% V %*% matrix(c(1, 0, 0, 1), ncol = 2)


pu = ggplot(data = points.U, aes(x = x, y = y)) + geom_point(color = "grey") + theme_bw() 
pu = pu + geom_segment(aes(x = 0, y = 0, xend = unit.U[1, 1], yend = unit.U[2, 1]), color = "red", size = 1, arrow = arrow(length = unit(0.03, "npc")))
pu = pu + geom_segment(aes(x = 0, y = 0, xend = unit.U[1, 2], yend = unit.U[2, 2]), color = "blue", size = 1, arrow = arrow(length = unit(0.03, "npc")))
pu = pu + xlim(c(-2, 2)) + ylim(c(-2, 2)) + ggtitle(TeX('(3): multiplication with $U$'))
pu


g1 = grid.arrange(p, pv, pd, pu, ncol = 4)

ggsave("figure_man/ignore/svd.png", g1, width = 12, height = 3)

g2 = grid.arrange(p, pa, ncol = 2)

ggsave("figure_man/ignore/matrixabbildung.png", g2, width = 8, height = 4)




