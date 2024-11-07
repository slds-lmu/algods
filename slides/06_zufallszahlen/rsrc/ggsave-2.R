

library(knitr)
library(ggplot2)
library(microbenchmark)

x = seq(-10, 10, .1)
f_x = dnorm(x, 3, 2) + dnorm(x, -5, 1)
f_y = dnorm(x, 0, 4)

b = max(f_x / f_y)^-1

env = f_y/b
#which(x == 2) # 121

plot(x, env, type = "l", col = "gray", lty = 2, lwd = 2, ylab = "f", ylim = c(0,1.1), main = expression(paste("Example Rejection Sampling, ", beta, "=", 1)))
lines(x, f_x, col = "blue", lwd = 2)
lines(x, f_y, col = rgb(.1, .1, .1, .3), lwd = 2)
segments(2,0,2,f_x[121], col = "green", lwd = 2)
segments(2,f_x[121],2,env[121], col = "red", lwd = 2)
legend(5, 1.1, legend = c("f(x)", "f(y)", expression(paste("1/", beta, " f(y)")), "accept", "reject"),
       col = c("blue", "gray", "gray", "green", "red"),
       lwd = 2,
       lty = c(1,1,2,1,1),
       seg.len = 1.5
)
text(0, .5, labels = expression(paste("1 - ", alpha)), col = rgb(1,0,0,1))
text(0, .2, labels = expression(alpha), col = rgb(0,1,0,1))
arrows(.8, .49, 1.9, .47, col = rgb(1,0,0,.7), length = 0.1, lwd = 2)
arrows(.6, .18, 1.9, .11, col = rgb(0,1,0,.7), length = 0.1, lwd = 2)
p=plot





########

n = 200
x = seq(-4, 4, length = n)
dx = dnorm(x)
dg = dcauchy(x) / 0.657
p=plot(dx ~ x, type = "l", ylim = range(dx, dg),
     lwd = 1, ylab = "Density", axes = FALSE)
axis(1)
axis(2)
lines(dg ~ x, col = "red", lty = 2)
legend("topright", c("N(0, 1)", expression(Cauchy(0, 1) / beta)), lwd = 1, col = c("black", "red"),
       lty = c(1, 2), box.col = NA, bg = NA)



ggsave("../06-zufallszahlen/figure_man/example.png", p)




########

rsamp = function(n, dx = dnorm, dy = dcauchy,
                 qy = qcauchy, beta = 0.657) {
  x = rep(NA, length = n)
  i = 0
  while(i < n) {
    y = qy(runif(1))
    alpha = beta * dx(y) / dy(y)
    u = runif(1)
    if(u <= alpha) {
      i = i + 1
      x[i] = y
    }
  }
  return(x)
}

set.seed(111)
x = rsamp(10000)
hist(x, freq = FALSE, breaks = 50)
lines(dnorm(x)[order(x)] ~ x[order(x)], col = "blue")



########


set.seed(123)
dx = function(x) dgamma(x, shape = 2)
dy = function(x) dunif(x, 0, 15)
qy = function(x) qunif(x, 0, 15)
x = rsamp(10000, dx = dx, dy = dy, qy = qy, beta = 0.175)
range(x)
hist(x, freq = FALSE, breaks = 50,
     ylim = c(0, 0.4), xlim = c(0, 15))
lines(dx(x)[order(x)] ~ x[order(x)], col = "blue")
rect(0, 0, 15, (1 / 15) / 0.175, border = "red", lty = 2)



#########

f = function(x) {
  1 / 0.03246362 * exp(- 4 - x^2)
}

h = function(x) log(f(x))

dh = function(x) {- 2 * x * f(x) / f(x)}

x = seq(-2,2,length=1000)
hmin = min(h(x))

X = matrix( c( -1, h(-1), 0, h(0), 1, h(1)), nrow = 2)

x = seq(-2,2,length = 1000)
plot(x, h(x), type = "l", ylim=c(min(h(x)),max(h(x))+0.4), ylab = "log(Denstity)",
     axes = FALSE)
axis(1)
axis(2)

for (i in 1:(dim(X)[2])) {
  x = X[1, i]
  m = dh(x)
  y = X[2, i]
  abline(y - m * x, m, col = "blue")
  abline(v = x, lty = 2)
  text(x + 0.1 , -4 , paste("y", i, sep = ""))
}


##########

library("ars")
logf = function(x, a = 1.3, b = 2.7) {
  (a - 1) * log(x) + (b - 1) * log(1 - x)
}
dlogf = function(x, a = 1.3, b = 2.7){
  (a - 1) / x - (b - 1)/(1 - x)
}
x = ars(10000, logf, dlogf, x = c(0.3, 0.6),
        m = 2, lb = TRUE, xlb = 0, ub = TRUE, xub = 1)
hist(x, freq = FALSE, breaks = 50)
lines(dbeta(x, 1.3, 2.7)[order(x)] ~ x[order(x)], col = "blue")


#########

f = function(x) {
  1 / 0.03246362 * exp(- 4 - x^2)
}

h = function(x) log(f(x))

dh = function(x) {- 2 * x * f(x) / h(x)}

x = seq(-2,2,length=1000)
hmin = min(h(x))

int.intervall = function() {
  int.one = function(i) {
    exp(coeffE[2,i])/coeffE[1,i]*(exp(coeffE[1,i]*z[i+1])-exp(coeffE[1,i]*z[i]))
  }
  sapply(1:(length(z)-1),int.one)
}

setup.ars = function(S) {
  noOfPoints = length(S)
  H = h(S)
  L = matrix(1e99,nrow=2,ncol=length(S))
  
  for (i in 1:(length(S)-1)) {
    L[1,i] = (H[i+1]-H[i])/(S[i+1]-S[i])
    L[2,i] = H[i]-L[1,i]*S[i]
  }
  
  if (noOfPoints>5) {
    z = NULL
    z = S[1:3]
    for (i in 3:(noOfPoints-3)) {
      z = c(z,-(L[2,i+1]-L[2,i-1])/(L[1,i+1]-L[1,i-1]),S[i+1])
    }
    z = c(z,S[(noOfPoints-1):noOfPoints])
  } else {
    z = S
  }
  
  coeffE = matrix(NA,2,length(z))
  coeffE[,1] = L[,2]
  coeffE[,2] = L[,3]
  i = j = 3
  while (i < (noOfPoints-2)) {
    coeffE[,j] = L[,i-1]
    coeffE[,j+1] = L[,i+1]
    j = j+2
    i = i+1
  }
  coeffE[,j] = L[,i-1]
  coeffE[,j+1] = L[,i]
  
  S <<- S
  noOfPoints <<- length(S)
  H <<- H
  L <<- L
  z <<- z
  coeffE <<- coeffE
  
  weights = c(0,int.intervall())
  norm = 1/sum(weights)
  weights = weights/sum(weights)
  csumw   = cumsum(weights)
  
  
  csumw <<- csumw
  norm <<- norm
}

plot.ars = function() {
  x = seq(-2,2,length=1000)
  plot(x, h(x), type = "l", ylim=c(min(h(x)),max(h(x))+0.4), ylab = "log(Denstity)",
       axes = FALSE)
  axis(1)
  axis(2)
  
  for (i in 1:(length(S)-1)) {
    if (prod(is.finite(L[,i]))) { abline(L[2,i], L[1,i],col = "blue")}
  }
  for (i in 1:(length(S))) {
    lines(c(S[i],S[i]),c(hmin,H[i]),lty = 2)
    text(S[i],hmin-0.01,substitute(x[i],list(i = i-1)))
  }
}

squeeze = function(x) {
  n = length(S)
  one = function(x) {
    idx = which.min(x > S) - 1
    if (idx == 1 | idx == (n-1)) {
      return(-Inf)
    } else {
      return(L[2,idx] + L[1,idx]*x)
    }
  }
  sapply(x,one)
}

envelope = function(x) {
  one = function(x) {
    idx = which.min(x > z) - 1
    return(coeffE[1,idx]*x + coeffE[2,idx])
  }
  sapply(x,one)
}

plot.sandwich = function(log=T,...) {
  x  = seq(-2,2,length = 1000)
  val= cbind(h(x),squeeze(x),envelope(x))
  if (!log) val = exp(val)
  
  matplot(x,val,lwd=1,lty=1,col=c("black","blue","red"),
          type="l", ylab = "log(Denstity)", axes = FALSE, ...)
  axis(1)
  axis(2)
  legend("bottom",c("log(f(x))","squeeze","envelope"),
         col=c("black","blue","red"),lty=1,lwd=1,
         box.col = NA, bg = NA)
}

S = c(-Inf,-1.3,-0.1,0.5,1.8,Inf)
try(setup.ars(S), silent = TRUE)

par(mfrow = c(1, 2))
plot.sandwich(log = TRUE)