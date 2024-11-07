f = function(x) {
  1/0.03246362 * exp(-4-x^2)
}

h = function(x) log(f(x))

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
  plot(x,h(x),type="l",ylim=c(min(h(x)),max(h(x))+0.4), ylab = "log(Denstity)",
    axes = FALSE)
  axis(1)
  axis(2)

  for (i in 1:(length(S)-1)) {
    if (prod(is.finite(L[,i]))) { abline(L[2,i],L[1,i],col="blue")}
  }
  for (i in 1:(length(S))) {
    lines(c(S[i],S[i]),c(hmin,H[i]),lty=2)
    text(S[i],hmin-0.01,substitute(x[i],list(i=i-1)))
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
  x  = seq(-2,2,length=1000)
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
plot.ars()
plot.sandwich(log = TRUE)

