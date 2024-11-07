f = function(x) {
  1/0.03246362 * exp(-4-x^2)
}

h = function(x) log(f(x))
  
hprime = function(x) -2*x

createARSState = function(a, b, x, hfun, hprimefun) {
  # x = c(a, x, b)
  n = length(x)
  h = hfun(x)
  hp = hprimefun(x)
  z = numeric(n)
  u = numeric(n)

  for (i in 1:n) {
    z[i] = x[i] + (h[i] - h[i+1] + hp[i+1]*(x[i+1] - x[i])) / (hp[i+1] - hp[i])
    u[i] = hp[i] * (z[i] - x[i]) + h[i]
  }
  z = c(a, z, b)
  u = c(h[1], u, h[n])


  list(hfun = hfun, hprimefun = hprimefun, a = a, b = b, 
    x = x, h = h, z = z, u = u)
}

createARSStateSequence = function(n, a, b, x, hfun, hprimefun) {
  new.x = runif(n - 1L, a, b)
  res = list(length = n)
  res[[1L]] = createARSState(a, b, x, hfun, hprimefun)
  for (i in seq_len(n - 1L)) {
    x = sort(c(x, new.x[i]))
    res[[i + 1L]] = createARSState(a, b, x, hfun, hprimefun)
    res[[i + 1L]]$new.x = new.x[[i]]
  }
  return(res)
}


createEnvelopeUpper = function(ars) {
  function(x) {
    one = function(x) {
      idx = which.min(x > z) - 1
      return(coeffE[1,idx]*x + coeffE[2,idx])
    }
    sapply(x, one)
  }
}

createEnvelopeLower = function(ars) {
  function(x) {
    n = length(S)
    one = function(x) {
      idx = which.min(x > S) - 1
      if (idx == 1 | idx == (n-1)) {
        return(-Inf)
      } else {
        return(L[2,idx] + L[1,idx]*x)
      }
    }
    sapply(x, one)
  }
}



plotARSLog = function(ars) {
  xseq = seq(ars$a, ars$b, length = 1000)
  yseq = ars$hfun(xseq)
  hmin = min(yseq)
  hmax = max(yseq)
  ylim = c(hmin, hmax + 0.4)
  plot(xseq, yseq, type = "l", ylim = ylim, ylab = "log(Denstity)")
  n = length(ars$x)
  lines(ars$x, ars$h, col = "blue")
  lines(ars$z, ars$u, col = "red")
  for (i in 1:n) {
    abline(v = ars$x[i], lty = "dotted")
  }
}

makeARSAnimation = function(ars.seq) {
  num.plots = length(ars.seq)
  grid.size = ceiling(num.plots / 2L)
  par(mfrow = c(2L, grid.size))
  for (i in seq_len(num.plots)) {
    if (i == 1L) {
      p.title = "state at start"
    } else {
      p.title = sprintf("Iteration: %d", i - 1L)
    }
    plotARSLog(ars.seq[[i]])
    title(p.title)
    if ( i != 1L) {
      abline(v = ars.seq[[i]]$new.x, col = "darkgreen", lwd = 2)
    }
    # Sys.sleep(2)
  }
}

x = c(-1.3, -0.1, 1.8)
ars = createARSState(x = x, a = -2, b = 2, h, hprime)

set.seed(123L)
ars.seq = createARSStateSequence(6L, x = x, a = -2, b = 2, h, hprime)
makeARSAnimation(ars.seq)
