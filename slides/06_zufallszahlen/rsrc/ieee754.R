as.float <- function(x, n = 23, unsigned = FALSE)
{
  g1 <- abs(x) < 1
  num <- x
  sgn <- sign(x)
  x <- abs(x)

  str <- ''
  ip <- trunc(x)
  rest <- x - trunc(x)
   
  s <- 2^(n:1 - 1)
  indx <- (ip >= s)

  if(length(s2 <- s[indx])) {
    for(i in 1:length(s2)) {    
      if(ip < s2[i]) str[i] <- 0
        else {
          str[i] <- 1
          ip <- ip - s2[i]
        }
    }
  } else str <- s2 <- 0
    
  str[length(s2) + 1] <- '.'
  
  for(i in (length(s2) + 2):(n + 1))  {
    rest <- 2 * rest;
    if(rest < 1) {
      str[i] <- 0;
    } else {
      str[i] <- '1';
      rest <- rest - 1;
    }
  }

  if(!unsigned)
    str <- c(if(sgn > 0) "+" else "-", str)
  str <- paste(str, collapse = "")
  class(str) <- "float"
  attr(str, "num") <- num
  attr(str, "|x|<1") <- g1

  return(str)  
}

as.bin <- function(x, ...) {
  x <- as.integer(x)
  x <- as.float(x, ...)
  x <- strsplit(x, ".", fixed = TRUE)[[1]][1]
  class(x) <- "bin"
  x
}

as.dec <- function(x, ...)
{
  UseMethod("as.dec")
}

as.dec.default <- function(x, ...)
{
  as.dec.float(x, ...)
}

as.dec.bin <- function(x, ...)
{
  x <- strsplit(x, "")[[1]]
  if(!(x[1] %in% c("+", "-")))
    x <- c("+", x)
  sgn <- if(x[1] != "+") -1 else 1
  x <- as.integer(x[-1])
  return(sgn * sum(x * 2^((length(x) - 1):0)))
}

as.dec.float <- function(x, ...)
{
  if(!inherits(x, "float")) {
    if(!inherits(x, "character")) {
      xnum <- x
      x <- as.float(x, ...)
    } else xnum <- NA
  } else {
    xnum <- as.numeric(attr(x, "num"))
  }

  x <- strsplit(x, ".", fixed = TRUE)[[1]]
  x1 <- strsplit(x[1], "")[[1]]
  x2 <- strsplit(x[2], "")[[1]]
  sgn <- x1[1]
  x1 <- x1[-1]
  x1 <- sum(as.integer(x1) * 2^((length(x1) - 1):0))
  x2 <- sum(as.integer(x2) * 1 / 2^(1:(length(x2))))
  x <- x1 + x2
  x <- x * if(sgn != "+") -1 else 1
  attr(x, "err") <- abs((xnum - x) / xnum)

  return(x)
}

as.ieee754 <- function(x, m = 8, e = 8, round = TRUE)
{
  g1 <- abs(x) < 1
  num <- x
  x <- as.float(x, n = 100)

  g1 <- attr(x, "|x|<1")
  x <- strsplit(x, ".", fixed = TRUE)[[1]]
  x1 <- strsplit(x[1], "")[[1]]
  sgn <- x1[1]
  x1 <- x1[-1]
  B <- 2^(e - 1) - 1

  if(!g1) {
    k <- nchar(x[1]) - 2
    x[1] <- paste(x1, collapse = "")
    x <- strsplit(paste(x, collapse = ""), "")[[1]]
    x <- rep(x, length.out = m + 1)
    M <- paste(x[-1], collapse = "")
    E <- as.bin(k + B, unsigned = TRUE)
    E <- paste(rep(strsplit(E, "")[[1]], length.out = e), collapse = "")
  } else {
    x2 <- strsplit(x[2], "")[[1]]
    k <- -1 * (k0 <- min(which(x2 == "1")))
    if(k + B < 0)
      E <- paste(rep(as.bin(0, unsigned = TRUE), length.out = e), collapse = "")
    else
      E <- as.bin(k + B, unsigned = TRUE)
    E <- strsplit(E, "")[[1]]
    E <- c(rep(0, e - length(E)), E)
    E <- paste(E, collapse = "")
    x2 <- x2[k0:length(x2)][-1]
    M <- paste(rep(x2, length.out = m), collapse = "")
  }

  x <- c("sign" = sgn, "exponent" = E, "mantissa" = M)
  class(x) <- "ieee754"
  attr(x, "num") <- num
  attr(x, "|x|<1") <- g1

  return(x)
}

as.dec.ieee754 <- function(x)
{
  e <- x["exponent"]
  num <- attr(x, "num")
  k <- nchar(e)
  e <- as.dec.bin(e) - (2^(k - 1) - 1) + 1
  m <- c(1, strsplit(x["mantissa"], "")[[1]])
  if(e < 1) {
    x <- c(x["sign"], 0, ".", rep(0, length = abs(e)), m)
  } else {
    if(e > (length(m) - 1)) {
      x <- c(x["sign"], m, ".", rep(0, 4))
    } else {
      x <- c(x["sign"], m[1:e], ".", m[(e + 1):length(m)])
    }
  }
  x <- paste(x, collapse = "")
  class(x) <- "float"
  attr(x, "num") <- num
  as.dec.float(x)
}

