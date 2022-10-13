#initialisations

n <- 50

p <- function(n, k, strategy, b) {
  if (strategy < 3) {
    if (strategy==1) {
      c <- b[k]
    } else if (strategy==2) {
      c <- b[sample(1:(2*n), 1)]
    }
    attempts <- 0
    while (c != k) {
      if (attempts == n) {
        return(0)
      }
      c = b[c]
      attempts <- attempts + 1
    }
    return(1)
  } else {
    cs <- b[sample(1:(2*n))][1:n]
    for (c in cs) {
       if (c == k) {
         return(1)
       }
    }
  }
  0
}

pone <- function(n, k, strategy, nreps=1000) {
  N <- 2*n
  I <- 1:N
  a <- 0
  for (rep in 1:nreps) {
    b <- sample(I)
    a <- a+p(n, k, strategy, b)
  }
  a/nreps
}

pone(n, 1, 3)

pall <- function(n, strategy, nreps) {
  N <- 2*n
  I <- 1:N
  nSuccesses <- 0
  for (rep in 1:nreps) {
    b <- 1:N
    b <- sample(I)
    a <- 0
    for (k in I) {
      a <- a + p(n, k, strategy, b)
    }
    if (a == N) {
      nSuccesses <- nSuccesses + 1
    }
  }
  nSuccesses/nreps
}

pall(n, 3, 1000)
