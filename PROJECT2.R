start.time <- Sys.time()

p <- function(n, k, strategy, shuffledboxes) {
  if (strategy < 3) {
    if (strategy==1) {
      box_to_open <- shuffledboxes[k]
    } else if (strategy==2) {
      box_to_open <- shuffledboxes[sample(1:(2*n), 1)]
    }
    attempts <- 0
    while (box_to_open != k) {
      if (attempts == n) {
        return(0)
      }
      box_to_open <- shuffledboxes[box_to_open]
      attempts <- attempts + 1
    }
    return(1)
  } else {
    choices <- shuffledboxes[sample(1:(2*n))][1:n]
    for (box_to_open in choices) {
      if (box_to_open == k) {
        return(1)
      }
    }
  }
  0
}
pone <- function(n, k, strategy, nreps=10000) {
  N <- 2*n
  prisoners <- 1:N
  nSuccesses <- 0
  for (rep in 1:nreps) {
    shuffledboxes <- sample(prisoners)
    nSuccesses <- nSuccesses+p(n, k, strategy, shuffledboxes)
  }
  nSuccesses/nreps
}
pall <- function(n, strategy, nreps=10000) {
  N <- 2*n
  prisoners <- c(1:N)
  nSuccesses <- 0
  for (rep in 1:nreps) {
    shuffle<-sample(1:N)
    tally<-lapply(prisoners,function(x) p(n,x,strategy,shuffle))
    if (sum(unlist(tally))==N){nSuccesses<-nSuccesses+1}
  }
  nSuccesses/nreps
}

PoneExample<-matrix(c(pone(5, 1, 1), pone(5, 1, 2), pone(5, 1, 3), pone(50, 1, 1), pone(50, 1, 2), pone(50, 1, 3)), 3, 2)
colnames(PoneExample)<-c("n=5","n=50")
rownames(PoneExample)<-c("Strategy1","Strategy2","Strategy3")

PallExample<-matrix(c(pall(5, 1), pall(5, 2), pall(5, 3), pall(50, 1), pall(50, 2), pall(50, 3)), 3, 2)
colnames(PallExample)<-c("n=5","n=50")
rownames(PallExample)<-c("Strategy1","Strategy2","Strategy3")
print(PoneExample)
print(PallExample)


end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
















