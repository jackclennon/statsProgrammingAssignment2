boxes <- sample(1:50)

checkLoop <- function(n, boxes) {
  for (card in boxes) {
    terminate <- card
    loop <- 0
    while (card != terminate || loop == 0) {
      card <- boxes[card]
      loop <- loop + 1
    }
    if (loop == n) {
      return(1)
    }
  }
  return(0)
}

for (i in 1:50) {
  if (checkLoop(i, boxes) == 1) {
    print(c("Loop of length ", i))
  }
}
