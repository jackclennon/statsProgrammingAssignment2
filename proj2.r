#,,Yeshwanth Zagabathuni s2319494
#https://github.com/jackclennon/statsProgrammingAssignment2/ 
#Contributions:  
start.time <- Sys.time()      
#The Prisioners problem is carried out as follows:

#The p() function does the simulation of the events based on the strategy of choosing
#We may loop over it 'nreps' number if times
p <- function(n, k, strategy, shuffledboxes) {  
#Strategies 1 and 2 are quite similar compared to Strategy 3 and hence, we deal them in this block
#Strategy 3 is dealt at the else block
  if (strategy < 3) {  
    if (strategy==1) {        #As Strategy 1 says, we start with the Box with the Prisoner's number on it
      box_to_open <- shuffledboxes[k] #Thus, shuffledboxes[k]
    } else if (strategy==2) { #As Strategy 2 says, we need to start with a random numbered box 
      box_to_open <- shuffledboxes[sample(1:(2*n), 1)] #Thus we use sample() to generate a random Prison number
    }
    attempts <- 0             #attempts is initialized to 0 and will be incremented on each term
    
    #Now we simulate the Prisoner checking boxes for atmost 'n' attempts using the while() loop  
    #The loop terminates if box_to_open= k or in simple terms when the the box contains the card with the Prioner's number(k) 
    while (box_to_open != k) {
      if (attempts == n) {    #If attempts=n, the Prisoner ran out of attempts 
        return(0)             #Thus we return 0 
      }
      box_to_open <- shuffledboxes[box_to_open] #
      attempts <- attempts + 1                  #'attempts' is incremented by 1 and is an indication of "1 turn completed"
    }
    return(1)
  } else {           #Strategy 3 is to open 'n' random boxes and check them one by one
    choices <- shuffledboxes[sample(1:(2*n))][1:n]  #Randomly picking 'n' boxes is simulated using sample()
    for (box_to_open in choices) {                  #Every box is checked by the prisoner and this is simulated using for() loop
      if (box_to_open == k) {                       #If he Prisoner found his number in any of those boxes, it is a success
        return(1)                                   #And thus, 1 is returned
      }
    }
  }
  0      #If we reached this line, then that means that the Prisoner couldn't find his number in 'n' attempts
         #Thus, we return 0
}
pone <- function(n, k, strategy, nreps=10000) { 
#The pone() function estimates the probability of each prisioner finding his number.
#The parameters are: 
# 'n' - the maximum number of attempts a Prisoner gets to find his number                                                                   
# 'k' - The Prisoner's Number                                                                
#'strategy' - which is either 1 or 2 or 3                                                                 
#'nreps' - The number of simulations to run to estimate Probability (ideally 10000)
#And also note that '2n' refers to the number of Prisons 
  
  N <- 2*n          #We assign the value 2*n to the variable 'N' for easier understandability as we code further
  prisoners <- 1:N  #The 'prisioners' vector is assigned with the Prison numbers from 1 to 'N'
  nSuccesses <- 0   #The 'nSuccesses' variable counts the number of successes in a simulation that is done 'nreps' number of times.
                    #This variable is initialized to 0
#Now we run our simulation 'nreps' number of times using the p() function
#Based on the outcome of each simulation, either 1 or 0 will be added to nSuccesses
#If 1 is returned by p(), that means the Prisoner found his number inside 'n' attempts
#If 0 is returned by p(), that means the Prisoner did not find his number inside 'n' attempts
  for (rep in 1:nreps) {  
    shuffledboxes <- sample(prisoners)  #The numbers in the 'prisoners' vector are re-arranged in random order and assigned to 'shuffledboxes' 
    nSuccesses <- nSuccesses+p(n, k, strategy, shuffledboxes) #Now the p() function does the simulation and returns 0 or 1 as discussed above 
  }
  nSuccesses/nreps  #After 'nreps' number of simulations, we now know how many times the Prisoner 'k' suceeded 
                    #Hence the Probability is computed as: Number of Successes/ Total Number of Simulations 
}


pall <- function(n, strategy, nreps=10000) {
#The pall() function estimates the probability of all prisoners going free
#It takes 3 parameters:
# 'n' - the maximum number of attempts a Prisoner gets to find his number  
#'strategy' - which is either 1 or 2 or 3
#'nreps' - The number of simulations to run to estimate Probability (ideally 10000)

  N <- 2*n            #We assign the value 2*n to the variable 'N' for easier understandability 
  prisoners <- c(1:N) #The 'prisioners' vector is assigned with the Prison numbers from 1 to 'N'
  nSuccesses <- 0     #The 'nSuccesses' variable counts the number of successes in a simulation that is done 'nreps' number of times.
                      #This variable is initialized to 0
  
  for (rep in 1:nreps) {
    shuffle<-sample(1:N)  #The numbers in the 'prisoners' vector are re-arranged in random order and assigned to 'shuffled' 
    tally<-lapply(prisoners,function(x) p(n,x,strategy,shuffle))
    if (sum(unlist(tally))==N){
      nSuccesses<-nSuccesses+1  #If all Prisoners suceed in finding their number, it is a success
                                #Thus 'nSuccesses' is incremented by 1
    }
  }
  nSuccesses/nreps  #After 'nreps' number of simulations, we now know how many times all Prioners succeeded 
                    #Hence the Probability is computed as: Number of Successes/ Total Number of Simulations 
}

#Our next step would be call the pone() function on all 3 strategies and for given values of 'n'
#n=5, k=1 and strategy = i (i=1,2,3)
#Next we do the same with n=50
#n=50, k=1 and strategy = i (i=1,2,3)
#The 6 results obtained are better illustrated in a matrix where each row represents results of ith strategy 
#The first column is the set of results for n=5 while the second is for n=50                 
PoneExample<-matrix(c(pone(5, 1, 1), pone(5, 1, 2), pone(5, 1, 3), pone(50, 1, 1), pone(50, 1, 2), pone(50, 1, 3)), 3, 2)
colnames(PoneExample)<-c("n=5","n=50")
rownames(PoneExample)<-c("Strategy1","Strategy2","Strategy3")

#Our next step would be call the pall() function on all 3 strategies and for given values of 'n'
#n=5, k=1 and strategy = i (i=1,2,3)
#Next we do the same with n=50
#n=50, k=1 and strategy = i (i=1,2,3)
#The 6 results obtained are illustrated in the matrix just like how we did for pone() 
#Once again, each row represents results of ith strategy 
#The first column is the set of results for n=5 while the second is for n=50         
PallExample<-matrix(c(pall(5, 1), pall(5, 2), pall(5, 3), pall(50, 1), pall(50, 2), pall(50, 3)), 3, 2)
colnames(PallExample)<-c("n=5","n=50")
rownames(PallExample)<-c("Strategy1","Strategy2","Strategy3")
print(PoneExample)     #This line prints the PoneExample matrix
print(PallExample)     #This line prints the PallExample matrix

dloop <- function(n, nreps=10000) { #Now we write the dloop() function to estimate the probability of each loop from 1 to 2n
                                    #occuring atleast once in a random shuffling of cards to boxes
  N <- 2*n               
  prisoners <- c(1:N)
  loops <- rep(0, N)
  for (rep in 1:nreps) {
    shuffle<-sample(1:N)
    tally<-lapply(prisoners,function(x) d(n,x,shuffle))
    tally <- unique(tally)
    for (t in tally) {
      loops[t] <- loops[t] + 1
    }
  }
  loops <- loops/nreps  
  #Here we apply the law of probability p(c)=1-p(c')
  #c = "probability of each loop from 1 to 2n occuring atleast once in a random shuffling of cards to boxes"
  #c' = "probability of each loop from 1 to 2n not occuring in a random shuffling of cards to boxes"
  #Thus we print, '1-sum(loops[n:N])'
  print(1-sum(loops[n:N]))
  plot(1:100, loops, ylim=c(0,0.1),type='s')
}

d <- function(n, k, shuffledboxes) {
  box_to_open <- shuffledboxes[k]
  attempts <- 0
  while (box_to_open != k) {
    box_to_open <- shuffledboxes[box_to_open]
    attempts <- attempts + 1
  }
  return(attempts)
}

dloop(50)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
