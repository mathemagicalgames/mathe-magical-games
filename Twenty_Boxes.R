# Copyright (c) 2017 - 2018  
# Nicolai Meinshausen [meinshausen@stat.math.ethz.ch]
# Jonas Peters [jonas.peters@math.ku.dk]
# All rights reserved.  See the file COPYING for license terms. 

# This code simulates the permutation game. It is meant for presentation purposes and is not optimized for speed. 
# For example, one could  

# number of repetitions for the experiment
repetitions <- 1000
success <- rep(TRUE, repetitions)

# number of boxes in total
n <- 150

# number of boxes each player is allowed to look into
m <- 75

# loop over all repetitions
for(exp in 1:repetitions){
  numbers <- sample(n, size = n, replace = FALSE) 
  numberfound <- rep(FALSE, n) 
  # loop over players
  for(p in 1:n){
    checknext <- p
    # loop over trials
    for(trials in 1:m){
      checknext <- numbers[checknext]
      numberfound[p] <- numberfound[p] || (checknext == p)
      if(numberfound[p]){break}
    }
    if(!numberfound[p]){break}
  }
  success[exp] <- min(numberfound)
}

cat("In", sum(success), "out of", repetitions, "cases, all players have found theirown number.\n") 
cat("This corresponds to an empirical success probability of ", sum(success)/repetitions, ".\n", sep = "")
cat("This empirical probability should be close to the theoretical success probability:", 1 - sum(1/((m+1):n)))
