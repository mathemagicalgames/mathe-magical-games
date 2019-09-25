# Copyright (c) 2010 - 2018  Jonas Peters [jonas.peters@math.ku.dk]
# All rights reserved. See the file COPYING for license terms. 

# This is an implementation of a trick based on the riffle shuffle. 
# Details can be found in: .......
# The ideas are based on:
# D. Bayer, P. Diaconis: "Trailing the dovetail shuffle to its lair", 
# The Annals of Applied Probability 2:294-313, 1992

# The code is not optimized for speed.

# Do you want to see output?
output <- FALSE

# How many cards are in the deck?
num_cards <- 52

# How many times shall the trick be repeated? (You may start with one.)
num_exp <- 1000

# How many riffle shuffles?
k <- 3

# How many cards are we allowed to guess?
allow_to_check <- 1

set.seed(1)

cards <- 0:(num_cards-1)
res <- rep(0,num_exp)
for (j in 1:num_exp)
{
  cat('This is experiment number:', j, '\n')
  
  #####
  #shuffling
  #####
  perm <- floor(runif(num_cards,0,2^k))
  shuffled_cards <- rep(0,num_cards)
  counter <- 0
  for (i in 0:(2^k-1))
  {
    counter2<-length(cards[perm==i])
    if(counter2>0)
    {
      shuffled_cards[perm==i] <- cards[(counter+1):(counter+counter2)]
    }
    counter <- counter+counter2
  }
  #show(shuffled_cards)
  
  #####
  #cutting
  #####
  cut <- rbinom(num_cards,n=1,0.5)
  cut_cards <- NULL
  cut_cards[1:(num_cards-cut)] <- shuffled_cards[(cut+1):num_cards]
  cut_cards[(num_cards-cut+1):num_cards] <- shuffled_cards[1:cut]

  #####
  #place the top card somewhere
  #####
  sel_place <- rbinom(num_cards,n=1,0.5)
  sel_card <- cut_cards[1]
  final_cards <- cut_cards
  final_cards[1:(sel_place-1)] <- cut_cards[2:sel_place]
  final_cards[sel_place] <- sel_card
  
  #####
  # 1x cutting (this has no influence on the score)
  #####
  cut <- rbinom(num_cards,n=1,0.5)
  cut_cards <- NULL
  cut_cards[1:(num_cards-cut)] <- final_cards[(cut+1):num_cards]
  cut_cards[(num_cards-cut+1):num_cards] <- final_cards[1:cut]
  final_cards <- cut_cards
  
  #####
  #find the card
  #####
  nblength <- NULL

  for(i in 1:num_cards)
  {
    nb1 <- sort(final_cards==((final_cards[i]-1) %% num_cards), index.return=TRUE)$ix[num_cards]
    nb2 <- sort(final_cards==((final_cards[i]+1) %% num_cards), index.return=TRUE)$ix[num_cards]
    nblength[i] <- ((i - nb1) %% num_cards) + ((nb2 - i) %% num_cards)
  }
  if(output){
    cat('These are the mixed cards and their scores:\n') 
    show(rbind(final_cards, nblength))
  }
  guess <- final_cards[sort(nblength, decreasing = TRUE, index.return = TRUE)$ix[1:allow_to_check]]
  
  #####
  #result
  #####
  if(output)
  {
    cat('These are my guesses:\n')
    show(guess)
    cat('And this is the true card:\n')
    show(sel_card)
  }
  if(sel_card %in% guess)
  {
    res[j]<-1
  } else
  {
    show(sort(nblength, decreasing = TRUE))
    show(rbind(final_cards, nblength))
    cat('These are my guesses:\n')
    show(guess)
    cat('And this is the true card:\n')
    show(sel_card)
  }
  cat('---------------\n')
}
cat('\n===========\n')
cat('Out of ', num_exp, ' trials, the trick worked in ', sum(res)/num_exp*100, '% of the cases.\n',sep ='')
\end{lstlisting}
\end{center}

\begin{center}
\begin{lstlisting}[language=R, columns=fullflexible, commentstyle=\color{commentgray}\itshape, backgroundcolor=\color{light-gray}, otherkeywords={!,!=,~,$,\&,\%/\%,\%*\%,\%\%,<-,<<-,_,/}]

n <- 52
ellVec <- 1:14
TVdistance <- rep(NA,max(ellVec))
numOfAn <- min(n,2^max(ellVec))
An <- rep(NA,numOfAn)


# compute the Eulerian numbers (watch out, they are shifted by one: A_{n,r} is the (r-1)th EN)
# see https://en.wikipedia.org/wiki/Eulerian_number
# and http://dlmf.nist.gov/26.14

# iterative procedure
#  An[1] <- 1
#  for(r in 2:numOfAn){
#    oneToRMinusOne <- 1:(r-1)
#    An[r] <- r^n - sum( choose(n+r-oneToRMinusOne,n) * An[oneToRMinusOne] )
#  }

# direct formula
An[1] <- 1
for(r in 2:numOfAn){
  r2 <- r
  if(r2 >= n %/% 2) r2 <- n - r2 + 1
  sig <- rep(c(1,-1), length.out = r2) # 1 for k=0; 1 -1 (k=1), ...
  An[r] <- round(sum( sig * choose(n+1, 0:(r2-1)) * (r2:1)^n ))
}

# compute the distance for different ells 
# for r > 2^ell, the left expression becomes zero, thus, there are [factorial(n) - sum(An)] summands missing.
for(ell in ellVec){
  oneTo2ToEll <- 1:numOfAn
  tmp1 <- abs(choose(2^ell + n - oneTo2ToEll, n)/(2^(n*ell)) - 1/factorial(n))
  tmp2 <- An * ( tmp1 )
  TVdistance[ell] <- 0.5 * sum(tmp2) + 0.5 * (1 - sum(An)/factorial(n))
}

show(TVdistance)

library(tikzDevice)
tw <- 4.9823
tikz(paste("figure-shufflingdistance.tex",sep=""), width = tw, height = 0.63*tw)
par( mar=c(4, 3.5, 1, 1) + 0.1, mgp = c(2.3, 1, 0), cex = .9)
plot(ellVec, TVdistance, col = "black",  pch = 19, cex = 2.0, 
     xlab = "number of shuffles", ylab = "TV distance")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
dev.off()
