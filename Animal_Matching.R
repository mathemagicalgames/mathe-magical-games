# Copyright (c) 2017 - 2018  
# Nicolai Meinshausen [meinshausen@stat.math.ethz.ch]
# Jonas Peters [jonas.peters@math.ku.dk]
# All rights reserved.  See the file COPYING for license terms. 


## dimension (will result in p^2+p+1 players and unique animals)
p <- 3
## check whether p is a small prime number 
if(! p %in% c(2,3,5,7,11, 13)) stop(" Dimension p has to be a small
prime: 2,3,5,7 or (if you are not impatient) also 11 or 13")

## names for animals -- is padded with characters in the end for a potentially larger number of players -- can be easily changed here 
names <- c("Lion", "Giraffe", "Dog", "Cat", "Mouse", "Elephant", "Snake","Eagle","Frog","Spider","Dolphin","Jellyfish","Earthworm", as.character(1:150))

## start from {0,1,...,p-1}
set <- 0:(p-1)
## embed into three dimensions
three.dim <- expand.grid(set,set,set)

## select points that lead to unique lines through origin (this is not just one possible choice -- keeping those vectors whose first nonzero entry is a 1)
keep <- which( apply(three.dim,1, function(x) if(identical(as.numeric(x),c(0,0,0))) FALSE else  x[which(x!=0)[1]]==1))

## for the p^2+p+1 unique lines through the origin, keep a representative point
lines <- as.matrix(three.dim[keep,])

## m is now the number of players and unique animals
m <- p^2+p+1
## q is the number of possible pairs between players (or animals)
q <- (m*(m-1))/2
## s is the number of animals each player will have (or players that will have a specific animal)
s <- p+1

## the matrix M will store the indices of s animals of player i in row i
M <- matrix(nrow=q,ncol=s)
cc <- 0
for (j1 in 1:(m-1)){
    for (j2 in (j1+1):m){
        o1 <- lines[j1,]
        o2 <- lines[j2,]
        tmp <- c(j1,j2)
        for (c1 in 0:p){
            for (c2 in 0:p){
                vec <- as.numeric((c1*o1 + c2*o2) %% p)
                new <- which( apply( sweep( lines,2,vec,FUN="-")==0, 1, all))
                tmp <- sort(unique(c(tmp, new)))
            }
        }
        cc <- cc+1
        M[ cc,] <- tmp
    }
}
M <- unique(M)
## A is the incidence matrix for the choice of animals in M
A <- matrix(0,nrow=m,ncol=m)
for (j in 1:m) A[j, M[j,]] <- 1

## can check that A satisfies A A^{\transp} =pI + J
print( A%*%t(A) )
        

## write out table of animals (latex version TRUE or FALSE )
latex <- FALSE
for (j in 1:m){
    if (latex){
        cat("\n Player ",j," & ", paste(names[M[j,]],collapse=" & " )," \\\\ ")
    }else{
        cat("\n Player ",j," takes ", paste(names[M[j,]],collapse=", " ),"  ")
    }
}

