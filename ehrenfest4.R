library(markovchain)

k <- 7
stateNames <- as.character( 0:k )
## Be careful here, because states numbered from 0,
## but R indexes from 1
transMatrix <- matrix(0, k+1,k+1)
transMatrix[1,1] <- 1
transMatrix[k+1, k+1] <- 1
for (row in 2:k) {  #row 2 is state 1, row 7 is state 6
    transMatrix[row, row-1] <- (row-1)*(k-(row-1))/k^2
    transMatrix[row,row] <- (row-1)^2/k^2 + (k-(row-1))^2/k^2
    transMatrix[row,row+1] <- (row-1)*(k-(row-1))/k^2
}
startState <- "3"

ehrenfest4 <- new("markovchain", transitionMatrix=transMatrix,
                 states=stateNames, name="Ehrenfest4")
print(ehrenfest4)

print( summary(ehrenfest4) )

pathLength <- 10 
path <- rmarkovchain(n=pathLength,
                     object = ehrenfest4, t0 = startState)
cat("A sample starting from ", startState, ":", path, "\n")
path <- rmarkovchain(n=pathLength,
                     object = ehrenfest4, t0 = startState)
cat("Another sample starting from ", startState, ":", path, "\n")

if ( length( transientStates(ehrenfest4) ) == 0 ) {
    largeN <- 1000; startStable <- 100
    path <- rmarkovchain(n=largeN,
                         object = ehrenfest4, t0 = startState)
    stablePattern <-  path[startStable:largeN]
    empiricalStable <- rep(0, k+1)
    for (i in seq_along(stateNames)) { 
        empiricalStable[i] <-  mean( stablePattern == stateNames[i] )
    }
    theorStable <- steadyStates(ehrenfest4)
    cat("Empirical long-time distribution: ",  empiricalStable, "\n")
    cat("Theoretical stable distribution: ", theorStable, "\n")
}

## NAME: ehrenfest4.R
## USAGE: within R, at interactive prompt
##        source("ehrenfest4.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: An R script using the markovchain library to 
##              simulate an alternative Ehrenfest urn model.              
## DIAGNOSTICS: some diagnostics implicit from markovchain
## CONFIGURATION AND ENVIRONMENT:  library(markovchain)
## DEPENDENCIES:  library(markovchain)
## INCOMPATIBILITIES: none known
## PROVENANCE: created Steve Dunbar, based on documentation in
## "The markovchain Package: A Package for Easily Handling Discrete
##  Markov Chains in R" by G. Spedicato, et al.
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:
## 1.  Make a random start state.
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Tue Jan 22 07:43:53 CST 2019
## KEYWORDS: Markov chain, stable distribution

