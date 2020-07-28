library(markovchain)

N <- 7
stateNames <- as.character( 0:N )
## Be careful here, because states numbered from 0,
## but R indexes from 1
transMatrix <- matrix(0, N+1,N+1)
transMatrix[1,1] <- 1
transMatrix[N+1, N+1] <- 1
for (row in 2:N) {  #row 2 is state 1, row 7 is state 6
    transMatrix[row, row-1] <- 1/2
    transMatrix[row, row+1] <- 1/2
}
startState <- "3"

absorbingbdy <- new("markovchain", transitionMatrix=transMatrix,
                 states=stateNames, name="AbsorbingBdy")
print(absorbingbdy)

print( summary(absorbingbdy) )

pathLength <- 10 
path <- rmarkovchain(n=pathLength,
                            object = absorbingbdy, t0 = startState)
cat("A sample starting from ", startState, path, "\n")
path <- rmarkovchain(n=pathLength,
                            object = absorbingbdy, t0 = startState)
cat("Another sample starting from ", startState, path, "\n")

if ( length( transientStates(absorbingbdy) ) == 0 ) {
    largeN <- 1000; startStable <- 100
    path <- rmarkovchain(n=largeN,
                            object = absorbingbdy, t0 = startState)
    stablePattern <-  path[startStable:largeN]
    empiricalStable <- rep(0, N+1)
    for (i in seq_along(stateNames)) { 
        empiricalStable[i] <-  mean( stablePattern == (i-1) )
    }
    theorStable <- steadyStates(absorbingbdy)
    cat("Empirical stable distribution: ",  empiricalStable, "\n")
    cat("Theoretical stable distribution: ", theorStable, "\n")
}

## NAME: markovchain.R
## USAGE: within R, at interactive prompt
##        source("markovchain.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: An R script using the markovchain library to 
##              simulate an absorbing random walk on a finite set.             
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
## VERSION: Version 1.0 as of Fri Jan 18 07:34:58 CST 2019
##          Version 1.1 as of Mon Jan 28 07:28:49 CST 2019
## KEYWORDS: Markov chain, stable distribution

