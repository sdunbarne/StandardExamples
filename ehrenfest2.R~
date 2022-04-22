library(markovchain)

N <- 7; p <- 1/2; q <- 1 - p;
stateNames <- as.character( 0:N )
## Be careful here, because states numbered from 0,
## but R indexes from 1
transMatrix <- matrix(0, N+1,N+1)
transMatrix[1,1] <- q
transMatrix[1,2] <- p
transMatrix[N+1, N  ] <- q
transMatrix[N+1, N+1] <- p
for (row in 2:N) {
    transMatrix[row, row-1] <- ((row-1)/N)*q
    transMatrix[row,row] <- ((N-(row-1))/N)*q + ((row-1)/N)*p
    transMatrix[row,row+1] <- ((N-(row-1))/N)*p
}
startState <- "3"

ehrenfest2 <- new("markovchain", transitionMatrix=transMatrix,
                 states=stateNames, name="Ehrenfest2")
print(ehrenfest2)

print( summary(ehrenfest2) )

pathLength <- 10 
path <- rmarkovchain(n=pathLength,
                            object = ehrenfest2, t0 = startState)
cat("A sample starting from startState ", startState, path, "\n")
path <- rmarkovchain(n=pathLength,
                            object = ehrenfest2, t0 = startState)
cat("Another sample starting from ", startState, path, "\n")

if ( length( transientStates(ehrenfest2) ) == 0 ) {
    largeN <- 1000; startStable <- 100
    path <- rmarkovchain(n=largeN,
                         object = ehrenfest2, t0 = startState)
    stablePattern <-  path[startStable:largeN]
    empiricalStable <- rep(0, N+1)
    for (i in seq_along(stateNames)) { 
        empiricalStable[i] <-  mean( stablePattern == stateNames[i] )
    }
    theorStable <- steadyStates(ehrenfest2)
    cat("Empirical stable distribution: ",  empiricalStable, "\n")
    cat("Theoretical stable distribution: ", theorStable, "\n")
}

## NAME: ehrenfest2.R
## USAGE: within R, at interactive prompt
##        source("ehrenfest2.R")
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

