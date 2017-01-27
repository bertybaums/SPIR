##
## Calculate a distribution of Switch Points
##
## Author......: Luis Gustavo Nardin
## Last Change.: 12/28/2016
##
library(doParallel)
library(data.table)
registerDoParallel(cores=24)

#############
## COMMAND-LINE
#############
args <- commandArgs(TRUE)

combID <- as.numeric(as.character(args[1]))


#############
## PATHS
#############
baseDir <- "/scratch/nardluis"
scriptDir <- paste0(baseDir, "/scripts/spir")
outputDir <- paste0(baseDir, "/data/spir/dist")


#############
## FUNCTIONS
#############
source(paste0(scriptDir, "/calcSwitch.R"))


#############
## INPUT PARAMETERS
#############
## Number of replications
N <- 100000

## Disease
bs <- 0.031
g <- 0.015
l <- 0
payoffs <- c(1.00, 0, 0, 0)

## Random
h <- as.integer(rgamma(N, shape=3, scale=15))
rho <- runif(N, 0.00, 0.50)
k <- rnorm(N, 1.00, 0.02)
payoff.P <- runif(N, 0.80, 1.00)
payoff.I <- runif(N, 0.00, 0.20)
payoff.R <- runif(N, 0.90, 1.00)

## Select 
if(combID == 2){
  payoff.P <- rep(0.95, N)
  payoff.I <- rep(0.10, N)
  payoff.R <- rep(0.95, N)
  rho <- rep(0.25, N)
  k <- rep(1, N)
  h <- rep(45, N)
  
} else if((combID >= 3) & (combID <= 29)){
  ## payoff.P
  if((combID >= 3) & (combID <= 11)){
    payoff.P <- rep(0.80, N)
  } else if((combID >= 12) & (combID <= 20)){
    payoff.P <- rep(0.90, N)
  } else if((combID >= 21) & (combID <= 29)){
    payoff.P <- rep(0.95, N)
  }
  
  ## payoff.I
  if(((combID >= 3) & (combID <= 5)) |
      ((combID >= 12) & (combID <= 14)) |
      ((combID >= 21) & (combID <= 23))){
    payoff.I <- rep(0.05, N)
  } else if(((combID >= 6) & (combID <= 8)) |
      ((combID >= 15) & (combID <= 17)) |
      ((combID >= 24) & (combID <= 26))){
    payoff.I <- rep(0.10, N)
  } else if(((combID >= 9) & (combID <= 11)) |
      ((combID >= 18) & (combID <= 20)) |
      ((combID >= 27) & (combID <= 29))){
    payoff.I <- rep(0.15, N)
  }
  
  ## payoff.R
  aux <- c(0.90, 0.95, 1.00)
  payoff.R <- rep(aux[(combID %% 3) + 1], N)
  
} else if((combID >= 30) & (combID <= 34)){
  ## Rho
  rho <- rep(((combID - 29) * 0.1), N)
  
} else if((combID >= 35) & (combID <= 39)){
  ## Kappa
  aux <- c(0.90, 0.95, 1.00, 1.05, 1.10)
  k <- rep(aux[combID - 34], N)
  
} else if((combID >= 40) & (combID <= 45)){
  ## Planning Horizon
  aux <- c(1, 15, 45, 75, 90, 180)
  h <- rep(aux[combID - 39], N)
}


#############
## MONTE CARLO SIMULATION
#############
switch <- c()
switch <- foreach(i = 1:N, .combine=rbind) %dopar%{
  payoffs[2] <- payoff.P[i]
  payoffs[3] <- payoff.I[i]
  payoffs[4] <- payoff.R[i]
  calc_iswitch(h[i], bs, rho[i], g, l, k[i], payoffs)[1,]
}

data <- data.table(switch)

write.table(data, file=paste0(outputDir,"/calcSW-", combID,".csv"),
    append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)
