##
## Evaluation
##
## Author......: Luis Gustavo Nardin
## Last Change.: 12/27/2016
##
library(data.table)
library(DEoptim, lib="/scratch/nardluis/rpackages")
library(deSolve)
library(doParallel)
library(foreach)
registerDoParallel(cores=24)


#############
## PATHS
#############
baseDir <- "/scratch/nardluis"
scriptDir <- paste0(baseDir, "/scripts/spir")
outputDir <- paste0(baseDir, "/data/spir/disease1")


#############
## FUNCTIONS
#############
source(paste0(scriptDir, "/calcSwitch.R"))
source(paste0(scriptDir, "/SPIRmodel.R"))


###############
## CONSTANTS
###############
PROTECTION <- 1
PEAK_SIZE <- 2
TIME_PEAK <- 3
AVG_PAYOFF <- 4


###############
## INPUT PARAMETERS
###############
# R0
R0 <- 2

# Disease duration
duration <- 65

# gamma
G <- 1 / duration

# beta
Bs <- R0 / duration

# Infection probability (Susceptible)
betaS <- 1 - exp(-Bs)

# Prophylactic protection
rho <- 0.01

# Recover probability
gamma <- 1 - exp(-G)

# Discount factor (0 = No discount)
lambda <- 0

# Fear factor (1 = No fear)
kappa <- 1

# Decision frequency probability
delta <- 0.01

# Planning horizon
h <- 30

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.95, 0.1, 0.95)

# Number of agents
N <- 100000

# Time steps
timesteps <- 3000

# Initial number of Infcetious
initI <- 100

# Initial ODE values
yinit <- c(S = N - initI, P = 0, I = initI, R = 0)

# Length of the simulation
times <- seq(1, timesteps, 1)

optimizeP <- function(up, peakSizeR){
  
  parsP <- list(
      R0 = R0,
      duration = duration,
      G = G,
      Bs = Bs,
      betaS = betaS,
      rho = rho,
      gamma = gamma,
      lambda = lambda,
      kappa = kappa,
      delta = delta,
      h = H[i],
      payoffs = c(payoffs[1], up, payoffs[3], payoffs[4]),
      iswitch = calc_iswitch(H[i], betaS, rho, gamma, lambda, kappa,
          c(payoffs[1], up, payoffs[3], payoffs[4]))
  )
  
  simP <- as.data.table(lsoda(y=yinit, times=times, func=SPIRmodel, parms=parsP,
          verbose=FALSE, rtol=1e-3, atol=1e-3))
  peakSizeP <- max(simP$I) / sum(yinit)
  
  return(abs(peakSizeP - peakSizeR))
}


H <- c(15, 30, 45, 90, 180, 360, 720, 1440)

deltaR <- 0.001
maxR <- 1
error <- 0.001
maxIter <- 100
numPop <- 50

header <- TRUE
for(i in 1:length(H)){
  
  r <- deltaR
  hasP <- TRUE
  while(hasP){
    ## ODE parameters
    parsR <- list(
        R0 = R0,
        duration = duration,
        G = G,
        Bs = Bs,
        betaS = betaS,
        rho = r,
        gamma = gamma,
        lambda = lambda,
        kappa = kappa,
        delta = delta,
        h = H[i],
        payoffs = payoffs,
        iswitch = calc_iswitch(H[i], betaS, r, gamma, lambda, kappa, payoffs)
    )
    
    ## Simulate the dynamics for Kappa and default P payoff
    simR <- as.data.table(lsoda(yinit, times, SPIRmodel, parsR,
            rtol=1e-3, atol=1e-3))
    peakSizeR <- max(simR$I) / sum(yinit)
    
    result <- DEoptim(fn=optimizeP, lower=0, upper=1,
        control=DEoptim.control(trace=FALSE, VTR=error,
            itermax=maxIter, NP=numPop,
            parallelType=2),
        peakSizeR=peakSizeR)
    
    if((result$optim$bestval > error) | (r > maxR)){
      hasP <- FALSE
    } else {
      outputPR_PS <- data.table(cbind(H[i], r, result$optim$bestmem))
      names(outputPR_PS) <- c("h", "rho", "up")
      
      write.table(outputPR_PS, file=paste0(outputDir,"/calcPR_PS.csv"),
          append=!header, quote=FALSE, sep=";",
          col.names=header, row.names=FALSE)
      
      r <- r + deltaR
      
      if(header){
        header <- FALSE
      }
    }
  }
}
