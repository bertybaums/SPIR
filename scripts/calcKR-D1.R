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
h <- 100

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

optimizeR <- function(r, peakSizeK){
  ## ODE parameters with changed P payoff
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
  
  simR <- as.data.table(lsoda(y=yinit, times=times, func=SPIRmodel, parms=parsR,
          verbose=FALSE, rtol=1e-3, atol=1e-3))
  peakSizeR <- max(simR$I) / sum(yinit)
  
  return(abs(peakSizeR - peakSizeK))
}


H <- c(15, 30, 45, 90, 180, 360, 720, 1440)

deltaK <- 0.001
maxK <- 3
error <- 0.001
maxIter <- 100
numPop <- 50

header <- TRUE
for(i in 1:length(H)){
  k <- kappa
  
  hasP <- TRUE
  while(hasP){
    ## ODE parameters
    parsK <- list(
      R0 = R0,
      duration = duration,
      G = G,
      Bs = Bs,
      betaS = betaS,
      rho = rho,
      gamma = gamma,
      lambda = lambda,
      kappa = k,
      delta = delta,
      h = H[i],
      payoffs = payoffs,
      iswitch = calc_iswitch(H[i], betaS, rho, gamma, lambda, k, payoffs)
    )
    
    ## Simulate the dynamics for Kappa and default P payoff
    simK <- as.data.table(lsoda(yinit, times, SPIRmodel, parsK,
                                rtol=1e-3, atol=1e-3))
    peakSizeK <- max(simK$I) / sum(yinit)
    
    result <- DEoptim(fn=optimizeR, lower=0, upper=1,
        control=DEoptim.control(trace=FALSE, VTR=error,
            itermax=maxIter, NP=numPop,
            parallelType=2),
        peakSizeK=peakSizeK)
    
    if((result$optim$bestval > error) | (k > maxK)){
      hasP <- FALSE
    } else {
      outputKR_PS <- data.table(cbind(H[i], k, result$optim$bestmem))
      names(outputKR_PS) <- c("h", "k", "rho")
      
      write.table(outputKR_PS, file=paste0(outputDir,"/calcKR_PS.csv"),
          append=!header, quote=FALSE, sep=";",
          col.names=header, row.names=FALSE)
      
      k <- k + deltaK
      
      if(header){
        header <- FALSE
      }
    }
  }
}
