##
## Evaluation
##
## Author......: Luis Gustavo Nardin
## Last Change.: 12/28/2016
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


H <- c(15, 30, 45, 90, 180, 360, 720, 1440)
Rhos <- seq(0.01, 1, 0.01)

output <-
    foreach(i=1:length(H), .combine='rbind') %dopar%{
      outRho <- NULL
      for(r in 1:length(Rhos)){
        ## ODE parameters with changed rho
        parsR <- list(
            R0 = R0,
            duration = duration,
            G = G,
            Bs = Bs,
            betaS = betaS,
            rho = Rhos[r],
            gamma = gamma,
            lambda = lambda,
            kappa = kappa,
            delta = delta,
            h = H[i],
            payoffs = payoffs,
            iswitch = calc_iswitch(H[i], betaS, Rhos[r], gamma, lambda, kappa, payoffs)
        )
        
        ## Simulate the dynamics with changed rho
        simR <- as.data.table(lsoda(yinit, times, SPIRmodel, parsR, rtol=1e-3, atol=1e-3))
        
        ## Calculate output metrics
        out <- c(0, 0, 0, 0)
        out[PROTECTION] <- 1 - Rhos[r]
        out[PEAK_SIZE] <- max(simR$I) / sum(yinit)
        out[TIME_PEAK] <- which(simR$I == max(simR$I))[1]
        
        aux <- which(simR$I < 1)
        aux <- ifelse(length(aux) > 0, aux[1] - 1, nrow(simR))
        out[AVG_PAYOFF] <- sum((simR[1:aux,]$S * payoffs[1]) +
                    (simR[1:aux,]$P * payoffs[2]) +
                    (simR[1:aux,]$I * payoffs[3]) +
                    (simR[1:aux,]$R * payoffs[4])) /
            (sum(yinit) * aux)
        
        outRho <- rbind(outRho, cbind(H[i], out[PROTECTION], out[PEAK_SIZE],
                out[TIME_PEAK], out[AVG_PAYOFF]))
      }
      
      outRho
    }

data <- data.table(output)
names(data) <- c("h", "protection", "peakSize", "timePeak", "avgPayoff")
data <- data[order(data$protection)]

write.table(data, file=paste0(outputDir,"/calcP_PT.csv"),
    append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)
