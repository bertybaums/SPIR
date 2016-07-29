##
## Calculate Switchs in parallel for INL
##
## Author......: Luis Gustavo Nardin
## Last Change.: 07/27/2016
##
library(parallel)

scriptPath <- "/data/workspace/cmci/SPIR/scripts"

source(paste0(scriptPath,"/","calcSwitch.R"))

calcSwitch <- function(x, H, bs, Rho, g, l, k, payoffs, scriptPath){
  setwd(scriptPath)
  source("calcSwitch.R")
  
  payoffs[4] <- x
  
  data <- NULL
  for(h in H){
    for(rho in Rho){
      data <- rbind(data, calc_iswitch(h, bs, rho, g, l, k, payoffs))
    }
  }
  
  return(data)
}


cl <- makeCluster(2)

###############
## DISEASE 2 INPUT PARAMETERS
###############
name <- "disease2"

# Disease duration
duration <- 8

# R0
R0 <- 2

# gamma
gamma <- 1 / duration

# beta
betaS <- R0 / duration

# Infection probability (Susceptible)
bs <- 1 - exp(-betaS)

# Prophylactic protection
rho <- 0.01
Rho <- seq(0.1, 0.1, 0.1)

# Recover probability
g <- 1 - exp(-gamma)

# Discount factor (0 = No discount)
lambda <- 0

# Fear factor (1 = No fear)
kappa <- 1

# Planning horizon
h <- 30
H <- seq(1, 400)

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.95, 0.60, 1)
uR <- seq(0, 1, 0.01)


data <- clusterApply(cl, uR, calcSwitch, H, bs, Rho, g, lambda, kappa, payoffs, scriptPath)


for(i in 1:length(uR)){
  write.table(data[[i]], paste0(scriptPath,"/",name,"-",uR[i],".csv"),
              quote=FALSE, sep=";", row.names=FALSE)
}

stopCluster(cl)
