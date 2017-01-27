##
## Evaluation
##
## Author......: Luis Gustavo Nardin
## Last Change.: 12/28/2016
##
library(data.table)
library(DEoptim)
library(deSolve)
library(doParallel)
library(foreach)
library(ggplot2)
registerDoParallel(cores=3)


#############
## PATHS
#############
baseDir <- "/data/workspace/cmci/SPIR"
scriptDir <- paste0(baseDir, "/scripts")
inputDir <- paste0(baseDir, "/data/raw/disease1")
outputDir <- paste0(baseDir, "/data/raw/disease1")
figureDir <- paste0(baseDir, "/data/figures/disease1")


#############
## FUNCTIONS
#############
source(paste0(scriptDir, "/calcUtilities.R"))
source(paste0(scriptDir, "/calcSwitch.R"))
source(paste0(scriptDir, "/SPIRmodel.R"))


###############
## CONSTANTS
###############
PROTECTION <- 1
PEAK_SIZE <- 2
TIME_PEAK <- 3
AVG_PAYOFF <- 4

THEME <- theme(axis.title.x = element_text(color = 'black', size = 16, face = 'bold',
        margin=margin(t=0.2, unit = "cm")),
    axis.title.y = element_text(color = 'black', size = 16, face = 'bold',
        margin=margin(r=0.4, unit = "cm")),
    axis.text.x = element_text(color = 'black', size = 12, face = 'bold'),
    axis.text.y = element_text(color = 'black', size = 12, face = 'bold'),
    axis.line.x = element_line(color='black', size=1, linetype='solid'),
    axis.line.y = element_line(color='black', size=1, linetype='solid'),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
    legend.position = "none",
    legend.title = element_text(color="black", size=14, face="bold"),
    legend.text = element_text(color="black", size=12, face="bold"),
    legend.key = element_rect(fill = "white"))


###############
## DISEASE PARAMETERS
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
rho <- 0.5

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

# Planning horizons
H <- c(15, 30, 45, 90, 180, 360, 720, 1440)

###############
## Generate the output metrics when increasing protection
###############
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

## Organize dataset
data <- data.table(output)
names(data) <- c("h", "protection", "peakSize", "timePeak", "avgPayoff")
data <- data[order(data$protection)]

write.table(data, file=paste0(outputDir,"/calcP_PT.csv"),
    append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)

## Generate plots
data <- fread(paste0(inputDir, "/calcP_PT.csv"),
    header=TRUE, sep=";")

dataIP <- data.table(cbind(h=15,protection=0,peakSize=0,timePeak=0,avgPayoff=0))
for(p in H){
  maxIP <- max(data[which(h == p),]$peakSize)
  dataIP <- rbind(dataIP, data[which((h == p) & (peakSize < maxIP)),])
}

gPeakSize <- ggplot(dataIP, aes(x=protection * 100, y=peakSize * 100,
            group=h,
            color=factor(h))) +
    geom_line(size=0.9) +
    xlim(0, 100) +
    xlab("Protection (%)") +
    ylab("Peak Size (%)") +
    scale_color_manual(name="Planning\nHorizon",
        values=c("red", "yellow4", "green4", "magenta4", "blue", "orange", "purple", "black")) +
    THEME +
    theme(legend.position = "right")

ggsave(file=paste0(figureDir, "/protectionPeakSize.png"),
    plot=gPeakSize, width=10, height=6.5)


dataIP <- data.table(cbind(h=15,protection=0,peakSize=0,timePeak=0,avgPayoff=0))
for(p in H){
  maxIP <- data[which(h == p),]$timePeak[1]
  dataIP <- rbind(dataIP, data[which((h == p) & (timePeak != maxIP)),])
}

gTimePeak <- ggplot(dataIP, aes(x=protection * 100, y=timePeak,
            group=h,
            color=factor(h))) +
    geom_line(size=0.9) +
    xlim(0, 100) +
    xlab("Protection (%)") +
    ylab("Time to Peak") +
    scale_color_manual(name="Planning\nHorizon",
        values=c("red", "yellow4", "green4", "magenta4", "blue", "orange", "purple", "black")) +
    THEME +
    theme(legend.position = "right")

ggsave(file=paste0(figureDir, "/protectionPeakTime.png"),
    plot=gTimePeak, width=10, height=6.5)


###############
## uP x Kappa - Peak Size
###############
optimizeP <- function(up, peakSizeK){
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
  
  return(abs(peakSizeP - peakSizeK))
}

H <- c(15, 30, 45, 90, 180, 360, 720, 1440)

deltaK <- 0.001
maxK <- 3
error <- 0.001

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
    
    result <- DEoptim(fn=optimizeP, lower=0, upper=1,
        control=DEoptim.control(trace=TRUE, VTR=error,
            itermax=100, NP=50, parallelType=2),
        peakSizeK=peakSizeK)
    
    if((result$optim$bestval > error) | (k > maxK)){
      hasP <- FALSE
    } else {
      outputKP_PS <- data.table(cbind(H[i], k, result$optim$bestmem))
      names(outputKP_PS) <- c("h", "k", "up")
      
      write.table(outputKP_PS, file=paste0(outputDir,"/calcKP_PS.csv"),
          append=!header, quote=FALSE, sep=";",
          col.names=header, row.names=FALSE)
      
      k <- k + deltaK
      
      if(header){
        header <- FALSE
      }
    }
  }
}

## Generate plots
outputKP_PS <- fread(paste0(inputDir, "/calcKP_PS.csv"),
        header=TRUE, sep=";")

dataKP <- NULL
for(p in H){
  minKP <- min(outputKP_PS[which(h == p),]$up)
  dataKP <- rbind(dataKP, outputKP_PS[which((h == p) & (up > minKP)),])
}

kp_ps <- ggplot(dataKP, aes(x=k, y=up,
            group=h,
            color=factor(h))) +
    geom_line(size=0.9) +
    xlab("Kappa") +
    ylab("Prophylactic Payoff") +
    ylim(0.945, 1) +
    scale_color_manual(name="Planning\nHorizon",
        values=c("red", "yellow4", "green4", "magenta4", "blue", "orange", "purple", "black")) +
    THEME +
    theme(legend.position = "right")

ggsave(file=paste0(figureDir, "/kappaXuP-PeakSize.png"),
    plot=kp_ps, width=10, height=6.5)


###############
## Rho x Kappa - Peak Size
###############
optimizeR <- function(r, peakSizeK){
  
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
            itermax=100, NP=10, parallelType=2),
        peakSizeK=peakSizeK)
    
    if((result$optim$bestval > error) | (k > maxK)){
      hasP <- FALSE
    } else {
      outputKR_PS <- data.table(cbind(H[i], k, result$optim$bestmem))
      names(outputKR_PS) <- c("h", "k", "rho")
      
      write.table(outputKR_PS, file=paste0(outputDir,"/calcKR_PS.csv"), append=!header,
          quote=FALSE, sep=";", col.names=header, row.names=FALSE)
      
      k <- k + deltaK
      
      if(header){
        header <- FALSE
      }
    }
  }
}

## Generate plots
outputKR_PS <- fread(paste0(inputDir, "/calcKR_PS.csv"),
    header=TRUE, sep=";")
    
dataKR <- data.table(cbind(h=15,k=1,rho=1))
for(p in H){
  maxKR <- max(outputKR_PS[which(h == p),]$rho)
  dataKR <- rbind(dataKR, outputKR_PS[which((h == p) & (rho < maxKR)),])
}

kr_ps <- ggplot(dataKR, aes(x=k, y=(1 - rho) * 100,
            group=factor(h),
            color=factor(h))) +
    geom_line(size=0.9) +
    xlab("Kappa") +
    ylab("Protection (%)") +
    ylim(99, 100) +
    scale_color_manual(name="Planning\nHorizon",
        values=c("red", "yellow4", "green4", "magenta4", "blue", "orange", "purple", "black")) +
    THEME +
    theme(legend.position = "right")

ggsave(file=paste0(figureDir, "/kappaXrho-PeakSize.png"),
    plot=kr_ps, width=10, height=6.5)


###############
## Rho x uP - Peak Size
###############
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
            itermax=100, NP=10, parallelType=2),
        peakSizeR=peakSizeR)
    
    if((result$optim$bestval > error) | (r > maxR)){
      hasP <- FALSE
    } else {
      outputPR_PS <- data.table(cbind(H[i], r, result$optim$bestmem))
      names(outputPR_PS) <- c("h", "rho", "up")
      
      write.table(outputPR_PS, file=paste0(outputDir,"/calcPR_PS.csv"), append=!header,
          quote=FALSE, sep=";", col.names=header, row.names=FALSE)
      
      r <- r + deltaR
      
      if(header){
        header <- FALSE
      }
    }
  }
}

## Generate plots
outputPR_PS <- fread(paste0(inputDir, "/calcPR_PS.csv"),
        header=TRUE, sep=";")

dataPR <- data.table(cbind(h=15,rho=1,up=1))
for(p in H){
  minUP <- min(outputPR_PS[which(h == p),]$up)
  dataPR <- rbind(dataPR, outputPR_PS[which((h == p) & (up > minUP)),])
}

pr_ps <- ggplot(dataPR, aes(x=up, y=(1 - rho) * 100,
            group=h,
            color=factor(h))) +
    geom_line(size=0.9) +
    xlab("Prophylactic Payoff") +
    ylab("Protection (%)") +
    ylim(25, 100) + xlim(0.85, 1) +
    scale_color_manual(name="Planning\nHorizon",
        values=c("red", "yellow4", "green4", "magenta4", "blue", "orange", "purple", "black")) +
    THEME +
    theme(legend.position = "right")

ggsave(file=paste0(figureDir, "/rhoXuP-PeakSize.png"),
    plot=pr_ps, width=10, height=6.5)
