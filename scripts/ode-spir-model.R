##
## ODE SPIR model
##
## Author......: Luis Gustavo Nardin
## Last Change.: 08/31/2016
##
library(data.table)
library(deSolve)
library(ggplot2)


#############
## PATHS
#############
baseDir <- "/data/workspace/cmci/SPIR"
scriptDir <- paste0(baseDir, "/scripts")


#############
## FUNCTIONS
#############
source(paste0(scriptDir, "/calcSwitch.R"))
source(paste0(scriptDir, "/calcUtilities.R"))
source(paste0(scriptDir, "/SPIRmodel.R"))


#############
## INPUT PARAMETERS
#############
pars <- list(
  R0 <- 2,
  duration <- 65,
  G <- 1 / duration,
  Bs <- R0 / duration,
  betaS <- 1 - exp(-Bs),
  rho <- 0.25,
  gamma <- 1 - exp(-G),
  lambda <- 0,
  kappa <- 1,
  delta <- 0.01,
  h <- 364,
  #payoffs <- c(1.00, 0.95, 0.10, 0.95),
  payoffs <- c(1, 0.99, 0, 1),
  iswitch <- calc_iswitch(h, betaS, rho, gamma, lambda, kappa, payoffs)
)

yinit <- c(S = 99999 - 1, P = 0, I = 1, R = 0)
times <- seq(1, 10000, 1)


#############
## SOLVER
#############
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))


#############
## PLOTS
#############
isps <- data.table(i=iswitch[iswitch[,8] != 1,8])
isps <- rbind(isps, data.table(i=iswitch[iswitch[,7] == 0 &&
                                           iswitch[,9] == 1,7]))
plot <- ggplot(out, aes(x=time, y=(I / (S + P + I + R)) * 100)) +
  xlab("Time") + ylab("Proportion of Infected (%)") +
  ylim(0, min(100, max(ifelse(nrow(isps) == 0, 0, (max(isps$i) * 100)),
                       max(as.numeric(as.character(out$I)) /
                             (as.numeric(as.character(out$S)) +
                                as.numeric(as.character(out$P)) +
                                as.numeric(as.character(out$I)) +
                                as.numeric(as.character(out$R)))) * 100))) +
  geom_line(size=0.9) +
  theme(axis.title.x = element_text(colour='black', size=20, face='bold'),
        axis.title.y = element_text(colour='black', size=20, face='bold'),
        axis.text.x = element_text(colour='black', size=18, face='bold'),
        axis.text.y = element_text(colour='black', size=18, face='bold'),
        axis.line.x = element_line(colour='black', size=1.5, linetype='solid'),
        axis.line.y = element_line(colour='black', size=1.5, linetype='solid'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))

if(nrow(isps) > 0){
  plot <- plot + geom_hline(data=isps, aes(yintercept=i*100),
                            linetype="dashed", size=0.9)
}
