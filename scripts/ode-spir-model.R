##
## ODE SPIR model
##
## Author......: Luis Gustavo Nardin
## Last Change.: 07/08/2016
##
library(data.table)
library(deSolve)
library(foreach)
library(ggplot2)
library(doParallel)
registerDoParallel(cores=2)

setwd("/data/workspace/cmci/SPIR/scripts")

source("calcSwitch.R")
source("SPIRmodel.R")

##
## Input parameters Seasonal Flu
##
pars <- list(
  R0 <- 2,
  duration <- 8,
  G <- 1 / duration,
  Bs <- R0 / duration,
  betaS <- 1 - exp(-Bs),
  rho <- 0.01,
  gamma <- 1 - exp(-G),
  lambda <- 0,
  kappa <- 2,
  delta <- 0.1,
  h <- 40,
  payoffs <- c(1.00, 0.95, 0.60, 1.00),
  iswitch <- calc_iswitch(h, betaS, rho, gamma, lambda, kappa, payoffs)
)

yinit <- c(S = 100000 - 1, P = 0, I = 1, R = 0)
times <- seq(1, 2000, 1)

##
## Solve the ODE
##
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))

##
## Plot the proportion of infected over time.
## The dashed line represents the i switching point(s)
##
isps <- data.table(i=iswitch[iswitch[,8] != 1,8])
plot <- ggplot(out, aes(x=time, y=(I / (S+P+I+R)) * 100)) +
  xlab("Time") + ylab("Proportion of Infected (%)") +
  ylim(0, min(100, max(ifelse(nrow(isps) == 0, 0, (max(isps$i) * 100) + 5),
                       max(as.numeric(as.character(out$I)) /
                             (as.numeric(as.character(out$S)) +
                                as.numeric(as.character(out$P)) +
                                as.numeric(as.character(out$I)) +
                                as.numeric(as.character(out$R)))) * 100) + 5)) +
  geom_line(size=0.9) +
  theme(axis.title.x = element_text(colour='black', size=20, face='bold'),
        axis.title.y = element_text(colour='black', size=20, face='bold'),
        axis.text.x = element_text(colour='black', size=18, face='bold'),
        axis.text.y = element_text(colour='black', size=18, face='bold'),
        axis.line = element_line(colour='black', size=1.5, linetype='solid'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))

if(nrow(isps) > 0){
  plot <- plot + geom_hline(data=isps, aes(yintercept=i*100),
                            linetype="dashed", size=0.9)
}

ggsave("/data/projects/current/cmci/project-3/sub-projects/spir/fluavian-dyn-h40-d0.01-k2.pdf",
       plot=plot)
