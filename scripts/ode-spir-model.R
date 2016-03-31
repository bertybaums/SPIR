##
## ODE SPIR model
##
## Author......: Luis Gustavo Nardin
## Last Change.: 03/25/2016
##
library(data.table)
library(deSolve)
library(foreach)
library(ggplot2)
library(doParallel)
registerDoParallel(cores=2)


##############
## Calculates i switch
##############
calc_iswitch <- function(h, bs, rho, g, l, k, payoffs){
  
  iswitch <- NULL
  iState <- NULL
  pI <- 0
  pUs <- NULL
  pUp <- NULL
  n <- 0
  
  for(i in seq(0.0001, 1.0, 0.0001)){
    p <- i^(1/k) * bs
    Tss <- (1 - ((1 - p)^h)) / p
    if (p != g){
      Tis <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - (((1 - p)^h) / g)
    } else {
      Tis <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - (((1 - p)^h) / g)
    }
    Trs <- h - Tss - Tis
    
    if (l == 0){
      Us <- (payoffs[1] * Tss) + (payoffs[3] * Tis) + (payoffs[4] * Trs)
    } else {
      ts <- seq(0, Tss, 0.001)
      ds <- sum(exp(-l * ts)) / length(ts)
      
      ti <- seq(Tss, Tss + Tis, 0.001)
      di <- sum(exp(-l * ti)) / length(ti)
      
      tr <- seq(Tss + Tis, Tss + Tis + Trs, 0.001)
      dr <- sum(exp(-l * tr)) / length(tr)
      
      Us <- (ds * payoffs[1] * Tss) + (di * payoffs[3] * Tis) + (dr * payoffs[4] * Trs)
    }
    
    p <- i^(1/k) * bs * rho
    Tpp <- (1 - ((1 - p)^h)) / p
    if (p != g){
      Tip <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - (((1 - p)^h) / g)
    } else {
      Tip <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - (((1 - p)^h) / g)
    }
    Trp <- h - Tpp - Tip
    
    if (l == 0){
      Up <- (payoffs[2] * Tpp) + (payoffs[3] * Tip) + (payoffs[4] * Trp)
    } else {
      tp <- seq(0, Tpp, 0.001)
      dp <- sum(exp(-l * tp)) / length(tp)
      
      ti <- seq(Tpp, Tpp + Tip, 0.001)
      di <- sum(exp(-l * ti)) / length(ti)
      
      tr <- seq(Tpp + Tip, Tpp + Tip + Trp, 0.001)
      dr <- sum(exp(-l * tr)) / length(tr)
      
      Up <- (dp * payoffs[2] * Tpp) + (di * payoffs[3] * Tip) + (dr * payoffs[4] * Trp)
    }
    
    if (is.null(pUs)){
      pUs <- Us
    }
    
    if (is.null(pUp)){
      pUp <- Up
    }
    
    if (is.null(iState)){
      if (Us > Up){
        # Susceptible
        iState <- 0
      } else {
        # Prophylactic
        iState <- 1
      }
    }
    
    if (((Us >= Up) && (pUs < pUp)) || ((Up >= Us) && (pUp < pUs))) {
      iswitch <- rbind(iswitch, cbind(h, bs, rho, g, l, k, pI, i, iState, n))
      
      if ((Us >= Up) && (pUs < pUp)){
        # Susceptible
        iState <- 0
      } else if ((Up >= Us) && (pUp < pUs)){
        # Prophylactic
        iState <- 1
      }
      
      pI <- i
      pUs <- Us
      pUp <- Up
      n <- n + 1
    }
  }
  
  i <- 1
  iswitch <- rbind(iswitch, cbind(h, bs, rho, g, l, k, pI, i, iState, n))
  
  return(iswitch)
}


##
## SPIR ODE model
##
SPIRmodel <- function(Time, State, Pars){
  with(as.list(c(State, Pars)),{
    
    i <- (I / (S+P+I+R))
    for (index in 1:nrow(iswitch)){
      if ((i >= iswitch[index,7]) && (i <= iswitch[index,8])){
        W <- iswitch[index,9]
        break
      }
    }

    dS <- (-Bs * i * S) - (delta * W * S) + (delta * (1 - W) * P)
    dP <- (-Bs * rho * i * P) + (delta * W * S) - (delta * (1 - W) * P)
    dI <- (Bs * i * S) + (Bs * rho * i * P) - (G * I)
    dR <- G * I
    
    return(list(c(dS,dP,dI,dR)))
  })
}

##
## Input parameters
##
pars <- list(
  R0 <- 2,
  duration <- 65,
  G <- 1 / duration,
  Bs <- R0 / duration,
  bs <- 1 - exp(-Bs),
  rho <- 0.10,
  gamma <- 1 - exp(-G),
  lambda <- 0,
  kappa <- 5,
  delta <- 0.01,
  h <- 10,
  payoffs <- c(1.00, 0.95, 0.10, 0.95),
  iswitch <- calc_iswitch(h, bs, rho, gamma, lambda, kappa, payoffs)
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
isps <- iswitch[iswitch[,8] != 1,8]
plot(I / (S+P+I+R) ~ time, out, type="l", col="red",
     ylim=c(0, min(1, max(isps, max(I / (S+P+I+R))) + 0.01)),
     main=c("SPIR Behavioral Decision ODE Model"),
     xlab=c("Time"), ylab=c("Proportion of Infected"))

for(isp in isps){
  lines(rep(isp, length(times)) ~ times, type="l", lty="dashed")
}

##
## Generic iSwitch analysis of time horizon
##
H <- seq(3, 365)

iSwitch <- foreach(h=H, .combine=rbind) %dopar%
  calc_iswitch(h, bs, rho, gamma, lambda, kappa, payoffs)

data <- data.table(iSwitch)

ggplot(data, aes(x=h, y=i, group=n, color=factor(n))) +
  xlab("Planning Horizon") +
  ylab("Proportion of Infected") +
  geom_line() +
  geom_ribbon(data=subset(data,n==0 | n==2),
              aes(x=h, ymax=i, ymin=pI), fill="black") +
  geom_ribbon(data=subset(data,n==1),
              aes(x=h, ymax=i, ymin=pI), fill="blue") +
  scale_color_manual(values=c("black","blue", "black")) +
  theme(axis.title.x = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.title.y = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.text.x = element_text(colour = 'black', size = 12, face = 'bold'),
        axis.text.y = element_text(colour = 'black', size = 12, face = 'bold'),
        axis.line = element_line(colour = 'black', size = 1.5, linetype = 'solid'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none")


## Generate Graphics
for (th in c(3, 5, 10, 20, 50, 100, 200, 365)){
  for (k in c(1, 2, 3, 4, 5, 10, 15)){
    for (d in c(1, 1/7, 1/30, 1/365)){
      pars <- list(
        R0 <- 2,
        duration <- 65,
        G <- 1 / duration,
        Bs <- R0 / duration,
        bs <- 1 - exp(-Bs),
        rho <- 0.25,
        gamma <- 1 - exp(-G),
        lambda <- 0,
        kappa <- k,
        delta <- d,
        h <- th,
        payoffs <- c(1.00, 0.95, 0.10, 0.95),
        iswitch <- calc_iswitch(h, bs, rho, gamma, lambda, kappa, payoffs)
      )

      yinit <- c(S = 100000 - 100, P = 0, I = 100, R = 0)
      times <- seq(1, 1500, 1)
      
      ##
      ## Solve the ODE
      ##
      out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
      
      ##
      ## Plot the proportion of infected over time.
      ## The dashed line represents the i switching point(s)
      ##
      png(paste0("/home/gnardin/", "h", th, "f", f, "d", d, ".png"))
      plot(I / (S+P+I+R) ~ time, out, type="l", col="red",
           #ylim=c(0,1),
           main=c("SPIR Behavioral Decision ODE Model"),
           xlab=c("Time"), ylab=c("Proportion of infected (i)"))
      dev.off()
    }
  }
}
