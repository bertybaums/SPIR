library(data.table)
library(deSolve)
library(foreach)
library(ggplot2)
library(doParallel)
registerDoParallel(cores=2)

##
## Calculates i switching point
##
calc_iswitch <- function(h, bs, bp, g, payoffs){
  difSP <- 10000
  iswitch <- NULL
  iState <- NULL
  pI <- 0
  pUs <- NULL
  pUp <- NULL
  n <- 0
  for(i in seq(0.00001,1.0,0.00001)){
    p <- i * bs
    Tss <- (1 - ((1 - p)^h)) / p
    if (p != g){
      Tis <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - (((1 - p)^h) / g)
    } else {
      Tis <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - (((1 - p)^h) / g)
    }
    Trs <- h - Tss - Tis
    Us <- (payoffs[1] * Tss) + (payoffs[3] * Tis) + (payoffs[4] * Trs)

    p <- i * bp
    Tpp <- (1 - ((1 - p)^h)) / p
    if (p != g){
      Tip <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - (((1 - p)^h) / g)
    } else {
      Tip <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - (((1 - p)^h) / g)
    }
    Trp <- h - Tpp - Tip
    Up <- (payoffs[2] * Tpp) + (payoffs[3] * Tip) + (payoffs[4] * Trp)
    
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
      iswitch <- rbind(iswitch, cbind(h, pI, i, iState, n))
      
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
  
  iswitch <- rbind(iswitch, cbind(h, pI, 1, iState, n))
  
  return(iswitch)
}

##
## SPIR ODE model
##
SPIRmodel <- function(Time, State, Pars){
  with(as.list(c(State, Pars)),{
    
    i <- I / (S+P+I+R)
    for (index in 1:nrow(iswitch)){
      if ((i >= iswitch[index,2]) && (i <= iswitch[index,3])){
        Switch <- iswitch[index,4]
        break
      }
    }

    dS <- -bs*i*S - Switch*decision.prob*S + (1-Switch)*decision.prob*P
    dP <- -bp*i*P + Switch*decision.prob*S - (1-Switch)*decision.prob*P
    dI <- bs*i*S + bp*i*P - g*I
    dR <- g*I
    
    return(list(c(dS,dP,dI,dR)))
  })
}

##
## Input parameters
##
pars <- list(
  payoffs <- c(1, 0.99, 0, 1),
  bs <- 0.15,
  rho <- 0.16,
  bp <- rho * bs,
  g <- 0.05,
  decision.prob <- 0,
  time.horizon <- 3,
  iswitch <- calc_iswitch(time.horizon, bs, bp, g, payoffs)
)

yinit <- c(S = 100000 - 1, P = 0, I = 1, R = 0)
times <- seq(1, 1000, 1)

##
## Solve the ODE
##
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))

##
## Plot the proportion of infected over time.
## The dashed line represents the i switching point(s)
##
plot(I / (S+P+I+R) ~ time, out, type="l", col="red",
     ylim=c(0,1),
     main=c("SPIR Behavioral Decision ODE Model"),
     xlab=c("Time"), ylab=c("Proportion infected (i)"))

for (index in 1:nrow(iswitch)){
  lines(rep(iswitch[index,3],length(times)) ~ times, type="l", lty="dashed")
}

##
## Analyzing the iSwitch in relation to the time horizon
##
H <- seq(3, 2000, 1)

iSwitch <- foreach(h=H, .combine=rbind) %dopar%
  calc_iswitch(h, bs, bp, g, payoffs)

data <- data.table(iSwitch)
data <- data[which(h <= 200)]

ggplot(data, aes(x=h, y=i, group=n, color=factor(n))) +
  xlab("Time Horizon") +
  ylab("i Switch") +
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

