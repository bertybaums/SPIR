library(deSolve)
library(foreach)
library(doParallel)
registerDoParallel(cores=2)

##
## Calculates i switching point
##
calc_iswitch <- function(h, bs, bp, g, payoffs){
  difSP <- 10000
  iswitch <- NA
  pUs <- NULL
  pUp <- NULL
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
    
    if (((Us >= Up) && (pUs < pUp)) || ((Up >= Us) && (pUp < pUs))) {
      iswitch <- i
      pUs <- Us
      pUp <- Up
    }
  }
  
  return(iswitch)
}

##
## SPIR ODE model
##
SPIRmodel <- function(Time, State, Pars){
  with(as.list(c(State, Pars)),{
    
    i <- I / (S+P+I+R)
    if ((!is.na(iswitch)) && (i > iswitch)){
      Switch <- 1
    } else {
      Switch <- 0
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
  payoffs <- c(1, 0.99, 0, 0.95),
  bs <- 0.15,
  rho <- 0.16,
  bp <- rho * bs,
  g <- 0.05,
  decision.prob <- 0.05,
  time.horizon <- 100,
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
## The dashed line represents the i switching point
##
if (is.na(iswitch)) iswitch <- 0
plot(I / (S+P+I+R) ~ time, out, type="l", col="red",
     ylim=c(0, max(iswitch, max(I / (S+P+I+R)))),
     main=c("SPIR Behavioral Decision ODE Model"),
     xlab=c("Time"), ylab=c("Proportion infected (i)"))

lines(rep(iswitch,length(times)) ~ times, type="l", lty="dashed")

##
## Analyzing the iSwitch in relation to the time horizon
##
H <- seq(3, 1000, 1)

iSwitch <- foreach(h=H) %dopar%
  calc_iswitch(h, bs, bp, g, payoffs)

plot(H, iSwitch, type="l", col="red",
     xlim=c(min(H),max(H)), ylim=c(0,1),
     xlab="Time Horizon", ylab="i Switch")
