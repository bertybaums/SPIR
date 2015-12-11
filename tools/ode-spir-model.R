library(xtable)
library(deSolve)

N <- 100000
T <- 1000
#beta <- 0.1
#gamma <- 0.01
R0 <- 1.25        # beta = R0*gamma
gen.time <- 10    # 1/gamma
gamma <- 1/gen.time
beta <- R0*gamma

parms=c(
  R0=R0,
  gen.time=gen.time
)

stopCriteria <- 0.00001

col.exp <- "red"
col.line <- "blue"
col.log <- "green"
col.cnst <- "grey"

SIR <- function(Time, State, Pars){    #Ben's SIR function
  with(as.list(c(State,Pars)),{
    ds <- -R0/gen.time*i*s 
    di <- R0/gen.time*i*s - i/gen.time
    dr <- i/gen.time
    
    return(list(c(ds,di,dr)))
  }
  )
}
  
rootFn <- function(Time, State, Pars) c(ifelse(State[2] < stopCriteria, 0, 1))
eventFn <- function(Time, State, Pars) State

y0 <- c(s = 0.999,i = 0.001, r = 0)
timepoints <- seq(0,T,1)
out <- as.data.frame(lsoda(y0, timepoints, SIR, parms, rootfunc = rootFn, events = list(root = TRUE, terminalroot = 1)))
plot(out$time, out$i, type="l", col="black", xlab="time", ylab="Proportion infected (i)")
