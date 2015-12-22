library(deSolve)

##
## SIR ODE Model
##
SIR <- function(Time, State, Pars){
  with(as.list(c(State,Pars)),{
    
    i <- I / N
    
    dS <- -1*b*i*S
    dI <- b*i*S - g*I
    dR <- g*I
    
    return(list(c(dS,dI,dR)))
  }
  )
}

stopCriteria <- 0.00001  
rootFn <- function(Time, State, Pars)
  c(ifelse(State[2] < stopCriteria, 0, 1))

eventFn <- function(Time, State, Pars) State

##
## Input parameters
##
pars <- list(
  b <- 0.02,
  g <- 0.01
)

yinit <- c(S = 100000 - 1, I = 1, R = 0)
times <- seq(0, 1000, 1)

##
## Solve the ODE
##
out <- as.data.frame(lsoda(yinit, times, SIR, pars,
                           rtol=1e-3, atol=1e-3,
                           rootfunc = rootFn,
                           events = list(root = TRUE, terminalroot = 1)))

##
## Plot the proportion of infected over time.
##
plot(I / (S+I+R) ~ time, out, type="l", col="red",
     xlab="Time", ylab="Proportion infected (i)")
