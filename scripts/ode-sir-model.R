##
## ODE SIR model
##
## Author......: Luis Gustavo Nardin
## Last Change.: 09/17/2016
##
library(deSolve)
library(ggplot2)

##
## SIR ODE Model
##
SIR <- function(Time, State, Pars){
  with(as.list(c(State, Pars)),{
    
    i <- I / (S + I + R)
    
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
  b = 0.02,
  g = 0.01
)

yinit <- c(S = 100000 - 1, I = 1, R = 0)
times <- seq(0, 10000, 1)

##
## Solve the ODE
##
out <- as.data.frame(lsoda(yinit, times, SIR, pars,
                           rtol=1e-3, atol=1e-3,
                           rootfunc = rootFn,
                           events = list(func=eventFn, root=TRUE, terminalroot=1)))

##
## Plot the proportion of infected over time.
##
ggplot(out, aes(x=time, y=(I / (S + I + R)) * 100)) +
		geom_line(size=0.9, color="red") +
		xlab("Time") +
		ylab("Proportion infected (i)")
