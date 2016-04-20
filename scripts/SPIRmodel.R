##
## SPIR ODE model
##
## Author......: Luis Gustavo Nardin
## Last Change.: 04/06/2016
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
    
    dS <- (-betaS * i * S) - (delta * W * S) + (delta * (1 - W) * P)
    dP <- (-betaS * rho * i * P) + (delta * W * S) - (delta * (1 - W) * P)
    dI <- (betaS * i * S) + (betaS * rho * i * P) - (gamma * I)
    dR <- gamma * I
    
    return(list(c(dS,dP,dI,dR)))
  })
}