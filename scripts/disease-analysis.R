library(data.table)
library(foreach)
library(ggplot2)
library(doParallel)
registerDoParallel(cores=3)

##
## Calculates i switching point
##
calc_iswitch <- function(h, bs, rho, g, payoffs){

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
    
    p <- i * bs * rho
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
      iswitch <- rbind(iswitch, cbind(h, bs, rho, g, pI, 1, iState, n))
      
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
    
  iswitch <- rbind(iswitch, cbind(h, bs, rho, g, pI, 1, iState, n))

  return(iswitch)
}


##
## Analyzing the iSwitch in relation to the time horizon for specific diseases
##

##
## Input parameters
##

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.95, 0.60, 1.00)

# Infection probability (Susceptible)
bs <- 1 - exp(-1.5 / 4)

# Prophylactic protection
Rho <- seq(0.01, 1.00, 0.1)

# Recover probability
g <- 1 - exp(-1 / 4)

# Range of Time Horizon to evaluate
H <- seq(3, 365)

##
## DO NOT FORGET TO EXCLUDE VALUES THAT RETURN NAN IN THE ISWITCH
##
iSwitch <- foreach(h=H, .combine=rbind) %:%
  foreach(rho=Rho, .combine=rbind) %dopar%
    calc_iswitch(h, bs, rho, g, payoffs)

data <- data.table(iSwitch)
colnames(data) <- c("h", "bs", "rho", "g", "pI", "i", "iState", "n")

ggplot(na.omit(data), aes(h, rho), colour=i) +


  stat_density2d(geom = "tile", contour = F, aes(fill = ..density..)) + 
  scale_fill_gradient(low = "white", high = "blue")


ggplot(data, aes(x=h, y=i, group=n, color=factor(n))) +
  xlab("Time Horizon") +
  ylab("Proportion Infected (i)") +
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
