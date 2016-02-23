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
      iswitch <- rbind(iswitch, cbind(h, bs, rho, g, pI, i, iState, n))
      
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
## i Switch analysis of specific diseases
##

##
## Seasonal Flu
##

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.95, 0.60, 1.00)

# Infection probability (Susceptible)
bs <- 1 - exp(-1.25 / 4)

# Prophylactic protection
Rho <- seq(0.01, 1.00, 0.01)

# Recover probability
g <- 1 - exp(-1 / 4)

# Range of Time Horizon to evaluate
H <- seq(3, 365)

# File name
filename <- "fluseasonal.csv"


##
## Pandemic Flu 1918
##

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.90, 0.40, 1.00)

# Infection probability (Susceptible)
bs <- 1 - exp(-3.00 / 4)

# Prophylactic protection
Rho <- seq(0.01, 1.00, 0.01)

# Recover probability
g <- 1 - exp(-1 / 4)

# Range of Time Horizon to evaluate
H <- seq(3, 365)

# File name
filename <- "flu1918.csv"


##
## Avian Flu
##

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.95, 0.60, 1.00)

# Infection probability (Susceptible)
bs <- 1 - exp(-2.00 / 8)

# Prophylactic protection
Rho <- seq(0.01, 1.00, 0.01)

# Recover probability
g <- 1 - exp(-1 / 8)

# Range of Time Horizon to evaluate
H <- seq(3, 365)

# File name
filename <- "fluavian.csv"


##
## Avian Flu
##

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.95, 0.60, 0.95)

# Infection probability (Susceptible)
bs <- 1 - exp(-2.00 / 8)

# Prophylactic protection
Rho <- seq(0.01, 1.00, 0.01)

# Recover probability
g <- 1 - exp(-1 / 8)

# Range of Time Horizon to evaluate
H <- seq(3, 365)

# File name
filename <- "fluavian-95.csv"


##
## Ebola (3, 365)
##

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.95, 0.10, 0.95)

# Infection probability (Susceptible)
bs <- 1 - exp(-2 / 65)

# Prophylactic protection
Rho <- seq(0.01, 1.00, 0.01)

# Recover probability
g <- 1 - exp(-1 / 65)

# Range of Time Horizon to evaluate
H <- seq(3, 365)

# File name
filename <- "ebola.csv"


##
## Ebola (3, 1000)
##

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.95, 0.10, 0.95)

# Infection probability (Susceptible)
bs <- 1 - exp(-2 / 65)

# Prophylactic protection
Rho <- seq(0.01, 1.00, 0.01)

# Recover probability
g <- 1 - exp(-1 / 65)

# Range of Time Horizon to evaluate
H <- seq(3, 1000)

# File name
filename <- "ebola-1000.csv"


##
## Calculating i Switches
##
iSwitch <- foreach(h=H, .combine=rbind) %:%
  foreach(rho=Rho, .combine=rbind) %dopar%
    calc_iswitch(h, bs, rho, g, payoffs)

data <- data.table(iSwitch)
colnames(data) <- c("h", "bs", "rho", "g", "pI", "i", "iState", "n")
write.table(data, file=filename, quote=FALSE, sep=";", col.names=TRUE)


##
## Graphic
##
data <- data.table(read.table(paste0("/data/projects/current/cmci/project-3/sub-projects/spir/",
                                     filename), sep=";"))

maxrho <- 1
pData <- data[which(((i < 1) & (iState == 0) | (iState == 2)))]

ggplot(pData, aes(h, rho, fill=i)) +
  geom_tile() +
  xlab("H") + ylab("+ rho -") +
  xlim(0,max(H)) + ylim(0, 1) +
  scale_fill_gradientn(name = "Proportion\nof Infected", limits = c(0, 1),
                       colours=rainbow(100, start=1, end=0.65)) +
  theme(axis.title.x = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.title.y = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.text.x = element_text(colour = 'black', size = 12, face = 'bold'),
        axis.text.y = element_text(colour = 'black', size = 12, face = 'bold'),
        axis.line = element_line(colour = 'black', size = 1.5, linetype = 'solid'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))
