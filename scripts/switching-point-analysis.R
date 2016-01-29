library(data.table)
library(ggplot2)

##
## Input parameters
##
# Payoffs (S, P, I, R)
payoffs <- c(1, 0.99, 0, 1)

# Infection probability (Susceptible)
bs <- 0.15

# Prophylactic protection
rho <- 0.16

# Infection probability (Prophylactic)
bp <- rho * bs

# Recover probability
g <- 0.05

# Range of Time Horizon to evaluate
H <- seq(3,1000)

# Range of Proportion of infected to evaluate
I <- seq(0.0001,1.00,0.0001)


rws <- length(H) * length(I)
drw <- 1
data <- matrix(data=0, nrow=rws, ncol=6)

erw <- 1
error <- matrix(data=0, nrow=length(H), ncol=4)
for(h in H){
  ioswitch <- NA
  iaswitch <- NA
  poUs <- NULL
  paUs <- NULL
  poUp <- NULL
  paUp <- NULL
  for(i in I){
    p <- i * bs
    oTss <- (1 - ((1 - p)^h)) / p
    aTss <- (1 - (1 - (p*h)))
    if (p != g){
      oTis <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - (((1 - p)^h) / g)
      aTis <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - ((1 - (p*h)) / g)
    } else {
      oTis <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - (((1 - p)^h) / g)
      aTis <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - ((1 - (p*h)) / g)
    }
    oTrs <- h - oTss - oTis
    aTrs <- h - aTss - aTis
    oUs <- (payoffs[1] * oTss) + (payoffs[3] * oTis) + (payoffs[4] * oTrs)
    aUs <- (payoffs[1] * aTss) + (payoffs[3] * aTis) + (payoffs[4] * aTrs)
    
    p <- i * bp
    oTpp <- (1 - ((1 - p)^h)) / p
    aTpp <- (1 - (1 - (p*h))) / p
    if (p != g){
      oTip <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - (((1 - p)^h) / g)
      aTip <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - ((1 - (p*h)) / g)
    } else {
      oTip <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - (((1 - p)^h) / g)
      aTip <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - ((1 - (p*h)) / g)
    }
    oTrp <- h - oTpp - oTip
    aTrp <- h - aTpp - aTip
    oUp <- (payoffs[2] * oTpp) + (payoffs[3] * oTip) + (payoffs[4] * oTrp)
    aUp <- (payoffs[2] * aTpp) + (payoffs[3] * aTip) + (payoffs[4] * aTrp)
    
    data[drw,] <- c(h, i, oUs, oUp, aUs, aUp)
    drw <- drw + 1
    
    if(is.null(poUs)){
      poUs <- oUs
    }
    
    if(is.null(paUs)){
      paUs <- aUs
    }
    
    if(is.null(poUp)){
      poUp <- oUp
    }
    
    if(is.null(paUp)){
      paUp <- aUp
    }
    
    if (is.na(ioswitch) && (((oUs >= oUp) && (poUs < poUp)) || ((oUp >= oUs) && (poUp < poUs)))) {
      ioswitch <- i
      poUs <- oUs
      poUp <- oUp
    }
    
    if (is.na(iaswitch) && (((aUs >= aUp) && (paUs < paUp)) || ((aUp >= aUs) && (paUp < paUs)))) {
      iaswitch <- i
      paUs <- aUs
      paUp <- aUp
    }
  }
  
  error[erw,] <- c(h, ioswitch, iaswitch, abs(ioswitch - iaswitch) / ioswitch)
  erw <- erw + 1
}

##
## Plot the error between the correct and approximated switching point
##
error <- data.table(error)
colnames(error) <- c("h","ioswitch","iaswitch","value")

ggplot(na.omit(error), aes(x=h, y=value*100))+
  xlab("Time Horizon (H)") +
  ylab("Error (%)") +
  geom_line() +
  theme(axis.title.x = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.title.y = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.text.x = element_text(colour = 'black', size = 12, face = 'bold'),
        axis.text.y = element_text(colour = 'black', size = 12, face = 'bold'),
        axis.line = element_line(colour = 'black', size = 1.5, linetype = 'solid'),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=12, face="bold"))

##
## Plot Expected Utility of Susceptible and Prophylactic for a specific H
##
inputH <- 200

data <- data.table(data)
colnames(data) <- c("h","i","oUS","oUP","aUS","aUP")
mdata <- melt(data, id=c("h","i"))

ggplot(mdata[h == inputH], aes(x=i, y=value, colour=variable)) +
  xlab("Proportion infected (i)") +
  ylab("Expected Utilities value") +
  geom_line() +
  scale_colour_discrete(name="Expected Utility",
                        l=30,
                        breaks=c("oUS", "oUP", "aUS", "aUP"),
                        labels=c("Correct Susceptible", "Correct Prophylactic",
                                 "Aprox Susceptible", "Aprox Prophylactic")) +
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