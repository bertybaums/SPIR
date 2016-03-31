##
## Heat map of the switching decision
## of some specific diseases
##
## Author......: Luis Gustavo Nardin
## Last Change.: 03/31/2016
##
library(data.table)
library(foreach)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(doParallel)
registerDoParallel(cores=2)


##############
## Calculates i switch for the perceived i
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
      ds <- (((-1/l)*exp(-l * Tss)) - (-1/l))
      di <- (((-1/l)*exp(-l * (Tss+Tis))) - ((-1/l)*exp(-l * Tss)))
      dr <- (((-1/l)*exp(-l * (Tss+Tis+Trs))) - ((-1/l)*exp(-l * (Tss+Tis))))
      
      Us <- (ds * payoffs[1]) + (di * payoffs[3]) + (dr * payoffs[4])
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
      dp <- (((-1/l)*exp(-l * Tpp)) - (-1/l))
      di <- (((-1/l)*exp(-l * (Tpp+Tip))) - ((-1/l)*exp(-l * Tpp)))
      dr <- (((-1/l)*exp(-l * (Tpp+Tip+Trp))) - ((-1/l)*exp(-l * (Tpp+Tip))))
      
      Up <- (dp * payoffs[2]) + (di * payoffs[3]) + (dr * payoffs[4])
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


##############
## Calculates utilities
##############
calc_utilities <- function(h, bs, rho, g, l, k, payoffs){
  
  size <- length(seq(0.0001,1,0.0001))
  u <- matrix(0.0, nrow=size, ncol=9)
  
  for(i in seq(0.0001,1.0,0.0001)){
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
      ds <- (((-1/l)*exp(-l * Tss)) - (-1/l))
      di <- (((-1/l)*exp(-l * (Tss+Tis))) - ((-1/l)*exp(-l * Tss)))
      dr <- (((-1/l)*exp(-l * (Tss+Tis+Trs))) - ((-1/l)*exp(-l * (Tss+Tis))))
      
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
      dp <- (((-1/l)*exp(-l * Tpp)) - (-1/l))
      di <- (((-1/l)*exp(-l * (Tpp+Tip))) - ((-1/l)*exp(-l * Tpp)))
      dr <- (((-1/l)*exp(-l * (Tpp+Tip+Trp))) - ((-1/l)*exp(-l * (Tpp+Tip))))
      
      Up <- (dp * payoffs[2] * Tpp) + (di * payoffs[3] * Tip) + (dr * payoffs[4] * Trp)
    }
    
    u[i * size,] <- c(i, Tss, Tis, Trs, Us, Tpp, Tip, Trp, Up)
  }
  
  data <- rbind(cbind("S", u[,1], u[,2], u[,3], u[,4], u[,5]),
                cbind("P", u[,1], u[,6], u[,7], u[,8], u[,9]))
  data <- data.table(data)
  colnames(data) <- c("state", "i", "Tsp", "Ti", "Tr", "U")
  
  return(data)
}


##############
## Parameters of Specific diseases
##############

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

# Discount factor (0 = No discount)
l <- 0

# Fear factor (1 = No fear)
k <- 1

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

# Discount factor (0 = No discount)
l <- 0

# Fear factor (1 = No fear)
k <- 1

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

# Discount factor (0 = No discount)
l <- 0

# Fear factor (1 = No fear)
k <- 1

# Range of Time Horizon to evaluate
H <- seq(3, 365)

# File name
filename <- "fluavian.csv"


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

# Discount factor (0 = No discount)
l <- 0

# Fear factor (1 = No fear)
k <- 1

# Range of Time Horizon to evaluate
H <- seq(3, 365)

# File name
filename <- "ebola.csv"


##############
## Calculating i Switches Rho x H
##############
iSwitch <- foreach(h=H, .combine=rbind) %:%
  foreach(rho=Rho, .combine=rbind) %dopar%
    calc_iswitch(h, bs, rho, g, l, k, payoffs)

data <- data.table(iSwitch)
colnames(data) <- c("h", "bs", "rho", "g", "lambda", "kappa", "pI", "i", "iState", "n")
write.table(data, file=filename, quote=FALSE, sep=";", col.names=TRUE)


##############
## Plots Heat Avian Flu
##############
values <- c(seq(0.00, 0.04, 0.01), seq(0.05, 0.90, 0.05), seq(0.91, 1.00, 0.01))
plots <- list()
saveG <- FALSE
for(purs in values){
  filename <- "fluavian-"
  data <- data.table(read.table(paste0("/data/projects/current/cmci/project-3/sub-projects/spir/fluavian/",
                                       filename,purs,".csv"), sep=";", header=TRUE))
  
  maxh <- max(H)
  pData <- data[which((h <= maxh))]
  
  ymin <- min(pData[which(n == 1 & h == 365 & i^(k) < 1)]$rho)
  ymax <- max(pData[which(n == 1 & h == 365 & i^(k) < 1)]$rho)
  
  pl <- ggplot(pData[which((n == 0) & (i^(k) < 1))],
               aes(h, rho, fill=i^(k))) +
    xlab("Planning Horizon") + ylab("rho") +
    xlim(0,maxh+20) + ylim(0, 1) +
    geom_tile() +
    geom_contour(data=pData[which(n == 1 & i^(k) < 1)],
                 aes(x=h, y=rho, z=(i^(k) - pI^(k))),
                 breaks=c(0.1, 0.3, 0.5, 0.7, 0.9),
                 color="black",
                 linetype="solid",
                 size=0.5) +
    annotate("text", x=375, y=ymin-0.01, label="1.0", fontface="bold") +
    annotate("text", x=375, y=ymax+0.01, label="0.0", fontface="bold") +
    geom_line(data=pData[which(n == 2)],
              alpha=0.05,
              size=1) +
    scale_fill_gradientn(name = "Proportion\nof Infected", limits = c(0, 1),
                         values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                         colours = c("red", "yellow", "green", "blue")) +
    theme(axis.title.x = element_text(colour='black', size=20, face='bold'),
          axis.title.y = element_text(colour='black', size=20, face='bold'),
          axis.text.x = element_text(colour='black', size=18, face='bold'),
          axis.text.y = element_text(colour='black', size=18, face='bold'),
          axis.line = element_line(colour='black', size=1.5, linetype='solid'),
          panel.background = element_rect(fill="transparent", colour=NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "none",
          legend.title = element_text(colour="black", size=14, face="bold"),
          legend.text = element_text(colour="black", size=12, face="bold"))
  
  if (saveG){
    ggsave(paste0("/data/projects/current/cmci/project-3/sub-projects/spir/fluavian/",
                  filename,purs,".png"), plot=pl, dpi=600)
  }
  
  plots[[length(plots)+1]] <- pl
}

pl <- grid.arrange(plots[[1]], plots[[2]], plots[[3]],
                   plots[[4]], plots[[5]], plots[[6]],
                   plots[[7]], plots[[8]], plots[[9]],
                   plots[[10]], plots[[11]], plots[[12]],
                   plots[[13]], plots[[14]], plots[[15]],
                   plots[[16]], plots[[17]], plots[[18]],
                   plots[[19]], plots[[20]], plots[[21]],
                   plots[[22]], plots[[23]], plots[[24]],
                   plots[[25]], plots[[26]], plots[[27]],
                   plots[[28]], plots[[29]], plots[[30]],
                   plots[[31]], plots[[32]], plots[[33]], ncol=6, nrow=6)

ggsave("/data/projects/current/cmci/project-3/sub-projects/spir/fluavian/fluavian-all.png",
       plot=pl, width=50, height=30, units="cm")


## Single plot
filename <- "fluavian-d0.0002"
data <- data.table(read.table(paste0("/data/projects/current/cmci/project-3/sub-projects/spir/fluavian/",
                                     filename,".csv"), sep=";", header=TRUE))

maxh <- max(H)
pData <- data[which((h <= maxh))]

ggplot(pData[which((n == 0) & (i < 1))],
       aes(h, rho, fill=i)) +
  xlab("Planning Horizon") + ylab("rho") +
  xlim(0,maxh+20) + ylim(0, 1) +
  geom_tile() +
  geom_contour(data=pData[which(n == 1 & i < 1)],
               aes(x=h, y=rho, z=(i - pI)),
               breaks=c(0.1,0.3,0.5,0.7,0.9),
               color="black",
               linetype="solid",
               size=0.5) +
  geom_line(data=pData[which(n == 2)],
            alpha=0.05,
            size=1) +
  scale_fill_gradientn(name = "Proportion\nof Infected", limits = c(0, 1),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colours = c("red", "yellow", "green", "blue")) +
  theme(axis.title.x = element_text(colour='black', size=20, face='bold'),
        axis.title.y = element_text(colour='black', size=20, face='bold'),
        axis.text.x = element_text(colour='black', size=18, face='bold'),
        axis.text.y = element_text(colour='black', size=18, face='bold'),
        axis.line = element_line(colour='black', size=1.5, linetype='solid'),
        panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))


##############
## Plots Heat Ebola
##############
values <- c(seq(0.00, 0.04, 0.01), seq(0.05, 0.90, 0.05), seq(0.91, 1.00, 0.01))
plots <- list()
saveG <- FALSE
for(purs in values){
  filename <- "ebola-"
  data <- data.table(read.table(paste0("/data/projects/current/cmci/project-3/sub-projects/spir/ebola/",
                                       filename,purs,".csv"), sep=";", header=TRUE))
  
  maxh <- max(H)
  pData <- data[which((h <= maxh))]
  
  ymin <- min(pData[which(n == 1 & h == 365 & i^(k) < 1)]$rho)
  ymax <- max(pData[which(n == 1 & h == 365 & i^(k) < 1)]$rho)
  
  pl <- ggplot(pData[which((n == 0) & (i^(k) < 1))],
               aes(h, rho, fill=i^(k))) +
    xlab("Planning Horizon") + ylab("rho") +
    xlim(0, maxh+20) + ylim(0, 1) +
    geom_tile() +
    geom_contour(data=pData[which(n == 1 & i^(k) < 1)],
                 aes(x=h, y=rho, z=(i^(k) - pI^(k))),
                 breaks=c(0.1, 0.3, 0.5, 0.7, 0.9),
                 color="black",
                 linetype="solid",
                 size=0.5) +
    annotate("text", x=375, y=ymin-0.01, label="1.0", fontface="bold") +
    annotate("text", x=375, y=ymax+0.01, label="0.0", fontface="bold") +
    geom_line(data=pData[which(n == 2)],
              alpha=0.05,
              size=1) +
    scale_fill_gradientn(name = "Proportion\nof Infected", limits = c(0, 1),
                         values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                         colours = c("red", "yellow", "green", "blue")) +
    theme(axis.title.x = element_text(colour='black', size=20, face='bold'),
          axis.title.y = element_text(colour='black', size=20, face='bold'),
          axis.text.x = element_text(colour='black', size=18, face='bold'),
          axis.text.y = element_text(colour='black', size=18, face='bold'),
          axis.line = element_line(colour='black', size=1.5, linetype='solid'),
          panel.background = element_rect(fill="transparent", colour=NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "none",
          legend.title = element_text(colour="black", size=14, face="bold"),
          legend.text = element_text(colour="black", size=12, face="bold"))
  
  if (saveG){
    ggsave(paste0("/data/projects/current/cmci/project-3/sub-projects/spir/ebola/",
                  filename,purs,".png"), plot=pl, dpi=600)
  }
  
  plots[[length(plots)+1]] <- pl
}

pl <- grid.arrange(plots[[1]], plots[[2]], plots[[3]],
                   plots[[4]], plots[[5]], plots[[6]],
                   plots[[7]], plots[[8]], plots[[9]],
                   plots[[10]], plots[[11]], plots[[12]],
                   plots[[13]], plots[[14]], plots[[15]],
                   plots[[16]], plots[[17]], plots[[18]],
                   plots[[19]], plots[[20]], plots[[21]],
                   plots[[22]], plots[[23]], plots[[24]],
                   plots[[25]], plots[[26]], plots[[27]],
                   plots[[28]], plots[[29]], plots[[30]],
                   plots[[31]], plots[[32]], plots[[33]], ncol=6, nrow=6)

ggsave("/data/projects/current/cmci/project-3/sub-projects/spir/ebola/ebola-all.png",
       plot=pl, width=50, height=30, units="cm")


## Single plot
filename <- "ebola-d0.01"
data <- data.table(read.table(paste0("/data/projects/current/cmci/project-3/sub-projects/spir/ebola/",
                                     filename,".csv"), sep=";", header=TRUE))

maxh <- max(H)
pData <- data[which((h <= maxh))]

ymin <- min(pData[which(n == 1 & h == 365) & i < 1]$rho)
ymax <- max(pData[which(n == 1 & h == 365) & i < 1]$rho)

ggplot(pData[which((n == 0) & (i < 1))],
       aes(h, rho, fill=i)) +
  xlab("Planning Horizon") + ylab("rho") +
  xlim(0,maxh+20) + ylim(0, 1) +
  geom_tile() +
  geom_contour(data=pData[which(n == 1)],
               aes(x=h, y=rho, z=i),
               breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
               color="black",
               linetype="solid",
               size=0.5) +
  annotate("text", x=375, y=ymin-0.01, label="1.0", fontface="bold") +
  annotate("text", x=375, y=ymax+0.01, label="0.0", fontface="bold") +
  geom_line(data=pData[which(n == 2)],
            alpha=0.05,
            size=1) +
  scale_fill_gradientn(name = "Proportion\nof Infected", limits = c(0, 1),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colours = c("red", "yellow", "green", "blue")) +
  theme(axis.title.x = element_text(colour='black', size=20, face='bold'),
        axis.title.y = element_text(colour='black', size=20, face='bold'),
        axis.text.x = element_text(colour='black', size=18, face='bold'),
        axis.text.y = element_text(colour='black', size=18, face='bold'),
        axis.line = element_line(colour='black', size=1.5, linetype='solid'),
        panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))


##############
## Utilities Plot
##############

rho <- 0.9
h <- 350

data <- calc_utilities(h, bs, rho, g, l, k, payoffs)

ggplot(data[which(i > 0)], aes(x=as.numeric(as.character(i)),
                               y=as.numeric(as.character(U)),
                               group=state, color=state)) +
  xlab("Proportion of Infected") + ylab("") +
  geom_point(size=0.01) +
  scale_color_manual(values=c("black", "blue")) +
  theme(axis.title.x = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.title.y = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.text.x = element_text(colour = 'black', size = 12, face = 'bold'),
        axis.text.y = element_text(colour = 'white', size = 12, face = 'bold'),
        axis.line = element_line(colour = 'black', size = 1.5, linetype = 'solid'),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))


##############
## Fear Onion Graph
##############
inormal <- seq(0, 1, 0.0001)
ks <- c(0.3, 0.7, 1.0, 1.5, 2.0, 4.0)
ichange <- matrix(0, nrow=length(ks), ncol=length(inormal))
x <- 1
for (k in ks) {
  y <- 1
  for (i in inormal){
    ichange[x,y] <- inormal[y]^(1 / k)
    y <- y + 1
  }
  x <- x + 1
}

df <- NULL
for (i in 1:length(ks)){
  df <- rbind(df, cbind(inormal, rep(ks[i], length(inormal)), ichange[i,]))
}
df <- data.table(df)
names(df) <- c("inormal", "fear", "value")

pl <- ggplot(df, aes(x=inormal, y=value, group=fear)) +
  xlab("Proportion of Infected") + ylab("Distorted Proportion of Infected") +
  geom_line(size=1) +
  geom_text(data=df[which(inormal == 0.30)], aes(label=paste0("k = ", fear)),
            size=6, fontface='bold',vjust=-0.5, hjust=0, angle=45) +
  theme(axis.title.x = element_text(colour='black', size=20, face='bold'),
        axis.title.y = element_text(colour='black', size=20, face='bold'),
        axis.text.x = element_text(colour='black', size=18, face='bold'),
        axis.text.y = element_text(colour='black', size=18, face='bold'),
        axis.line = element_line(colour='black', size=1.5, linetype='solid'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))

ggsave("/data/projects/current/cmci/project-3/sub-projects/spir/fear.pdf",
       plot=pl)