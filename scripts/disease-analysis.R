##
## Heat map of the switching decision
## of some specific diseases
##
## Author......: Luis Gustavo Nardin
## Last Change.: 07/13/2016
##
library(data.table)
library(foreach)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(doParallel)
registerDoParallel(cores=3)


setwd("/data/workspace/cmci/SPIR/scripts/")

baseDir <- "/data/projects/current/cmci/socialepi/sub-projects/spir/"
outputDir <- "/data/projects/current/cmci/socialepi/sub-projects/spir/figures"

###############
## FUNCTIONS
###############
source("calcSwitch.R")
source("calcUtilities.R")


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
H <- seq(1, 500)

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
H <- seq(1, 500)

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
H <- seq(1, 500)

# File name
filename <- "fluavian.csv"


##
## Ebola (1, 500)
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
k <- 1.5

# Range of Time Horizon to evaluate
H <- seq(1, 500)

# File name
filename <- "ebola-k1.5.csv"


##############
## Calculating i Switches Rho x H
##############
iSwitch <- foreach(h=H, .combine=rbind) %:%
  foreach(rho=Rho, .combine=rbind) %dopar%
    calc_iswitch(h, bs, rho, g, l, k, payoffs)

data <- data.table(iSwitch)
colnames(data) <- c("h", "bs", "rho", "g", "lambda", "kappa", "pI", "i", "iState", "n")
write.table(data, file=paste0(outputDir, "/", filename),
            quote=FALSE, sep=";", col.names=TRUE)


##############
## Plots Heat Avian Flu
##############
values <- c(seq(0.00, 0.04, 0.01), seq(0.05, 0.90, 0.05), seq(0.91, 1.00, 0.01))
plots <- list()
saveG <- FALSE
for(purs in values){
  filename <- "fluavian-"
  data <- data.table(read.table(paste0(baseDir, "/fluavian/", filename, purs, ".csv"),
                                quote=FALSE, sep=";", header=TRUE))
  
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
    ggsave(paste0(baseDir, "/fluavian/", filename, purs, ".png"),
           plot=pl, dpi=600)
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

ggsave(paste0(baseDir, "/fluavian/fluavian-all.png"),
       plot=pl, width=50, height=30, units="cm")


## Single plot
filename <- "fluavian-d0.1"
data <- data.table(read.table(paste0(baseDir, "/fluavian/", filename,".csv"),
                              quote=FALSE, sep=";", header=TRUE))

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
  data <- data.table(read.table(paste0(baseDir, "/ebola/", filename, purs, ".csv"),
                                quote=FALSE, sep=";", header=TRUE))
  
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
    ggsave(paste0(baseDir, "/ebola/", filename, purs, ".png"),
           plot=pl, dpi=600)
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

ggsave(paste0(baseDir, "/ebola/ebola-all.png"),
       plot=pl, width=50, height=30, units="cm")


## Single plot
filename <- "ebola-k1.5"
data <- data.table(read.table(paste0(baseDir, "ebola/", filename,".csv"),
                              sep=";", header=TRUE))

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

rho <- 0.25
h <- 40

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

ggsave(paste0(baseDir, "/fear.pdf"), plot=pl)