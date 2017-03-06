##
## Plots heterogeneous simulation results
##
## Author......: Luis Gustavo Nardin
## Last Change.: 02/21/2017
##
library(data.table)
library(ggplot2)
library(RColorBrewer)

baseDir <- "/data/downloads/garbage/spir"
inputDir <- paste0(baseDir, "/output/summary")
outputDir <- paste0(baseDir, "/output/figures")

WIDTH <- 8
HEIGHT <- 5

## Combination of parameters
nComb <- 48

## 1 - Time to get infected
## 2 - Number of infected individuals
## 3 - Proportion of infected + recovered
## 4 - Time to adopt prophylactic behavior
## 5 - Number of adopter individuals
## 6 - Proportion of adopter individuals
## 7 - Accumulated payoff
types <- 7
TIME_INFECTED <- 1
NUM_INFECTED <- 2
PROP_INFECTED <- 3
TIME_ADOPTED <- 4
NUM_ADOPTED <- 5
PROP_ADOPTED <- 6
ACC_PAYOFF <- 7

## Variable
## 0 - Mean
## 1 - Median
## 2 - Standard Deviation
## 3 - Quantity

THEME <- theme(axis.title.x = element_text(color = 'black', size = 12,
        face = 'bold', margin=margin(t=0.2, unit = "cm")),
    axis.title.y = element_text(color = 'black', size = 16, face = 'bold',
        margin=margin(r=0.4, unit = "cm")),
    axis.text.x = element_text(color = 'black', size = 12, face = 'bold'),
    axis.text.y = element_text(color = 'black', size = 12, face = 'bold'),
    axis.line.x = element_line(color='black', size=1, linetype='solid'),
    axis.line.y = element_line(color='black', size=1, linetype='solid'),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
    legend.position = "bottom",
    legend.title = element_text(color="black", size=14, face="bold"),
    legend.text = element_text(color="black", size=12, face="bold"),
    legend.key = element_rect(fill = "white"))


##
## PLOTTING FUNCTION
##
plotting <- function(){
  ## Time to infection MEDIAN
  pl <- ggplot(data[which(type == 1 & var == 1 & comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value,
                  color=as.factor(comb))) +
    #geom_errorbar(aes(x=range, ymin=meanValue - sdValue,
    #                  ymax=meanValue + sdValue,
    #                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Time to infection") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/time-infected-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
  
  ## Time to infection SD
  pl <- ggplot(data[which(type == 1 & var == 2 & comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value,
                  color=as.factor(comb))) +
    #geom_errorbar(aes(x=range, ymin=meanValue - sdValue,
    #                  ymax=meanValue + sdValue,
    #                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Time to infection") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/time-infected-sd-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
  
  
  ## Number of infected individuals
  pl <- ggplot(data[which(type == 2 & var == 3 & value != 0 &
                            comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value,
                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Number of Infected") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/number-infected-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
  
  
  ## Proportion of infected individuals
  pl <- ggplot(data[which(type == 3 & var == 3 & comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value * 100,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value * 100,
                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Proportion of Infected") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/prop-infected-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
  
  
  ## Time to adoption MEDIAN
  pl <- ggplot(data[which(type == 4 & var == 1 & comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value,
                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Time of adoption") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/time-adoption-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
  
  
  ## Time to adoption SD
  pl <- ggplot(data[which(type == 4 & var == 2 & comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value,
                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Time of adoption") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/time-adoption-sd-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
  
  
  ## Number of adopters
  pl <- ggplot(data[which(type == 5 & var == 3 & value != 0 &
                            comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value,
                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Number of adopters") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/number-adoption-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
  
  
  ## Proportion of prophylaxis adoption
  pl <- ggplot(data[which(type == 6 & var == 3 & comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value * 100,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value * 100,
                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Proportion of Prophylaxis adoption") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/prop-adoption-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
  
  
  ## Accrued payoff MEDIAN
  pl <- ggplot(data[which(type == 7 & var == 1 & comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value,
                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Accrued payoff") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/accrued-payoff-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
  
  
  ## Accrued payoff SD
  pl <- ggplot(data[which(type == 7 & var == 1 & comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value,
                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Accrued payoff") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/accrued-payoff-sd-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
  
  
  ##
  ## Quantity per switching point range
  ##
  pl <- ggplot(datasw[which(var == 3 & value != 0 & comb != 2 & range <= 20),],
               group=as.factor(comb)) +
    geom_point(aes(x=range, y=value,
                   color=as.factor(comb))) +
    geom_line(aes(x=range, y=value,
                  color=as.factor(comb))) +
    xlab("Switching Point range") +
    ylab("Total number of individuals") +
    scale_color_manual(name = legName,
                       labels = labs,
                       values = getColors) +
    THEME
  ggsave(paste0(outputDir, "/numbers-", figName, ".png"),
         pl, width=WIDTH, height=HEIGHT)
}


##
## Upload
##
loaded <- fread(paste0(inputDir, "/individual-sw.csv"), sep=";")
numsw <- fread(paste0(inputDir, "/num-sw.csv"), sep=";")


## Random X Static
data <- loaded[which(((comb == 1) | (comb == 2)) & !is.na(value)),]
datasw <- numsw[which(((comb == 1) | (comb == 2))  & !is.na(value)),]
getColors <- c("black", "blue")
labs <- c("Random", "Static")
legName <- c()
figName <- "random-static"

plotting()


## Random X Static X Payoff Prophylactic
data <- loaded[which(((comb == 1) | (comb == 2) |
              ((comb >= 3) & (comb <= 11)))  & !is.na(value)),]
datasw <- numsw[which(((comb == 1) | (comb == 2) |
              ((comb >= 3) & (comb <= 11)))  & !is.na(value)),]
colourCount <- length(unique(data[which(comb >= 3 & comb <= 11),]$comb))
getColors <- c("black", "blue",
    colorRampPalette(brewer.pal(9, "Set1"))(colourCount))
labs <- c("Random (0.90-1.00, 0.10, 0.95)",
    "Static (0.95, 0.10, 0.95)",
    "(0.90, 0.05, 0.90)", "(0.90, 0.05, 0.95)", "(0.90, 0.05, 1.00)",
    "(0.90, 0.10, 0.90)", "(0.90, 0.10, 0.95)", "(0.90, 0.10, 1.00)",
    "(0.90, 0.15, 0.90)", "(0.90, 0.15, 0.95)", "(0.90, 0.15, 1.00)")
#legName <- "Payoffs (P, I, R)"
legName <- c()
figName <- "payoffP090"

plotting()


data <- loaded[which(((comb == 1) | (comb == 2) |
              ((comb >= 12) & (comb <= 20)))  & !is.na(value)),]
datasw <- numsw[which(((comb == 1) | (comb == 2) |
              ((comb >= 12) & (comb <= 20)))  & !is.na(value)),]
colourCount <- length(unique(data[which(comb >= 12 & comb <= 20),]$comb))
getColors <- c("black", "blue",
    colorRampPalette(brewer.pal(9, "Set1"))(colourCount))
labs <- c("Random (0.90-1.00, 0.10, 0.95)",
    "Static (0.95, 0.10, 0.95)",
    "(0.95, 0.05, 0.90)", "(0.95, 0.05, 0.95)", "(0.95, 0.05, 1.00)",
    "(0.95, 0.10, 0.90)", "(0.95, 0.10, 0.95)", "(0.95, 0.10, 1.00)",
    "(0.95, 0.15, 0.90)", "(0.95, 0.15, 0.95)", "(0.95, 0.15, 1.00)")
#legName <- "Payoffs (P, I, R)"
legName <- c()
figName <- "payoffP095"

plotting()


data <- loaded[which(((comb == 1) | (comb == 2) |
              ((comb >= 21) & (comb <= 29))) & !is.na(value)),]
datasw <- numsw[which(((comb == 1) | (comb == 2) |
              ((comb >= 21) & (comb <= 29))) & !is.na(value)),]
colourCount <- length(unique(data[which(comb >= 21 & comb <= 29),]$comb))
getColors <- c("black", "blue",
    colorRampPalette(brewer.pal(9, "Set1"))(colourCount))
labs <- c("Random (0.90-1.00, 0.10, 0.95)",
    "Static (0.95, 0.10, 0.95)",
    "(0.99, 0.05, 0.90)", "(0.99, 0.05, 0.95)", "(0.99, 0.05, 1.00)",
    "(0.99, 0.10, 0.90)", "(0.99, 0.10, 0.95)", "(0.99, 0.10, 1.00)",
    "(0.99, 0.15, 0.90)", "(0.99, 0.15, 0.95)", "(0.99, 0.15, 1.00)")
#legName <- "Payoffs (P, I, R)"
legName <- c()
figName <- "payoffP099"

plotting()


## Random X Static X Rho
data <- loaded[which(((comb == 1) | (comb == 2) |
              ((comb >= 30) & (comb <= 34))) & !is.na(value)),]
datasw <- numsw[which(((comb == 1) | (comb == 2) |
              ((comb >= 30) & (comb <= 34))) & !is.na(value)),]
getColors <- c("black", "blue", "red", "orange", "gray", "brown", "magenta", "purple")
labs <- c("Random 0.50-0.90", "Static 0.75", "0.9", "0.8", "0.7", "0.6", "0.5")
#legName <- "Protection"
legName <- c()
figName <- "rho"

plotting()


## Random X Static X Kappa
data <- loaded[which(((comb == 1) | (comb == 2) |
              ((comb >= 35) & (comb <= 39))) & !is.na(value)),]
datasw <- numsw[which(((comb == 1) | (comb == 2) |
              ((comb >= 35) & (comb <= 39))) & !is.na(value)),]
getColors <- c("black", "blue", "red", "orange", "gray", "brown", "magenta", "purple")
labs <- c("Random Normal(1, 0.2)", "Static 1", "0.90", "0.95", "1.00", "1.05", "1.10")
#legName <- "Fear"
legName <- c()
figName <- "kappa"

plotting()


## Random X Static X Decision Frequency
data <- loaded[which(((comb == 1) | (comb == 2) |
              ((comb >= 40) & (comb <= 42)))  & !is.na(value)),]
datasw <- numsw[which(((comb == 1) | (comb == 2) |
              ((comb >= 40) & (comb <= 42)))  & !is.na(value)),]
getColors <- c("black", "blue", "red", "orange", "gray", "brown", "magenta", "purple")
labs <- c("Random 0.01-0.04", "Static 0.02", "0.01", "0.02", "0.03")
#legName <- "Decision\nFrequency"
legName <- c()
figName <- "delta"

plotting()


## Planning Horizon
data <- loaded[which(((comb == 1) | (comb == 2) |
              ((comb >= 43) & (comb <= 48))) & !is.na(value)),]
datasw <- numsw[which(((comb == 1) | (comb == 2) |
              ((comb >= 43) & (comb <= 48))) & !is.na(value)),]
getColors <- c("black", "blue", "green", "red", "orange",
               "gray", "brown", "magenta")
labs <- c("Random Gamma(3, 15)", "Static 45", "1", "15", "30",
          "45", "90", "180")
#legName <- "Planning\nHorizon"
legName <- c()
figName <- "h"

plotting()
