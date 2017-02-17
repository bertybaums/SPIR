##
## Plots heterogeneous simulation results
##
## Author......: Luis Gustavo Nardin
## Last Change.: 02/16/2017
##
library(data.table)
library(ggplot2)
library(RColorBrewer)

baseDir <- "/data/downloads/garbage/spir"
inputDir <- paste0(baseDir, "/output")
outputDir <- paste0(baseDir, "/figures")

## Combination of parameters
nComb <- 48

## 1 - Time to get infected
## 2 - Time to adopt prophylactic behavior
## 3 - Frequency of prophylactic behavior adoption
## 4 - Proportion of infected + recovered
## 5 - Accrued payoff
type <- 5

## 1 - Mean
## 2 - Standard Deviation
type <- 2

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
## Upload
##
loaded <- fread(paste0(inputDir, "/individual-sw.csv"), sep=";")

## Random X Static
data <- loaded[which((comb == 1) | (comb == 2 & !is.na(meanValue))),]
getColors <- c("black", "blue")
labs <- c("Random", "Static")
legName <- c()

## Random X Static X Payoff Prophylactic
data <- loaded[which((comb == 1) | (comb == 2 & !is.na(meanValue)) |
                       ((comb >= 3) & (comb <= 11))),]
colourCount <- length(unique(data[which(comb >= 3 & comb <= 11),]$comb))
getColors <- c("black", "blue",
               colorRampPalette(brewer.pal(9, "Set1"))(colourCount))
labs <- c("Random (0.90-1.00, 0.10, 0.95)",
          "Static (0.95, 0.10, 0.95)",
          "(0.90, 0.05, 0.90)", "(0.90, 0.05, 0.95)", "(0.90, 0.05, 1.00)",
          "(0.90, 0.10, 0.90)", "(0.90, 0.10, 0.95)", "(0.90, 0.10, 1.00)",
          "(0.90, 0.15, 0.90)", "(0.90, 0.15, 0.95)", "(0.90, 0.15, 1.00)")
legName <- "Payoffs (P, I, R)"

data <- loaded[which((comb == 1) | (comb == 2 & !is.na(meanValue)) |
                       ((comb >= 12) & (comb <= 20))),]
colourCount <- length(unique(data[which(comb >= 12 & comb <= 20),]$comb))
getColors <- c("black", "blue",
               colorRampPalette(brewer.pal(9, "Set1"))(colourCount))
labs <- c("Random (0.90-1.00, 0.10, 0.95)",
          "Static (0.95, 0.10, 0.95)",
          "(0.95, 0.05, 0.90)", "(0.95, 0.05, 0.95)", "(0.95, 0.05, 1.00)",
          "(0.95, 0.10, 0.90)", "(0.95, 0.10, 0.95)", "(0.95, 0.10, 1.00)",
          "(0.95, 0.15, 0.90)", "(0.95, 0.15, 0.95)", "(0.95, 0.15, 1.00)")
legName <- "Payoffs (P, I, R)"

data <- loaded[which((comb == 1) | (comb == 2 & !is.na(meanValue)) |
                       ((comb >= 21) & (comb <= 29))),]
colourCount <- length(unique(data[which(comb >= 21 & comb <= 29),]$comb))
getColors <- c("black", "blue",
               colorRampPalette(brewer.pal(9, "Set1"))(colourCount))
labs <- c("Random (0.90-1.00, 0.10, 0.95)",
          "Static (0.95, 0.10, 0.95)",
          "(0.99, 0.05, 0.90)", "(0.99, 0.05, 0.95)", "(0.99, 0.05, 1.00)",
          "(0.99, 0.10, 0.90)", "(0.99, 0.10, 0.95)", "(0.99, 0.10, 1.00)",
          "(0.99, 0.15, 0.90)", "(0.99, 0.15, 0.95)", "(0.99, 0.15, 1.00)")
legName <- "Payoffs (P, I, R)"

## Random X Static X Rho
data <- loaded[which((comb == 1) | (comb == 2 & !is.na(meanValue)) |
                       ((comb >= 30) & (comb <= 34))),]
getColors <- c("black", "blue", "red", "orange", "gray", "brown", "magenta", "purple")
labs <- c("Random 0.50-0.90", "Static 0.75", "0.9", "0.8", "0.7", "0.6", "0.5")
legName <- "Protection"

## Random X Static X Kappa
data <- loaded[which((comb == 1) | (comb == 2 & !is.na(meanValue)) |
                       ((comb >= 35) & (comb <= 39))),]
getColors <- c("black", "blue", "red", "orange", "gray", "brown", "magenta", "purple")
labs <- c("Random Normal(1, 0.2)", "Static 1", "0.90", "0.95", "1.00", "1.05", "1.10")
legName <- "Fear"

## Random X Static X Decision Frequency
data <- loaded[which((comb == 1) | (comb == 2 & !is.na(meanValue)) |
                       ((comb >= 40) & (comb <= 42))),]
getColors <- c("black", "blue", "red", "orange", "gray", "brown", "magenta", "purple")
labs <- c("Random 0.01-0.04", "Static 0.02", "0.01", "0.02", "0.03")
legName <- "Decision\nFrequency"

## Planning Horizon
data <- loaded[which(((comb == 1) | (comb == 2) |
                       ((comb >= 43) & (comb <= 48)) & !is.na(meanValue))),]
getColors <- c("black", "blue", "green", "red", "orange",
               "gray", "brown", "magenta")
labs <- c("Random Gamma(3, 15)", "Static 45", "1", "15", "30",
          "45", "90", "180")
legName <- "Planning\nHorizon"


## Mean time to get infected
ggplot(data[which(type == 1),],
        group=as.factor(comb)) +
    geom_point(aes(x=range, y=medianValue,
            color=as.factor(comb))) +
    geom_line(aes(x=range, y=medianValue,
            color=as.factor(comb))) +
    #geom_errorbar(aes(x=range, ymin=meanValue - sdValue,
    #                  ymax=meanValue + sdValue,
    #                  color=as.factor(comb))) +
    xlab("Switch Point range") +
    ylab("Mean time to get infected") +
    scale_color_manual(name = legName,
        labels = labs,
        values = getColors) +
    THEME

## Mean time to adopt prophylaxis
ggplot(data[which(type == 2),],
        group=as.factor(comb)) +
    geom_point(aes(x=range, y=meanValue,
            color=as.factor(comb))) +
  geom_line(aes(x=range, y=meanValue,
            color=as.factor(comb))) +
    xlab("Switch Point") +
    ylab("Mean time to adopt prophylactic behavior") +
  scale_color_manual(name = legName,
                     labels = labs,
                     values = getColors) +
  THEME

## Proportion of prophylaxis adoption
ggplot(data[which(type == 3),],
       group=as.factor(comb)) +
  geom_point(aes(x=range, y=meanValue * 100,
                 color=as.factor(comb))) +
  geom_line(aes(x=range, y=meanValue * 100,
                 color=as.factor(comb))) +
  xlab("Switch Point") +
  ylab("Proportion of Prophylaxis adoption") +
  scale_color_manual(name = legName,
                     labels = labs,
                     values = getColors) +
  THEME

## Proportion of infected or recovered individuals
ggplot(data[which(type == 4),],
       group=as.factor(comb)) +
  geom_point(aes(x=range, y=meanValue * 100,
                 color=as.factor(comb))) +
  geom_line(aes(x=range, y=meanValue * 100,
                color=as.factor(comb))) +
  xlab("Switch Point") +
  ylab("% Infected or Recovered") +
  scale_color_manual(name = legName,
                     labels = labs,
                     values = getColors) +
  THEME

## Mean accrued payoff
ggplot(data[which(type == 5),],
       group=as.factor(comb)) +
  geom_point(aes(x=range, y=meanValue,
                 color=as.factor(comb))) +
  geom_line(aes(x=range, y=meanValue,
                color=as.factor(comb))) +
  xlab("Switch Point") +
  ylab("Mean accrued payoff") +
  scale_color_manual(name = legName,
                     labels = labs,
                     values = getColors) +
  THEME
