library(data.table)
library(ggplot2)
library(RColorBrewer)

baseDir <- "/data/downloads/garbage/spir"
inputDir <- paste0(baseDir, "/raw/hete")
outputDir <- paste0(baseDir, "/output")

data <- NULL
for(i in 1:48){
  aux <- fread(paste0(inputDir, "/output-", i, ".csv"))
  data <- rbind(data, cbind(i, aux))
}
data <- data.table(data)

data <- data[,list(avgS=mean(susceptible), sdS=sd(susceptible),
        avgP=mean(prophylactic), sdP=sd(prophylactic),
        avgI=mean(infected), sdI=sd(infected),
        avgR=mean(recovered), sdR=sd(recovered)),
    by=list(i,time)]


## Random X Static
ggplot(data[which(i == 1 | i == 2),],
        aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
            group=as.factor(i),
            colour=as.factor(i))) +
    xlab("Time") +
    ylab("% Infectious") +
    geom_line(size=1.2) +
    scale_color_manual(name = "",
        labels = c("Random", "Static"),
        values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
    theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
            margin=margin(t=0.2, unit = "cm")),
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
        #legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))


## Random X Static X Payoff Prophylactic
colourCount = length(unique(data[which(i >= 3 & i <= 11),]$i))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(data[which(i == 1 | i == 2 | (i >= 3 & i <= 11)),],
       aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
           group=as.factor(i),
           colour=as.factor(i))) +
  xlab("Time") +
  ylab("% Infectious") +
  geom_line(size=1.2) +
  scale_color_manual(name = "Payoffs (P, I, R)",
                     labels = c("Random (0.90-1.00, 0.10, 0.95)",
                                "Static (0.95, 0.10, 0.95)",
                                "(0.90, 0.05, 0.90)", "(0.90, 0.05, 0.95)", "(0.90, 0.05, 1.00)",
                                "(0.90, 0.10, 0.90)", "(0.90, 0.10, 0.95)", "(0.90, 0.10, 1.00)",
                                "(0.90, 0.15, 0.90)", "(0.90, 0.15, 0.95)", "(0.90, 0.15, 1.00)"),
                     values = c("black", "blue", getPalette(colourCount))) +
  theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
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
        #legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))

colourCount = length(unique(data[which(i >= 12 & i <= 20),]$i))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(data[which(i == 1 | i == 2 | (i >= 12 & i <= 20)),],
        aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
            group=as.factor(i),
            colour=as.factor(i))) +
    xlab("Time") +
    ylab("% Infectious") +
    geom_line(size=1.2) +
  scale_color_manual(name = "Payoffs (P, I, R)",
                     labels = c("Random (0.90-1.00, 0.10, 0.95)",
                                "Static (0.95, 0.10, 0.95)",
                                "(0.95, 0.05, 0.90)", "(0.95, 0.05, 0.95)", "(0.95, 0.05, 1.00)",
                                "(0.95, 0.10, 0.90)", "(0.95, 0.10, 0.95)", "(0.95, 0.10, 1.00)",
                                "(0.95, 0.15, 0.90)", "(0.95, 0.15, 0.95)", "(0.95, 0.15, 1.00)"),
                     values = c("black", "blue", getPalette(colourCount))) +
    theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
            margin=margin(t=0.2, unit = "cm")),
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
        #legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))

colourCount = length(unique(data[which(i >= 21 & i <= 29),]$i))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(data[which(i == 1 | i == 2 | (i >= 21 & i <= 29)),],
       aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
           group=as.factor(i),
           colour=as.factor(i))) +
  xlab("Time") +
  ylab("% Infectious") +
  geom_line(size=1.2) +
  scale_color_manual(name = "Payoffs (P, I, R)",
                     labels = c("Random (0.90-1.00, 0.10, 0.95)",
                                "Static (0.95, 0.10, 0.95)",
                                "(0.99, 0.05, 0.90)", "(0.99, 0.05, 0.95)", "(0.99, 0.05, 1.00)",
                                "(0.99, 0.10, 0.90)", "(0.99, 0.10, 0.95)", "(0.99, 0.10, 1.00)",
                                "(0.99, 0.15, 0.90)", "(0.99, 0.15, 0.95)", "(0.99, 0.15, 1.00)"),
                     values = c("black", "blue", getPalette(colourCount))) +
  theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
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
        #legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))


## Random X Static X Rho
ggplot(data[which(i == 1 | i == 2 | (i >= 30 & i <= 34)),],
        aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
            group=as.factor(i),
            colour=as.factor(i))) +
    xlab("Time") +
    ylab("% Infectious") +
    geom_line(size=1.2) +
    scale_color_manual(name = "Protection",
        labels = c("Random 0.50-0.90", "Static 0.75", "0.9", "0.8", "0.7", "0.6", "0.5"),
        values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
    theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
            margin=margin(t=0.2, unit = "cm")),
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
        #legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))


## Random X Static X Kappa
ggplot(data[which(i == 1 | i == 2 | (i >= 35 & i <= 39)),],
        aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
            group=as.factor(i),
            colour=as.factor(i))) +
    xlab("Time") +
    ylab("% Infectious") +
    geom_line(size=1.2) +
    scale_color_manual(name = "Fear",
        labels = c("Random Normal(1, 0.2)", "Static 1", "0.90", "0.95", "1.00", "1.05", "1.10"),
        values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
    theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
            margin=margin(t=0.2, unit = "cm")),
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
        #legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))


## Random X Static X Decision Frequency
ggplot(data[which(i == 1 | i == 2 | (i >= 40 & i <= 42)),],
        aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
            group=as.factor(i),
            colour=as.factor(i))) +
    xlab("Time") +
    ylab("% Infectious") +
    geom_line(size=1.2) +
    scale_color_manual(name = "Decision\nFrequency",
        labels = c("Random 0.01-0.04", "Static 0.02", "0.01", "0.02", "0.03"),
        values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
    theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
            margin=margin(t=0.2, unit = "cm")),
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
        #legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))


## Random X Static X Planning Horizon
ggplot(data[which(i == 1 | i == 2 | (i >= 43 & i <= 48)),],
        aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
            group=as.factor(i),
            colour=as.factor(i))) +
    xlab("Time") +
    ylab("% Infectious") +
    geom_line(size=1.2) +
    scale_color_manual(name = "Planning\nHorizon",
        labels = c("Random Gamma(3, 15)", "Static 45", "1", "15", "30", "45", "90", "180"),
        values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta")) +
    theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
            margin=margin(t=0.2, unit = "cm")),
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
        #legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))
