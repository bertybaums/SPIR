##
## Disease 1 Plots
##
## Author......: Luis Gustavo Nardin
## Last Change.: 07/27/2016
##
library(colorRamps)
library(data.table)
library(deSolve)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)

setwd("/data/workspace/cmci/SPIR/scripts/")

baseDir <- "/data/projects/current/cmci/socialepi/sub-projects/spir/"
inputDisease1Dir <- paste0(baseDir, "disease1/")
outputDir <- paste0(baseDir, "figures/")

###############
## FUNCTIONS
###############
source("calcSwitch.R")
source("calcUtilities.R")
source("SPIRmodel.R")


###############
## DISEASE 1 - INPUT PARAMETERS
###############
# Disease duration
duration <- 65

# R0
R0 <- 2

# gamma
gamma <- 1 / duration

# beta
betaS <- R0 / duration

# Infection probability (Susceptible)
bs <- 1 - exp(-betaS)

# Prophylactic protection
rho <- 0.1

# Recover probability
g <- 1 - exp(-gamma)

# Discount factor (0 = No discount)
lambda <- 0

# Fear factor (1 = No fear)
kappa <- 1

# Decision frequency probability
delta <- 0

# Planning horizon
h <- 90

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.95, 0.10, 0.95)

# Initial values
yinit <- c(S = 100000 - 1, P = 0, I = 1, R = 0)

# Length of simulation
times <- seq(1, 3500, 1)


###############
## HEAT MAP
###############
filename <- "disease1-0.95"
data <- fread(paste0(inputDisease1Dir, filename,".csv"),
              sep=";", header=TRUE)

maxh <- 365
pData <- data[which((h <= maxh))]

ymin <- 1 - max(pData[which(n == 2 & h == 365 & pI >= 0.3)]$rho)
ymax <- 1 - min(pData[which(n == 2 & h == 365 & pI >= 0.9)]$rho)

pl <- ggplot(pData[which((n == 0) & (i < 1))],
             aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  xlab(expression(paste("Planning Horizon (H)"))) +
  ylab(expression(paste("% Protection (1 - ", rho, ")"))) +
  xlim(0, maxh + 20) +
  geom_raster(interpolate=TRUE, stat="identity") +
  geom_contour(data=pData[which(n == 2)],
               aes(x=h, y=(1 - rho) * 100, z=pI),
               breaks=c(0.3, 0.5, 0.7, 0.9),
               color="black",
               linetype="solid",
               size=0.5) +
  geom_line(data=pData[which(n == 2)],
            alpha=0.05,
            size=1) +
  geom_segment(aes(x = 360, y = 43, xend = 374, yend = 43),
               color="black", show.legend=FALSE) +
  annotate("text", x=380, y=ymin * 100, label="30%", fontface="bold", size=5) +
  annotate("text", x=380, y=ymax * 100, label="90%", fontface="bold", size=5) +
  annotate("text", x=300, y=10, label="A", fontface="italic", size=6) +
  annotate("text", x=150, y=65, label="B", fontface="italic", size=6) +
  annotate("text", x=380, y=43, label="C", fontface="italic", size=6) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colors = blue2green2red(50),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(color='black', size=14, face='bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color='black', size=16, face='bold',
                                    margin=margin(r=0.5, unit = "cm")),
        axis.text.x = element_text(color='black', size=16, face='bold'),
        axis.text.y = element_text(color='black', size=16, face='bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", color=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        #legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))

pl <- pl + annotation_custom(
  grob = textGrob(label = "D", hjust = 0,
                  gp = gpar(cex = 1.3, fontface="bold")),
  ymin = 108,
  ymax = 108,
  xmin = -65,
  xmax = -65)

gt <- ggplot_gtable(ggplot_build(pl))
gt$layout$clip[gt$layout$name == "panel"] <- "off"


###############
## NO SWITCHING
###############
rho <- 0.5
h <- 3

data <- data.table(calc_utilities(h, bs, rho, g, lambda, kappa, payoffs))

data$state <- factor(data$state, levels=rev(levels(as.factor(data$state))))

ymax <- max(as.numeric(as.character(data[which(i > 0)]$U)))

pu1 <- ggplot(data[which(i > 0)], aes(x=as.numeric(as.character(i)) * 100,
                                      y=as.numeric(as.character(U)),
                                      group=state,
                                      color=state,
                                      linetype=state)) +
  xlab("") + ylab(expression(paste("Utility"))) +
  geom_line(size=0.9) +
  scale_x_continuous(breaks = c(0, 50, 100),
                     labels = c("0%", "50%", "100%"),
                     limits = c(0, 110)) +
  scale_linetype_manual(name = "",
                        values = c("solid", "dashed"),
                        labels = c(expression(paste("Susceptible")),
                                   expression(paste("Prophylactic")))) +
  scale_color_manual(name = "",
                     values = c("black", "black"),
                     labels = c(expression(paste("Susceptible")),
                                expression(paste("Prophylactic")))) +
  theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 16, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))

pu1 <- pu1 + annotation_custom(
  grob = textGrob(label = "A", hjust = 0,
                  gp = gpar(cex = 1.3, fontface="bold")),
  ymin = ymax+0.02,
  ymax = ymax+0.02,
  xmin = -20,
  xmax = -20)

gt1 <- ggplot_gtable(ggplot_build(pu1))
gt1$layout$clip[gt1$layout$name == "panel"] <- "off"


###############
## ONE SWITCHING POINT
###############
rho <- 0.5
h <- 100

data <- data.table(calc_utilities(h, bs, rho, g, lambda, kappa, payoffs))

data$state <- factor(data$state, levels=rev(levels(as.factor(data$state))))

ymax <- max(as.numeric(as.character(data[which(i > 0)]$U)))

xint <- data.table(calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs))

pu2 <- ggplot(data[which(i > 0)], aes(x=as.numeric(as.character(i)) * 100,
                                      y=as.numeric(as.character(U)),
                                      group=state,
                                      color=state,
                                      linetype=state)) +
  xlab(expression(paste("% Infective (i)"))) + ylab("") +
  geom_vline(xintercept=xint[which(n == 1)]$pI * 100,
             linetype="dotted", size=1) +
  geom_line(size=0.9) +
  scale_x_continuous(breaks = c(0, 50, 100),
                     labels = c("0%", "50%", "100%"),
                     limits = c(0, 110)) +
  scale_linetype_manual(name = "",
                        values = c("solid", "dashed"),
                        labels = c(expression(paste("Susceptible")),
                                   expression(paste("Prophylactic")))) +
  scale_color_manual(name = "",
                     values = c("black", "black"),
                     labels = c(expression(paste("Susceptible")),
                                expression(paste("Prophylactic")))) +
  theme(axis.title.x = element_text(color = 'black', size = 14, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 16, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))

pu2 <- pu2 + annotation_custom(
  grob = textGrob(label = "B", hjust = 0,
                  gp = gpar(cex = 1.3, fontface="bold")),
  ymin = ymax+2.8,
  ymax = ymax+2.8,
  xmin = -20,
  xmax = -20)

gt2 <- ggplot_gtable(ggplot_build(pu2))
gt2$layout$clip[gt2$layout$name == "panel"] <- "off"


###############
## TWO SWITCHING POINTS
###############
rho <- 0.5
h <- 364

data <- data.table(calc_utilities(h, bs, rho, g, lambda, kappa, payoffs))

data$state <- factor(data$state, levels=c("S", "P"))

ymax <- max(as.numeric(as.character(data[which(i > 0)]$U)))

xint <- data.table(calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs))

pu3 <- ggplot(data[which(i > 0)], aes(x=as.numeric(as.character(i)) * 100,
                                      y=as.numeric(as.character(U)),
                                      group=state,
                                      color=state,
                                      linetype=state)) +
  xlab("") + ylab("") +
  geom_vline(xintercept=xint[which(n == 1)]$pI * 100,
             linetype="dotted", size=1) +
  geom_vline(xintercept=xint[which(n == 2)]$pI * 100,
             linetype="dotted", size=1) +
  geom_line(size=0.9) +
  scale_x_continuous(breaks = c(0, 50, 100),
                     labels = c("0%", "50%", "100%"),
                     limits = c(0, 110)) +
  scale_linetype_manual(name = "",
                        values = c("solid", "dashed"),
                        labels = c(expression(paste("Susceptible")),
                                   expression(paste("Prophylactic")))) +
  scale_color_manual(name = "",
                     values = c("black", "black"),
                     labels = c(expression(paste("Susceptible")),
                                expression(paste("Prophylactic")))) +
  theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 12, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y = element_blank(),
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

pu3 <- pu3 + annotation_custom(
  grob = textGrob(label = "C", hjust = 0,
                  gp = gpar(cex = 1.3, fontface="bold")),
  ymin = ymax+6,
  ymax = ymax+6,
  xmin = -20,
  xmax = -20)

gt3 <- ggplot_gtable(ggplot_build(pu3))
gt3$layout$clip[gt3$layout$name == "panel"] <- "off"

plot <- grid.arrange(gt1, gt2, gt3, gt, ncol=3, nrow=2,
                     layout_matrix= rbind(c(1, 2, 3),
                                          c(4, 4, 4)),
                     heights=c(1.5, 3), widths=c(0.65, 0.65, 1))

ggsave(paste0(outputDir, "disease1-heat.pdf"), plot=plot)


###############
## DYNAMICS PLANNING HORIZON
###############
times <- seq(1, 2100, 1)
rho <- 0.1
h <- 1
delta <- 0.01

iswitch <- calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs)
pars <- list(R0, duration, gamma, betaS, delta, iswitch)
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
data <- data.table(H=h, time=out$time, S=out$S, P=out$P, I=out$I, R=out$R)

h <- 30
iswitch <- calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs)
pars <- list(R0, duration, gamma, betaS, delta, iswitch)
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
data <- rbind(data, data.table(H=h, time=out$time, S=out$S, P=out$P, I=out$I, R=out$R))
isp <- data.table(H=h, i=iswitch[iswitch[,8] != 1,8])

h <- 45
iswitch <- calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs)
pars <- list(R0, duration, gamma, betaS, delta, iswitch)
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
data <- rbind(data, data.table(H=h, time=out$time, S=out$S, P=out$P, I=out$I, R=out$R))
isp <- rbind(isp, data.table(H=h, i=iswitch[iswitch[,8] != 1,8]))

h <- 90
iswitch <- calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs)
pars <- list(R0, duration, gamma, betaS, delta, iswitch)
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
data <- rbind(data, data.table(H=h, time=out$time, S=out$S, P=out$P, I=out$I, R=out$R))
isp <- rbind(isp, data.table(H=h, i=iswitch[iswitch[,8] != 1,8]))

pl <- ggplot(data, aes(x=time, y=((I / (S+P+I+R)) * 100),
                       color=as.factor(H),
                       size=as.factor(H))) +
  xlab("") +
  ylab(expression(paste("% Infective (i)"))) +
  geom_line() +
  scale_color_manual(name = expression(paste("Planning\nHorizon (H)")),
                     values = c("grey60", "blue", "red", "green")) +
  scale_size_manual(name = expression(paste("Planning\nHorizon (H)")),
                    values = c(15, 10, 5, 2)) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = c(0, 5, 10, 15, 20),
                     labels = c("0%", "5%", "10%", "15%", "20%")) +
  guides(color = guide_legend(override.aes=list(size=2))) +
  theme(axis.title.x = element_text(color='black', size=12, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(color='black', size=48, face='bold',
                                    margin=margin(r=0.5, unit = "cm")),
        axis.text.x = element_text(color='black', size=24, face='bold'),
        axis.text.y = element_text(color='black', size=24, face='bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent",color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        #legend.position = "none",
        legend.title = element_text(color="black", size=34, face="bold"),
        legend.text = element_text(color="black", size=28, face="bold"),
        legend.key = element_rect(fill = "white"),
        legend.key.height=unit(1.5,"line"))

if(nrow(isp) > 0){
  pl <- pl + geom_hline(data=isp, aes(yintercept=i*100),
                        linetype="dashed", color=c("blue","red", "green"), size=1)
}

pl <- pl + annotation_custom(
  grob = textGrob(label = "A", hjust = 0,
                  gp = gpar(cex = 4, fontface="bold")),
  ymin = 21,
  ymax = 21,
  xmin = -380,
  xmax = -380)

gth1 <- ggplot_gtable(ggplot_build(pl))
gth1$layout$clip[gth1$layout$name == "panel"] <- "off"

#ggsave(paste0(outputDir,"disease1-horizon.pdf"), plot=gth1)


###############
## DYNAMICS DECISION
###############
times <- seq(1, 2100, 1)
rho <- 0.1
h <- 90
delta <- 0
iswitch <- calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs)
pars <- list(R0, duration, gamma, betaS, delta, iswitch)
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
data <- data.table(D=delta, time=out$time, S=out$S, P=out$P, I=out$I, R=out$R)
isp <- data.table(i=iswitch[iswitch[,8] != 1,8])

delta <- 0.01
iswitch <- calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs)
pars <- list(R0, duration, gamma, betaS, delta, iswitch)
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
data <- rbind(data, data.table(D=delta, time=out$time, S=out$S, P=out$P, I=out$I, R=out$R))
isp <- data.table(i=iswitch[iswitch[,8] != 1,8])

delta <- 0.02
iswitch <- calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs)
pars <- list(R0, duration, gamma, betaS, delta, iswitch)
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
data <- rbind(data, data.table(D=delta, time=out$time, S=out$S, P=out$P, I=out$I, R=out$R))
isp <- rbind(isp, data.table(i=iswitch[iswitch[,8] != 1,8]))

pl <- ggplot(data, aes(x=time, y=((I / (S+P+I+R)) * 100),
                       color=as.factor(D),
                       size=as.factor(D))) +
  xlab("") + ylab(expression(paste("% Infective (i)"))) +
  geom_line() +
  scale_color_manual(name = expression(paste("Decision\nFrequency (d)")),
                      values = c("grey60", "blue", "red"),
                      labels = c("0.00", "0.01", "0.02")) +
  scale_size_manual(name = expression(paste("Decision\nFrequency (d)")),
                    values = c(15, 10, 5),
                    labels = c("0.00", "0.01", "0.02")) +
  scale_y_continuous(limits = c(0, 17),
                     breaks = c(0, 5, 10, 15),
                     labels = c("0%", "5%", "10%", "15%")) +
  guides(color = guide_legend(override.aes=list(size=2))) +
  theme(axis.title.x = element_text(color='black', size=12, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(color='black', size=48, face='bold',
                                    margin=margin(r=0.5, unit = "cm")),
        axis.text.x = element_text(color='black', size=24, face='bold'),
        axis.text.y = element_text(color='black', size=24, face='bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent",color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        #legend.position = "none",
        legend.title = element_text(color="black", size=34, face="bold"),
        legend.text = element_text(color="black", size=28, face="bold"),
        legend.key = element_rect(fill = "white"),
        legend.key.height=unit(1.5,"line"))

if(nrow(isp) > 0){
  pl <- pl + geom_hline(data=isp, aes(yintercept=i*100),
                        linetype="dashed", color=c("black","black"), size=1)
}

pl <- pl + annotation_custom(
  grob = textGrob(label = "A", hjust = 0,
                  gp = gpar(cex = 4, fontface="bold")),
  ymin = 18,
  ymax = 18,
  xmin = -380,
  xmax = -380)

gtd1 <- ggplot_gtable(ggplot_build(pl))
gtd1$layout$clip[gtd1$layout$name == "panel"] <- "off"

#ggsave(paste0(outputDir,"disease1-decision.pdf"), plot=gt)


###############
## FEAR
###############
filename <- "disease1-0.95"
data <- data.table(read.table(paste0(inputDisease1Dir, filename,".csv"),
                              sep=";", header=TRUE))

maxh <- 365
pData <- data[which((h <= maxh))]

ymin <- 1 - max(pData[which(n == 2 & h == 365 & pI >= 0.3)]$rho)
ymax <- 1 - min(pData[which(n == 2 & h == 365 & pI >= 0.9)]$rho)

pf1 <- ggplot(pData[which((n == 0) & (i < 1))],
             aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  xlab("") +
  ylab(expression(paste("% Protection (1 - ", rho, ")"))) +
  xlim(0, maxh + 60) +
  geom_raster(interpolate=TRUE, stat="identity") +
  geom_contour(data=pData[which(n == 2)],
               aes(x=h, y=(1 - rho) * 100, z=pI),
               breaks=c(0.3, 0.5, 0.7, 0.9),
               color="black",
               linetype="solid",
               size=0.5) +
  geom_line(data=pData[which(n == 2)],
            alpha=0.05,
            size=1) +
  annotate("text", x=410, y=ymin * 100, label="30%", fontface="bold", size=4) +
  annotate("text", x=410, y=ymax * 100, label="90%", fontface="bold", size=4) +
  annotate("text", x=190, y=65, label="kappa == 1.0", fontface="bold", size=7,
           parse=TRUE, color="white") +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colors = blue2green2red(50),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(color='black', size=12, face='bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color='black', size=12, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(color='black', size=12, face='bold'),
        axis.text.y = element_text(color='black', size=12, face='bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", color=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        legend.position = "none",
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12, face="bold"),
        legend.key = element_rect(fill = "white"))

pf1 <- pf1 + annotation_custom(
  grob = textGrob(label = "A", hjust = 0,
                  gp = gpar(cex = 1.3, fontface="bold")),
  ymin = 115,
  ymax = 115,
  xmin = -95,
  xmax = -95)

gtf1 <- ggplot_gtable(ggplot_build(pf1))
gtf1$layout$clip[gtf1$layout$name == "panel"] <- "off"


filename <- "disease1-k1.5"
data <- data.table(read.table(paste0(inputDisease1Dir, filename,".csv"),
                              sep=";", header=TRUE))

maxh <- 365
pData <- data[which((h <= maxh))]

ymin <- 1 - max(pData[which(n == 2 & h == 365 & pI >= 0.3)]$rho)
ymax <- 1 - min(pData[which(n == 2 & h == 365 & pI >= 0.9)]$rho)

pf2 <- ggplot(pData[which((n == 0) & (i < 1))],
             aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  xlab("") +
  ylab("") +
  xlim(0, maxh + 60) +
  geom_raster(interpolate=TRUE, stat="identity") +
  geom_contour(data=pData[which(n == 2)],
               aes(x=h, y=(1 - rho) * 100, z=pI),
               breaks=c(0.3, 0.5, 0.7, 0.9),
               color="black",
               linetype="solid",
               size=0.5) +
  geom_line(data=pData[which(n == 2)],
            alpha=0.05,
            size=1) +
  annotate("text", x=410, y=ymin * 100, label="30%", fontface="bold", size=4) +
  annotate("text", x=410, y=ymax * 100, label="90%", fontface="bold", size=4) +
  annotate("text", x=190, y=65, label="kappa == 1.5", fontface="bold", size=7,
           parse=TRUE, color="white") +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colors = blue2green2red(50),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(color='black', size=12, face='bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color='black', size=12, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(color='black', size=12, face='bold'),
        axis.text.y = element_text(color='black', size=12, face='bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", color=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        #legend.position = "none",
        legend.title = element_text(color="black", size=12, face="bold"),
        legend.text = element_text(color="black", size=10, face="bold"),
        legend.key = element_rect(fill = "white"))

pf2 <- pf2 + annotation_custom(
  grob = textGrob(label = "B", hjust = 0,
                  gp = gpar(cex = 1.3, fontface="bold")),
  ymin = 115,
  ymax = 115,
  xmin = -95,
  xmax = -95)

gtf2 <- ggplot_gtable(ggplot_build(pf2))
gtf2$layout$clip[gtf2$layout$name == "panel"] <- "off"


gtf <- grid.arrange(gtf1, gtf2, ncol=2, nrow=1,
                    layout_matrix= rbind(c(1, 2)),
                    heights=c(1), widths=c(0.75, 1),
                    bottom=textGrob(expression(paste("Planning Horizon (H)")),
                                    gp=gpar(fontsize=12,
                                            fontface="bold")))

rho <- 0.1
h <- 90
delta <- 0.01
kappa <- 1
iswitch <- calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs)
pars <- list(R0, duration, gamma, betaS, delta, iswitch)
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
data <- data.table(K=kappa, time=out$time, S=out$S, P=out$P, I=out$I, R=out$R)
isp <- data.table(i=iswitch[iswitch[,8] != 1,8])

h <- 90
kappa <- 1.2
iswitch <- calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs)
pars <- list(R0, duration, gamma, betaS, delta, iswitch)
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
data <- rbind(data, data.table(K=kappa, time=out$time, S=out$S, P=out$P, I=out$I, R=out$R))
isp <- rbind(isp, data.table(i=iswitch[iswitch[,8] != 1,8]))

h <- 90
kappa <- 1.5
iswitch <- calc_iswitch(h, bs, rho, g, lambda, kappa, payoffs)
pars <- list(R0, duration, gamma, betaS, delta, iswitch)
out <- as.data.frame(lsoda(yinit, times, SPIRmodel, pars, rtol=1e-3, atol=1e-3))
data <- rbind(data, data.table(K=kappa, time=out$time, S=out$S, P=out$P, I=out$I, R=out$R))
isp <- rbind(isp, data.table(i=iswitch[iswitch[,8] != 1,8]))

pl <- ggplot(data, aes(x=time, y=((I / (S+P+I+R)) * 100),
                       color=as.factor(K),
                       size=as.factor(K))) +
  xlab(expression(paste("Time (t)"))) +
  ylab(expression(paste("% Infective (i)"))) +
  geom_line() +
  scale_color_manual(name = expression(paste("Distortion of\nPerception (",kappa,")")),
                     values = c("grey60", "blue", "red"),
                     labels = c("1.0", "1.2", "1.5")) +
  scale_size_manual(name = expression(paste("Distortion of\nPerception (",kappa,")")),
                    values = c(3, 1.5, 0.75),
                    labels = c("1.0", "1.2", "1.5")) +
  scale_y_continuous(limits = c(0, 9),
                     breaks = c(0, 2.5, 5, 7.5),
                     labels = c("0%", "2.5%", "5%", "7.5%")) +
  guides(color = guide_legend(override.aes = list(size=1))) +
  theme(axis.title.x = element_text(color='black', size=12, face='bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color='black', size=12, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(color='black', size=10, face='bold'),
        axis.text.y = element_text(color='black', size=10, face='bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", color=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(1, 0, 1, 0), "cm"),
        #legend.position = "none",
        legend.title = element_text(color="black", size=12, face="bold"),
        legend.text = element_text(color="black", size=10, face="bold"),
        legend.key = element_rect(fill = "white"))

if(nrow(isp) > 0){
  pl <- pl + geom_hline(data=isp, aes(yintercept=i*100),
                        linetype="dashed", color=c("grey60", "blue", "red"),
                        size=c(1, 1, 1))
}

pl <- pl + annotation_custom(
  grob = textGrob(label = "C", hjust = 0,
                  gp = gpar(cex = 1.3, fontface="bold")),
  ymin = 10,
  ymax = 10,
  xmin = -400,
  xmax = -400)

gt <- ggplot_gtable(ggplot_build(pl))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

plot <- grid.arrange(gtf, gt, ncol=1, nrow=2,
                     layout_matrix= rbind(c(1),c(2)),
                     heights=c(1,1), widths=c(1))

ggsave(paste0(outputDir,"disease1-fear.pdf"), plot=plot)
