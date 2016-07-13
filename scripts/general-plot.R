##
## General Plots
##
## Author......: Luis Gustavo Nardin
## Last Change.: 07/12/2016
##
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(doParallel)
registerDoParallel(cores=2)

setwd("/data/workspace/cmci/SPIR/scripts/")

baseDir <- "/data/projects/current/cmci/socialepi/sub-projects/spir/"
inputEbolaDir <- paste0(baseDir, "ebola/")
inputFluavianDir <- paste0(baseDir, "fluavian/")
outputDir <- paste0(baseDir, "figures/")

###############
## FUNCTIONS
###############
source("calcExpectedTime.R")


###############
## CASES
###############

xmn <- -100
xmx <- -100
ymn <- 115
ymx <- 115

###############
## CASE 1 - EBOLA
###############
filename <- "ebola-1"
data <- data.table(read.table(paste0(inputEbolaDir, filename,".csv"),
                              sep=";", header=TRUE))

maxh <- 365
pData <- data[which((h <= maxh))]

ymin <- 1 - max(pData[which(n == 2 & h == 365 & pI >= 0.3)]$rho)
ymax <- 1 - min(pData[which(n == 2 & h == 365 & pI >= 0.9)]$rho)

pce1 <- ggplot(pData[which((n == 0) & (i < 1))],
               aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  ggtitle(expression(paste("u"[S]*" = u"[R]*" > u"[P]))) +
  xlab("") +
  ylab(expression(paste("% Protection (1 - ", rho, ")"))) +
  #ylab("") +
  xlim(0, maxh + 30) +
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
  annotate("text", x=380, y=ymin * 100, label="30%", fontface="bold", size=5) +
  annotate("text", x=380, y=ymax * 100, label="90%", fontface="bold", size=5) +
  scale_y_continuous(limits=c(0, 100),
                     breaks=c(0, 25, 50, 75, 100),
                     labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colours = c("red", "yellow", "green", "blue"),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(colour='black', size=34, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(colour='black', size=34, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(colour='black', size=16, face='bold'),
        axis.text.y = element_text(colour='black', size=16, face='bold'),
        axis.line = element_line(colour='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(colour="black", size=28, face="bold"),
        legend.text = element_text(colour="black", size=24, face="bold"),
        legend.key = element_rect(fill = "white"),
        plot.margin = unit(c(1, 0, 0, 1), "cm"),
        plot.title = element_text(color="black", size=34, face="bold"))

pce1 <- pce1 + annotation_custom(
  grob = textGrob(label = "A", hjust = 0,
                  gp = gpar(cex = 3.5, fontface="bold")),
  ymin = ymn,
  ymax = ymx,
  xmin = xmn,
  xmax = xmx)

gtce1 <- ggplot_gtable(ggplot_build(pce1))
gtce1$layout$clip[gtce1$layout$name == "panel"] <- "off"


###############
## CASE 2 - EBOLA
###############
filename <- "ebola-0.97"
data <- data.table(read.table(paste0(inputEbolaDir, filename,".csv"),
                              sep=";", header=TRUE))

maxh <- 365
pData <- data[which((h <= maxh))]

ymin <- 1 - max(pData[which(n == 2 & h == 365 & pI >= 0.3)]$rho)
ymax <- 1 - min(pData[which(n == 2 & h == 365 & pI >= 0.9)]$rho)

pce2 <- ggplot(pData[which((n == 0) & (i < 1))],
               aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  ggtitle(expression(paste("u"[S]*" > u"[R]*" > u"[P]))) +
  xlab("") +
  ylab("") +
  xlim(0, maxh + 30) +
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
  annotate("text", x=380, y=ymin * 100, label="30%", fontface="bold", size=5) +
  annotate("text", x=380, y=ymax * 100, label="90%", fontface="bold", size=5) +
  scale_y_continuous(limits=c(0, 100),
                     breaks=c(0, 25, 50, 75, 100),
                     labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colours = c("red", "yellow", "green", "blue"),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(colour='black', size=34, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(colour='black', size=34, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(colour='black', size=16, face='bold'),
        axis.text.y = element_text(colour='black', size=16, face='bold'),
        axis.line = element_line(colour='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(colour="black", size=28, face="bold"),
        legend.text = element_text(colour="black", size=24, face="bold"),
        legend.key = element_rect(fill = "white"),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        plot.title = element_text(color="black", size=34, face="bold"))

pce2 <- pce2 + annotation_custom(
  grob = textGrob(label = "B", hjust = 0,
                  gp = gpar(cex = 3.5, fontface="bold")),
  ymin = ymn,
  ymax = ymx,
  xmin = xmn,
  xmax = xmx)

gtce2 <- ggplot_gtable(ggplot_build(pce2))
gtce2$layout$clip[gtce2$layout$name == "panel"] <- "off"


###############
## CASE 3 - EBOLA
###############
filename <- "ebola-0.95"
data <- data.table(read.table(paste0(inputEbolaDir, filename,".csv"),
                              sep=";", header=TRUE))

maxh <- 365
pData <- data[which((h <= maxh))]

ymin <- 1 - max(pData[which(n == 2 & h == 365 & pI >= 0.3)]$rho)
ymax <- 1 - min(pData[which(n == 2 & h == 365 & pI >= 0.9)]$rho)

pce3 <- ggplot(pData[which((n == 0) & (i < 1))],
               aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  ggtitle(expression(paste("u"[S]*" > u"[R]*" = u"[P]))) +
  xlab("") +
  ylab("") +
  xlim(0, maxh + 30) +
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
  annotate("text", x=380, y=ymin * 100, label="30%", fontface="bold", size=5) +
  annotate("text", x=380, y=ymax * 100, label="90%", fontface="bold", size=5) +
  scale_y_continuous(limits=c(0, 100),
                     breaks=c(0, 25, 50, 75, 100),
                     labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colours = c("red", "yellow", "green", "blue"),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(colour='black', size=34, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(colour='black', size=34, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(colour='black', size=16, face='bold'),
        axis.text.y = element_text(colour='black', size=16, face='bold'),
        axis.line = element_line(colour='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(colour="black", size=28, face="bold"),
        legend.text = element_text(colour="black", size=24, face="bold"),
        legend.key = element_rect(fill = "white"),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        plot.title = element_text(color="black", size=34, face="bold"))

pce3 <- pce3 + annotation_custom(
  grob = textGrob(label = "C", hjust = 0,
                  gp = gpar(cex = 3.5, fontface="bold")),
  ymin = ymn,
  ymax = ymx,
  xmin = xmn,
  xmax = xmx)

gtce3 <- ggplot_gtable(ggplot_build(pce3))
gtce3$layout$clip[gtce3$layout$name == "panel"] <- "off"


###############
## CASE 4 - EBOLA
###############
filename <- "ebola-0.9"
data <- data.table(read.table(paste0(inputEbolaDir, filename,".csv"),
                              sep=";", header=TRUE))

maxh <- 365
pData <- data[which((h <= maxh))]

ymin <- 1 - max(pData[which(n == 2 & h == 365 & pI >= 0.3)]$rho)
ymax <- 1 - min(pData[which(n == 2 & h == 365 & pI >= 0.9)]$rho)

pce4 <- ggplot(pData[which((n == 0) & (i < 1))],
               aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  ggtitle(expression(paste("u"[S]*" > u"[P]*" > u"[R]))) +
  xlab("") +
  ylab("") +
  xlim(0, maxh + 30) +
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
  annotate("text", x=380, y=ymin * 100, label="30%", fontface="bold", size=5) +
  annotate("text", x=380, y=ymax * 100, label="90%", fontface="bold", size=5) +
  scale_y_continuous(limits=c(0, 100),
                     breaks=c(0, 25, 50, 75, 100),
                     labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colours = c("red", "yellow", "green", "blue"),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(colour='black', size=34, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(colour='black', size=34, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(colour='black', size=16, face='bold'),
        axis.text.y = element_text(colour='black', size=16, face='bold'),
        axis.line = element_line(colour='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        #legend.position = "none",
        legend.title = element_text(colour="black", size=28, face="bold"),
        legend.text = element_text(colour="black", size=24, face="bold"),
        legend.key = element_rect(fill = "white"),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        plot.title = element_text(color="black", size=34, face="bold"))

pce4 <- pce4 + annotation_custom(
  grob = textGrob(label = "D", hjust = 0,
                  gp = gpar(cex = 3.5, fontface="bold")),
  ymin = ymn,
  ymax = ymx,
  xmin = xmn,
  xmax = xmx)

gtce4 <- ggplot_gtable(ggplot_build(pce4))
gtce4$layout$clip[gtce4$layout$name == "panel"] <- "off"


###############
## CASE 1 - FLUAVIAN
###############
filename <- "fluavian-1"
data <- data.table(read.table(paste0(inputFluavianDir, filename,".csv"),
                              sep=";", header=TRUE))

maxh <- 365
pData <- data[which((h <= maxh))]

pcf1 <- ggplot(pData[which((n == 0) & (i < 1))],
               aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  ggtitle(expression(paste("u"[S]*" = u"[R]*" > u"[P]))) +
  xlab("") +
  ylab(expression(paste("% Protection (1 - ", rho, ")"))) +
  xlim(0, maxh + 30) +
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
  scale_y_continuous(limits=c(0, 100),
                     breaks=c(0, 25, 50, 75, 100),
                     labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colours = c("red", "yellow", "green", "blue"),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(colour='black', size=34, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(colour='black', size=34, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(colour='black', size=16, face='bold'),
        axis.text.y = element_text(colour='black', size=16, face='bold'),
        axis.line = element_line(colour='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(colour="black", size=28, face="bold"),
        legend.text = element_text(colour="black", size=24, face="bold"),
        legend.key = element_rect(fill = "white"),
        plot.margin = unit(c(1, 0, 0, 1), "cm"),
        plot.title = element_text(color="black", size=34, face="bold"))

pcf1 <- pcf1 + annotation_custom(
  grob = textGrob(label = "E", hjust = 0,
                  gp = gpar(cex = 3.5, fontface="bold")),
  ymin = ymn,
  ymax = ymx,
  xmin = xmn,
  xmax = xmx)

gtcf1 <- ggplot_gtable(ggplot_build(pcf1))
gtcf1$layout$clip[gtcf1$layout$name == "panel"] <- "off"


###############
## CASE 2 - FLUAVIAN
###############
filename <- "fluavian-0.97"
data <- data.table(read.table(paste0(inputFluavianDir, filename,".csv"),
                              sep=";", header=TRUE))

maxh <- 365
pData <- data[which((h <= maxh))]

pcf2 <- ggplot(pData[which((n == 0) & (i < 1))],
               aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  ggtitle(expression(paste("u"[S]*" > u"[R]*" > u"[P]))) +
  xlab("") +
  ylab("") +
  xlim(0, maxh + 30) +
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
  scale_y_continuous(limits=c(0, 100),
                     breaks=c(0, 25, 50, 75, 100),
                     labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colours = c("red", "yellow", "green", "blue"),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(colour='black', size=34, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(colour='black', size=34, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(colour='black', size=16, face='bold'),
        axis.text.y = element_text(colour='black', size=16, face='bold'),
        axis.line = element_line(colour='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(colour="black", size=28, face="bold"),
        legend.text = element_text(colour="black", size=24, face="bold"),
        legend.key = element_rect(fill = "white"),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        plot.title = element_text(color="black", size=34, face="bold"))

pcf2 <- pcf2 + annotation_custom(
  grob = textGrob(label = "F", hjust = 0,
                  gp = gpar(cex = 3.5, fontface="bold")),
  ymin = ymn,
  ymax = ymx,
  xmin = xmn,
  xmax = xmx)

gtcf2 <- ggplot_gtable(ggplot_build(pcf2))
gtcf2$layout$clip[gtcf2$layout$name == "panel"] <- "off"


###############
## CASE 3 - FLUAVIAN
###############
filename <- "fluavian-0.95"
data <- data.table(read.table(paste0(inputFluavianDir, filename,".csv"),
                              sep=";", header=TRUE))

maxh <- 365
pData <- data[which((h <= maxh))]

pcf3 <- ggplot(pData[which((n == 0) & (i < 1))],
               aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  ggtitle(expression(paste("u"[S]*" > u"[R]*" = u"[P]))) +
  xlab("") +
  ylab("") +
  xlim(0, maxh + 30) +
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
  scale_y_continuous(limits=c(0, 100),
                     breaks=c(0, 25, 50, 75, 100),
                     labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colours = c("red", "yellow", "green", "blue"),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(colour='black', size=34, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(colour='black', size=34, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(colour='black', size=16, face='bold'),
        axis.text.y = element_text(colour='black', size=16, face='bold'),
        axis.line = element_line(colour='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(colour="black", size=28, face="bold"),
        legend.text = element_text(colour="black", size=24, face="bold"),
        legend.key = element_rect(fill = "white"),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        plot.title = element_text(color="black", size=34, face="bold"))

pcf3 <- pcf3 + annotation_custom(
  grob = textGrob(label = "G", hjust = 0,
                  gp = gpar(cex = 3.5, fontface="bold")),
  ymin = ymn,
  ymax = ymx,
  xmin = xmn,
  xmax = xmx)

gtcf3 <- ggplot_gtable(ggplot_build(pcf3))
gtcf3$layout$clip[gtcf3$layout$name == "panel"] <- "off"


###############
## CASE 4 - FLUAVIAN
###############
filename <- "fluavian-0.9"
data <- data.table(read.table(paste0(inputFluavianDir, filename,".csv"),
                              sep=";", header=TRUE))

maxh <- 365
pData <- data[which((h <= maxh))]

pcf4 <- ggplot(pData[which((n == 0) & (i < 1))],
               aes(x=h, y=(1 - rho) * 100, fill=(i * 100))) +
  ggtitle(expression(paste("u"[S]*" > u"[P]*" > u"[R]))) +
  xlab("") +
  ylab("") +
  xlim(0, maxh + 30) +
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
  scale_y_continuous(limits=c(0, 100),
                     breaks=c(0, 25, 50, 75, 100),
                     labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_gradientn(name = expression(paste("% Infective (i)")),
                       limits = c(0, 100),
                       values = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0),
                       colours = c("red", "yellow", "green", "blue"),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(axis.title.x = element_text(colour='black', size=34, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(colour='black', size=34, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(colour='black', size=16, face='bold'),
        axis.text.y = element_text(colour='black', size=16, face='bold'),
        axis.line = element_line(colour='black', size=1, linetype='solid'),
        panel.background = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        #legend.position = "none",
        legend.title = element_text(colour="black", size=28, face="bold"),
        legend.text = element_text(colour="black", size=24, face="bold"),
        legend.key = element_rect(fill = "white"),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        plot.title = element_text(color="black", size=34, face="bold"))

pcf4 <- pcf4 + annotation_custom(
  grob = textGrob(label = "H", hjust = 0,
                  gp = gpar(cex = 3.5, fontface="bold")),
  ymin = ymn,
  ymax = ymx,
  xmin = xmn,
  xmax = xmx)

gtcf4 <- ggplot_gtable(ggplot_build(pcf4))
gtcf4$layout$clip[gtcf4$layout$name == "panel"] <- "off"

plot <- grid.arrange(gtce1, gtce2, gtce3, gtce4, 
                     gtcf1, gtcf2, gtcf3, gtcf4,
                     ncol=4, nrow=2,
                     layout_matrix=rbind(c(1, 2, 3, 4), c(5, 6, 7, 8)),
                     heights=c(1, 1), widths=c(0.25, 0.25, 0.25, 0.3),
                     bottom=textGrob(expression(paste("Planning Horizon (H)")),
                                     gp=gpar(fontsize=34,
                                             fontface="bold")),
                     left=textGrob(expression(paste("Avian Influenza                Ebola")),
                                   rot=90,
                                   gp=gpar(fontfamily="sans",
                                           fontsize=38,
                                           fontface="bold")))

ggsave(paste0(outputDir,"cases.pdf"), plot=plot,
       width=80, height=30, units="cm")


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

pl <- ggplot(df, aes(x=inormal*100, y=value*100, group=fear)) +
  xlab(expression(paste("Real % Infectious (i)"))) +
  ylab(expression(paste("Perceived % Infectious ("*i^{1 / kappa}*")"))) +
  geom_line(size=1) +
  geom_text(data=df[which(inormal == 0.30)], aes(label=paste0("k = ", fear)),
            size=6, fontface='bold',vjust=-0.5, hjust=0, angle=40) +
  scale_x_continuous(breaks=c(0, 25, 50, 75, 100),
                     labels=c("0%", "25%", "50%", "75%", "100%"),
                     limits=c(0, 100)) +
  scale_y_continuous(breaks=c(0, 25, 50, 75, 100),
                     labels=c("0%", "25%", "50%", "75%", "100%"),
                     limits=c(0, 100)) +
  theme(axis.title.x = element_text(colour='black', size=24, face='bold',
                                    margin=margin(t=0.5, unit = "cm")),
        axis.title.y = element_text(colour='black', size=24, face='bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(colour='black', size=18, face='bold'),
        axis.text.y = element_text(colour='black', size=18, face='bold'),
        axis.line = element_line(colour='black', size=1.5, linetype='solid'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))

ggsave(paste0(outputDir, "fear.pdf"), plot=pl,
       width=7.5, height=7, units="in")


##
## Calculating the Expected Times
##
###############
## EBOLA INPUT PARAMETERS
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
rho <- 0.50

# Recover probability
g <- 1 - exp(-gamma)

# Discount factor (0 = No discount)
lambda <- 0

# Fear factor (1 = No fear)
kappa <- 1

# Payoffs (S, P, I, R)
payoffs <- c(1, 0.95, 0.10, 0.95)

# Disease's prevalence
i <- 0.5

# Planning Horizon
H <- seq(1, 365)

eTime <- foreach(h=H, .combine=rbind) %dopar%
  calc_expectedTime(h, i, bs, rho, g, lambda, kappa, payoffs)

total <- eTime[,6] + eTime[,7] + eTime[,8]
dataS <- cbind(eTime[,1], eTime[,2], eTime[,3], eTime[,4], eTime[,5], "S", eTime[,6] / total)
dataS <- rbind(dataS, cbind(eTime[,1], eTime[,2], eTime[,3], eTime[,4], eTime[,5], "I", eTime[,7] / total))
dataS <- rbind(dataS, cbind(eTime[,1], eTime[,2], eTime[,3], eTime[,4], eTime[,5], "R", eTime[,8] / total))
dataS <- rbind(dataS, cbind(eTime[,1], eTime[,2], eTime[,3], eTime[,4], eTime[,5], "U", eTime[,9]))

total <- eTime[,10] + eTime[,11] + eTime[,12]
dataP <- cbind(eTime[,1], eTime[,2], eTime[,3], eTime[,4], eTime[,5], "P", eTime[,10] / total)
dataP <- rbind(dataP, cbind(eTime[,1], eTime[,2], eTime[,3], eTime[,4], eTime[,5], "I", eTime[,11] / total))
dataP <- rbind(dataP, cbind(eTime[,1], eTime[,2], eTime[,3], eTime[,4], eTime[,5], "R", eTime[,12] / total))
dataP <- rbind(dataP, cbind(eTime[,1], eTime[,2], eTime[,3], eTime[,4], eTime[,5], "U", eTime[,13]))

dataS <- data.table(dataS)
colnames(dataS) <- c("h", "i", "rho", "lambda", "kappa", "type", "value")
dataS[which(value < 0)] <- 0

dataP <- data.table(dataP)
colnames(dataP) <- c("h", "i", "rho", "lambda", "kappa", "type", "value")
dataP[which(value < 0)] <- 0

plS <- ggplot(dataS[which(type != "U")], aes(x=as.numeric(as.character(h)),
                                             y=as.numeric(as.character(value)),
                                             fill=as.factor(type),
                                             color=as.factor(type),
                                             group=as.factor(type))) +
  geom_area(position="stack") +
  #xlab(expression(paste("Planning Horizon (H)"))) +
  ylab(expression("Expected % of H")) +
  xlab("") +
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1),
                     labels=c("0%", "25%", "50%", "75%", "100%"),
                     limits=c(-0.001, 1.001)) +
  scale_color_manual(name="",
                     values=c("S" = "black", "I" = "red2", "R" = "gold1")) +
  scale_fill_manual(name="",
                    values=c("S" = "black", "I" = "red2", "R" = "gold1")) +
  annotate("text", x=50, y=0.3, label="S", fontface="bold", size=10, color="white") +
  annotate("text", x=160, y=0.5, label="I", fontface="bold", size=10, color="white") +
  annotate("text", x=270, y=0.75, label="R", fontface="bold", size=10, color="white") +
  theme(axis.title.x = element_text(colour = 'black', size = 20, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(colour = 'black', size = 20, face = 'bold',
                                    margin=margin(r=0.2, unit = "cm")),
        axis.text.x = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.text.y = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.line = element_line(colour = 'black', size = 1.5, linetype = 'solid'),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(1.0, 0, 0.0, 0.0), "cm"),
        legend.position = "none",
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        legend.background = element_rect(fill="white"),
        legend.key = element_rect(fill = "white"))

#ggsave(paste0(outputDir,"s-contribution.png"), plot=plS)

plS <- plS + annotation_custom(
  grob = textGrob(label = "A", hjust = 0,
                  gp = gpar(cex = 2, fontface="bold")),
  ymin = 1.15,
  ymax = 1.15,
  xmin = -130,
  xmax = -130)

pl1 <- ggplot_gtable(ggplot_build(plS))
pl1$layout$clip[pl1$layout$name == "panel"] <- "off"


plP <- ggplot(dataP[which(type != "U")], aes(x=as.numeric(as.character(h)),
                                             y=as.numeric(as.character(value)),
                                             fill=as.factor(type),
                                             color=as.factor(type),
                                             group=as.factor(type))) +
  geom_area(position="stack") +
  #ylab(expression("Expected % of H")) +
  ylab("") +
  xlab("") +
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1),
                     labels=c("0%", "25%", "50%", "75%", "100%"),
                     limits=c(-0.001, 1.001)) +
  scale_color_manual(name="",
                     values=c("P" = "blue3", "I" = "red2", "R" = "gold1")) +
  scale_fill_manual(name="",
                    values=c("P" = "blue3", "I" = "red2", "R" = "gold1")) +
  annotate("text", x=100, y=0.4, label="P", fontface="bold", size=10, color="white") +
  annotate("text", x=175, y=0.66, label="I", fontface="bold", size=10, color="white") +
  annotate("text", x=280, y=0.85, label="R", fontface="bold", size=10, color="white") +
  theme(axis.title.x = element_text(colour = 'black', size = 20, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(colour = 'black', size = 20, face = 'bold',
                                    margin=margin(r=0.2,l=1.5, unit = "cm")),
        axis.text.x = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.text.y = element_text(colour = 'black', size = 18, face = 'bold'),
        axis.line = element_line(colour = 'black', size = 1.5, linetype = 'solid'),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(1.0, 0, 0.0, 0), "cm"),
        legend.position = "none",
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        legend.background = element_rect(fill="white"),
        legend.key = element_rect(fill = "white"))

#ggsave(paste0(outputDir,"p-contribution.png"), plot=plP)

plP <- plP + annotation_custom(
  grob = textGrob(label = "B", hjust = 0,
                  gp = gpar(cex = 2, fontface="bold")),
  ymin = 1.15,
  ymax = 1.15,
  xmin = -130,
  xmax = -130)

pl2 <- ggplot_gtable(ggplot_build(plP))
pl2$layout$clip[pl2$layout$name == "panel"] <- "off"


pl <- grid.arrange(pl1, pl2, ncol=2, nrow=1,
                   layout_matrix= rbind(c(1, 2)),
                   heights=c(1), widths=c(0.9, 1),
                   bottom=textGrob(expression(paste("Planning Horizon (H)")),
                                   gp=gpar(fontsize=24,
                                           fontface="bold")))

ggsave(paste0(outputDir,"contributions.pdf"), height=4, plot=pl)
