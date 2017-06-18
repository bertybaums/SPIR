library(data.table)
library(ggplot2)
library(RColorBrewer)

baseDir <- "/data/workspace/cmci/SPIR"
inputDir <- paste0(baseDir, "/data/raw/network")
outputDir <- paste0(baseDir, "/data/figures/network")

data <- NULL
for(i in 1:16){
  aux <- fread(paste0(inputDir, "/output-", i, ".csv"))
  data <- rbind(data, cbind(i, aux))
}
data <- data.table(data)

data <- data[,list(avgS=mean(susceptible), sdS=sd(susceptible),
        avgP=mean(prophylactic), sdP=sd(prophylactic),
        avgI=mean(infected), sdI=sd(infected),
        avgR=mean(recovered), sdR=sd(recovered)),
    by=list(i, time)]


## Complete Network
ggplot(data[which(i == 1 | i == 9),],
       aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
           group=as.factor(i),
           colour=as.factor(i))) +
  xlab("Time") +
  ylab("% Infectious") +
  geom_line(size=1.5) +
  scale_color_manual(name = "",
                     labels = c("Random", "Static"),
                     values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
  theme(axis.title.x = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 24, face = 'bold'),
        axis.text.y = element_text(color = 'black', size = 24, face = 'bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        #legend.position = "none",
        legend.title = element_text(color="black", size=24, face="bold"),
        legend.text = element_text(color="black", size=20, face="bold"),
        legend.key = element_rect(fill = "white"))


## Random Network 0.2
ggplot(data[which(i == 2 | i == 10),],
       aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
           group=as.factor(i),
           colour=as.factor(i))) +
  xlab("Time") +
  ylab("% Infectious") +
  geom_line(size=1.5) +
  scale_color_manual(name = "",
                     labels = c("Random", "Static"),
                     values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
  theme(axis.title.x = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 24, face = 'bold'),
        axis.text.y = element_text(color = 'black', size = 24, face = 'bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        legend.position = "none",
        legend.title = element_text(color="black", size=24, face="bold"),
        legend.text = element_text(color="black", size=20, face="bold"),
        legend.key = element_rect(fill = "white"))


## Random Network 0.5
ggplot(data[which(i == 3 | i == 11),],
       aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
           group=as.factor(i),
           colour=as.factor(i))) +
  xlab("Time") +
  ylab("% Infectious") +
  geom_line(size=1.5) +
  scale_color_manual(name = "",
                     labels = c("Random", "Static"),
                     values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
  theme(axis.title.x = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 24, face = 'bold'),
        axis.text.y = element_text(color = 'black', size = 24, face = 'bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        legend.position = "none",
        legend.title = element_text(color="black", size=24, face="bold"),
        legend.text = element_text(color="black", size=20, face="bold"),
        legend.key = element_rect(fill = "white"))

## Random Network 0.8
ggplot(data[which(i == 4 | i == 12),],
       aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
           group=as.factor(i),
           colour=as.factor(i))) +
  xlab("Time") +
  ylab("% Infectious") +
  geom_line(size=1.5) +
  scale_color_manual(name = "",
                     labels = c("Random", "Static"),
                     values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
  theme(axis.title.x = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 24, face = 'bold'),
        axis.text.y = element_text(color = 'black', size = 24, face = 'bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        #legend.position = "none",
        legend.title = element_text(color="black", size=24, face="bold"),
        legend.text = element_text(color="black", size=20, face="bold"),
        legend.key = element_rect(fill = "white"))

## Small World Network 0.2
ggplot(data[which(i == 5 | i == 13),],
       aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
           group=as.factor(i),
           colour=as.factor(i))) +
  xlab("Time") +
  ylab("% Infectious") +
  geom_line(size=1.5) +
  scale_color_manual(name = "",
                     labels = c("Random", "Static"),
                     values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
  theme(axis.title.x = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 24, face = 'bold'),
        axis.text.y = element_text(color = 'black', size = 24, face = 'bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        legend.position = "none",
        legend.title = element_text(color="black", size=24, face="bold"),
        legend.text = element_text(color="black", size=20, face="bold"),
        legend.key = element_rect(fill = "white"))

## Small World Network 0.5
ggplot(data[which(i == 6 | i == 14),],
       aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
           group=as.factor(i),
           colour=as.factor(i))) +
  xlab("Time") +
  ylab("% Infectious") +
  geom_line(size=1.5) +
  scale_color_manual(name = "",
                     labels = c("Random", "Static"),
                     values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
  theme(axis.title.x = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 24, face = 'bold'),
        axis.text.y = element_text(color = 'black', size = 24, face = 'bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        legend.position = "none",
        legend.title = element_text(color="black", size=24, face="bold"),
        legend.text = element_text(color="black", size=20, face="bold"),
        legend.key = element_rect(fill = "white"))

## Small World Network 0.8
ggplot(data[which(i == 7 | i == 15),],
       aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
           group=as.factor(i),
           colour=as.factor(i))) +
  xlab("Time") +
  ylab("% Infectious") +
  geom_line(size=1.5) +
  scale_color_manual(name = "",
                     labels = c("Random", "Static"),
                     values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
  theme(axis.title.x = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 24, face = 'bold'),
        axis.text.y = element_text(color = 'black', size = 24, face = 'bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        #legend.position = "none",
        legend.title = element_text(color="black", size=24, face="bold"),
        legend.text = element_text(color="black", size=20, face="bold"),
        legend.key = element_rect(fill = "white"))

## Scalefree Network
ggplot(data[which(i == 8 | i == 16),],
       aes(x=time, y=(avgI/(avgS+avgP+avgI+avgR))*100,
           group=as.factor(i),
           colour=as.factor(i))) +
  xlab("Time") +
  ylab("% Infectious") +
  geom_line(size=1.5) +
  scale_color_manual(name = "",
                     labels = c("Random", "Static"),
                     values = c("black", "blue", "green", "red", "orange", "gray", "brown", "magenta", "purple")) +
  theme(axis.title.x = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(t=0.2, unit = "cm")),
        axis.title.y = element_text(color = 'black', size = 24, face = 'bold',
                                    margin=margin(r=0.4, unit = "cm")),
        axis.text.x = element_text(color = 'black', size = 24, face = 'bold'),
        axis.text.y = element_text(color = 'black', size = 24, face = 'bold'),
        axis.line.x = element_line(color='black', size=1, linetype='solid'),
        axis.line.y = element_line(color='black', size=1, linetype='solid'),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        #legend.position = "none",
        legend.title = element_text(color="black", size=24, face="bold"),
        legend.text = element_text(color="black", size=20, face="bold"),
        legend.key = element_rect(fill = "white"))
