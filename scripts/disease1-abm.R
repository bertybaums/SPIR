##
## Disease 1 ABM Dynamics Plots
##
## Author......: Luis Gustavo Nardin
## Last Change.: 10/01/2016
##
library(colorRamps)
library(data.table)
library(deSolve)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)


###############
## PATHS
###############
baseDir <- "/data/workspace/cmci/SPIR"
scriptDir <- paste0(baseDir, "/scripts")
inputDir <- paste0("/home/gnardin/spir-data")
outputDir <- paste0("/home/gnardin/spir-data")
figureDir <- paste0("/home/gnardin/spir-data")


###############
## FUNCTIONS
###############
source(paste0(scriptDir, "/calcSwitch.R"))
source(paste0(scriptDir, "/calcUtilities.R"))
source(paste0(scriptDir, "/SPIRmodel.R"))


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
payoffs <- c(1.00, 0.95, 0.10, 0.95)

# Number of agents
N <- 100000

# Time steps
timesteps <- 2100

# Initial number of Infcetious
initI <- 100

# Initial values
yinit <- c(S = N - initI, P = 0, I = initI, R = 0)

# Length of simulation
times <- seq(1, timesteps, 1)


###############
## INDIVIDUAL PLOTS
###############

H <- c(1, 30, 45, 90)

data <- NULL
for(h in H){
  info <- fread(paste0(inputDir, "/output-D1-H", h,"-d01.csv"),
                sep=";", header=TRUE)
  
  aux <- unlist(info[1,])
  aux <- cbind(H=h, replication=aux[1], time=aux[2], S=aux[3], P=aux[4], I=aux[5], R=aux[6])
  reg <- aux
  
  for(rep in unique(info$replication)){
    x <- info[which(replication == rep),]
    
    t <- 1
    while((t - 1) < max(times)){
      aux <- colMeans(x[which((time > ((t - 1) * N)) & (time <= (t * N))),],
                      na.rm=TRUE)
      
      if(!is.nan(aux[3])){
        reg <- cbind(H=h, replication=rep, time=t, S=aux[3], P=aux[4], I=aux[5], R=aux[6])
      }
      data <- rbind(data, reg)
      
      t <- t + 1
    }
  }
}

data <- data.table(data)

write.table(data, file=paste0(outputDir,"/replication-H-D1.csv"),
            append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)

data <- fread(paste0(outputDir, "/replication-H-D1.csv"), sep=";")


###############
## DYNAMICS PLANNING HORIZON
###############

H <- c(1, 30, 45, 90)

summary <- NULL
for(h in H){
	info <- fread(paste0(inputDir, "/output-D1-H", h,"-d01.csv"),
			sep=";", header=TRUE)
	
	aux <- unlist(info[1,])
	aux <- cbind(H=h, time=aux[2], S=aux[3], P=aux[4], I=aux[5], R=aux[6])
	reg <- aux
	
	t <- 1
	while((t - 1) < max(times)){
		aux <- colMeans(info[which((time > ((t - 1) * N)) &
										(time <= (t * N))),], na.rm=TRUE)
		
		if(!is.nan(aux[3])){
			reg <- cbind(H=h, time=t, S=aux[3], P=aux[4], I=aux[5], R=aux[6])
		}
		summary <- rbind(summary, reg)
		
		t <- t + 1
	}
}

summary <- data.table(summary)

write.table(summary, file=paste0(outputDir,"/summary-H-D1.csv"),
    append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)

## Generate plots
summary <- fread(paste0(inputDir, "/summary-H-D1.csv"), header=TRUE, sep=";")

pl <- ggplot(summary, aes(x=time, y=((I / (S + P + I + R)) * 100),
						color=as.factor(H),
						size=as.factor(H))) +
		xlab("") +
		ylab(expression(paste("% Infectious (i)"))) +
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


###############
## DYNAMICS DECISION
###############

D <- c(0, 1, 2)

summary <- NULL
for(d in D){
  info <- fread(paste0(inputDir, "/output-D1-H90-d0", d, ".csv"),
                sep=";", header=TRUE)
  
  aux <- unlist(info[1,])
  aux <- cbind(D=d, time=aux[2], S=aux[3], P=aux[4], I=aux[5], R=aux[6])
  reg <- aux
  
  t <- 1
  while((t - 1) < max(times)){
    aux <- colMeans(info[which((time > ((t - 1) * N)) &
                                 (time <= (t * N))),], na.rm=TRUE)
    
    if(!is.nan(aux[3])){
      reg <- cbind(D=d, time=t, S=aux[3], P=aux[4], I=aux[5], R=aux[6])
    }
    summary <- rbind(summary, reg)
    
    t <- t + 1
  }
}

summary <- data.table(summary)

write.table(summary, file=paste0(outputDir,"/summary-d-D1.csv"),
            append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)

## Generate plots
summary <- fread(paste0(inputDir, "/summary-d-D1.csv"), header=TRUE, sep=";")

pl <- ggplot(summary, aes(x=time, y=((I / (S + P + I + R)) * 100),
                       color=as.factor(D),
                       size=as.factor(D))) +
  xlab("") + ylab(expression(paste("% Infectious (i)"))) +
  geom_line() +
  scale_color_manual(name = expression(paste("Decision\nFrequency (", delta, ")")),
                     values = c("grey60", "blue", "red"),
                     labels = c("0.00", "0.01", "0.02")) +
  scale_size_manual(name = expression(paste("Decision\nFrequency (", delta, ")")),
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
