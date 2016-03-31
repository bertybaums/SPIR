library(data.table)
library(ggplot2)


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
      ts <- seq(0, Tss, 0.001)
      ds <- sum(exp(-l * ts)) / length(ts)
      
      ti <- seq(Tss, Tss + Tis, 0.001)
      di <- sum(exp(-l * ti)) / length(ti)
      
      tr <- seq(Tss + Tis, Tss + Tis + Trs, 0.001)
      dr <- sum(exp(-l * tr)) / length(tr)
      
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
      tp <- seq(0, Tpp, 0.001)
      dp <- sum(exp(-l * tp)) / length(tp)
      
      ti <- seq(Tpp, Tpp + Tip, 0.001)
      di <- sum(exp(-l * ti)) / length(ti)
      
      tr <- seq(Tpp + Tip, Tpp + Tip + Trp, 0.001)
      dr <- sum(exp(-l * tr)) / length(tr)
      
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

filename <- "ebola-k2"
data <- data.table(read.table(paste0("/data/projects/current/cmci/project-3/sub-projects/spir/ebola/",
                                     filename,".csv"), sep=";", header=TRUE))

maxh <- max(H)
pData <- data[which((h <= maxh))]

ymin <- max(pData[which(n == 2 & h == 365 & pI >= 0.3)]$rho)
ymax <- min(pData[which(n == 2 & h == 365 & pI >= 0.9)]$rho)

pl <- ggplot(pData[which((n == 0) & (i < 1))],
             aes(h, rho, fill=i)) +
  xlab("Planning Horizon") + ylab("rho") +
  xlim(0,maxh+20) + ylim(0, 1) +
  geom_tile() +
  geom_contour(data=pData[which(n == 2)],
               aes(x=h, y=rho, z=pI),
               breaks=c(0.3, 0.5, 0.7, 0.9),
               color="black",
               linetype="solid",
               size=0.5) +
  annotate("text", x=378, y=ymin, label=0.3, fontface="bold") +
  annotate("text", x=378, y=ymax, label=0.9, fontface="bold") +
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

##
##
##
rho <- 0.5
h <- 3

data <- data.table(calc_utilities(h, bs, rho, g, l, k, payoffs))

pu1 <- ggplot(data[which(i > 0)], aes(x=as.numeric(as.character(i)),
                                      y=as.numeric(as.character(U)),
                                      group=state, color=state, linetype=state)) +
  xlab("Proportion of Infected") + ylab("Utility") +
  geom_line(size=0.9) +
  scale_x_continuous(breaks=c(0, 0.5, 1.0),
                     labels=c("0.0", "0.5", "1.0")) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  scale_color_manual(values=c("black", "black")) +
  theme(axis.title.x = element_text(colour = 'black', size = 10, face = 'bold'),
        axis.title.y = element_text(colour = 'black', size = 14, face = 'bold'),
        axis.text.x = element_text(colour = 'black', size = 10, face = 'bold'),
        axis.text.y = element_text(colour = 'white', size = 10, face = 'bold'),
        axis.line = element_line(colour = 'black', size = 1.5, linetype = 'solid'),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))

##
##
##
rho <- 0.5
h <- 100

data <- data.table(calc_utilities(h, bs, rho, g, l, k, payoffs))

pu2 <- ggplot(data[which(i > 0)], aes(x=as.numeric(as.character(i)),
                                      y=as.numeric(as.character(U)),
                                      group=state, color=state, linetype=state)) +
  xlab("Proportion of Infected") + ylab("") +
  geom_line(size=0.9) +
  scale_x_continuous(breaks=c(0, 0.5, 1.0),
                     labels=c("0.0", "0.5", "1.0")) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  scale_color_manual(values=c("black", "black")) +
  theme(axis.title.x = element_text(colour = 'black', size = 10, face = 'bold'),
        axis.title.y = element_text(colour = 'black', size = 14, face = 'bold'),
        axis.text.x = element_text(colour = 'black', size = 10, face = 'bold'),
        axis.text.y = element_text(colour = 'white', size = 10, face = 'bold'),
        axis.line = element_line(colour = 'black', size = 1.5, linetype = 'solid'),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))

##
##
##
rho <- 0.5
h <- 364

data <- data.table(calc_utilities(h, bs, rho, g, l, k, payoffs))

pu3 <- ggplot(data[which(i > 0)], aes(x=as.numeric(as.character(i)),
                                      y=as.numeric(as.character(U)),
                                      group=state, color=state, linetype=state)) +
  xlab("Proportion of Infected") + ylab("") +
  geom_line(size=0.9) +
  scale_x_continuous(breaks=c(0, 0.5, 1.0),
                     labels=c("0.0", "0.5", "1.0")) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  scale_color_manual(values=c("black", "black")) +
  theme(axis.title.x = element_text(colour = 'black', size = 10, face = 'bold'),
        axis.title.y = element_text(colour = 'black', size = 14, face = 'bold'),
        axis.text.x = element_text(colour = 'black', size = 10, face = 'bold'),
        axis.text.y = element_text(colour = 'white', size = 10, face = 'bold'),
        axis.line = element_line(colour = 'black', size = 1.5, linetype = 'solid'),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))

plot <- grid.arrange(pu1, pu2, pu3, pl, ncol=4, nrow=4,
                     layout_matrix= rbind(c(1,2,3),
                                          c(1,2,3),
                                          c(4,4,4),
                                          c(4,4,4),
                                          c(4,4,4),
                                          c(4,4,4)))

ggsave("/data/projects/current/cmci/project-3/sub-projects/spir/ebola-plot.pdf",
       plot=plot)