library(biganalytics)
library(bigmemory)
library(data.table)
library(ggplot2)
library(snow)

##
## Function
##
calc_expectedtime <- function(h){
  require(bigmemory)
  
  setwd("/data")
  data <- attach.big.matrix(dataDescr)

  rw <- (h - 1) * length(I) * length(B) * length(R) * length(G)
  for(i in I){
    for(b in B){
      for(r in R){
        for(g in G){
          p <- i * b
          Tss <- (1 - ((1 - p)^h)) / p
          if (p != g){
            Tis <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - (((1 - p)^h) / g)
          } else {
            Tis <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - (((1 - p)^h) / g)
          }
          Trs <- h - Tss -Tis
          
          p <- i * (b * r)
          Tpp <- (1 - ((1 - p)^h)) / p
          if (p != g){
            Tip <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - (((1 - p)^h) / g)
          } else {
            Tip <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - (((1 - p)^h) / g)
          }
          Trp <- h - Tpp - Tip
          
          data[rw, ] <- c(h, i, b, r, g, Tss, Tis, Trs, Tpp, Tip, Trp)
          rw <- rw + 1
        }
      }
    }
  }
}


##
## Input parameters
##

# Transmission risk (Susceptible)
B <- seq(0.00, 1.00, 0.01)

# Prophylactic protection
R <- seq(0.00, 1.00, 0.01)

# Recover probability
G <- seq(0.00, 0.50, 0.01)

# Range of Time Horizon to evaluate
H <- seq(1, 368)

# Range of Proportion of infected to evaluate
I <- seq(0.00, 0.25, 0.01)

# File name
dataFile <- "data.bin"

# File descriptor
dataDescr <- "data.dsc"

# Column headers
colHeader <- c("h","i", "bs", "rho", "g", "Tss", "Tis", "Trs", "Tpp", "Tip", "Trp")

# Total number of rows
rows <- as.numeric(length(H)) * length(I) * length(B) * length(R) * length(G)

# Total number of columns
cols <- length(colHeader)

# Number of processors
np <- 3

data <- big.matrix(rows, cols, type="double", init=0,
                   dimnames=list(NULL, colHeader),
                   backingfile=dataFile,
                   descriptorfile=dataDescr)

for(P in split(H, rep(1:as.integer(length(H)/np), np))){
  cl <- makeCluster(np)
  
  clusterExport(cl, c("calc_expectedtime","I", "B", "R", "G", "dataDescr"))
  
  parLapply(cl, P, function(h){
    calc_expectedtime(h)
  })
  
  stopCluster(cl)
}

for(P in split(H, rep(1:as.integer(length(H)/np), np))){
  cl <- makeCluster(np)
  
  clusterExport(cl, c("calc_expectedtime","I", "B", "R", "G", "dataDescr"))
  
  parLapply(cl, P, function(h){
    calc_expectedtime(h)
  })
  
  stopCluster(cl)
}

##
## Contribution of each term
##
h <- 100
data <- attach.big.matrix(dataDescr)
ndata <- matrix(data=0, nrow=rows / length(H), ncol=cols)
idata <- mwhich(data, 1, h, "eq")
for (index in 1:length(idata)){
  ndata[index,] <- data[idata[index],]
}
ndata <- data.table(ndata)
colnames(ndata) <- colHeader

ggplot(na.omit(ndata[which(bs == 0.02 & rho == 0.5 & g == 0.05)])) + ylim(0,1) +
  geom_line(aes(x=i, y=Trp / (Tpp+Tip+Trp)), color="blue") +
  geom_line(aes(x=i, y=Trs / (Tss+Tis+Trs)), color="black")

##
## Check the processed H values
##
n <- array(data=0,dim=length(H))
for(h in H){
  n[h] <- length(mwhich(data, 1, h, "eq"))
}
