##
## Summarizes heterogeneous simulation results
##
## Author......: Luis Gustavo Nardin
## Last Change.: 02/17/2017
##
library(data.table)
library(ggplot2)

baseDir <- "/data/downloads/garbage/spir"
inputDir <- paste0(baseDir, "/raw/hete")
outputDir <- paste0(baseDir, "/output/summary")

## Combination of parameters
nComb <- 48

## 1 - Time to get infected
## 2 - Number of agents that get infected
## 3 - Proportion of infected + recovered
## 4 - Time to adopt prophylactic behavior
## 5 - Number that adopted prophylactic behavior
## 6 - Proportion of prophylactic behavior adoption
## 7 - Accumulated payoff
types <- 7
TIME_INFECTED <- 1
NUM_INFECTED <- 2
PROP_INFECTED <- 3
TIME_ADOPTED <- 4
NUM_ADOPTED <- 5
PROP_ADOPTED <- 6
ACC_PAYOFF <- 7

## Switching point range
swRange <- list(c(0, 0.01), c(0.01, 0.02), c(0.02, 0.03), c(0.03, 0.04),
    c(0.04, 0.05), c(0.05, 0.06), c(0.06, 0.07), c(0.07, 0.08), c(0.08, 0.09),
    c(0.09, 0.10), c(0.10, 0.11), c(0.11, 0.12), c(0.12, 0.13), c(0.13, 0.14),
    c(0.14, 0.15), c(0.15, 0.16), c(0.16, 0.17), c(0.17, 0.18), c(0.18, 0.19),
    c(0.19, 0.20), c(0.20, 1.00))

## Output data
outputM <- array(0, dim=c(nComb, types, length(swRange)))
outputD <- array(0, dim=c(nComb, types, length(swRange)))
outputS <- array(0, dim=c(nComb, types, length(swRange)))

outputQM <- array(0, dim=c(nComb, types, 4))
outputQD <- array(0, dim=c(nComb, types, 4))
outputQS <- array(0, dim=c(nComb, types, 4))

outputQ <- array(0, dim=c(nComb, length(swRange)))
outputQQ <- array(0, dim=c(nComb, 4))

for(j in 1:nComb){
  dataI <- fread(paste0(inputDir, "/output-", j, "-init.csv"))
  setkey(dataI, id)
  
  nAgents <- nrow(dataI)
  
  qt <- quantile(dataI$switchPoint)
  qtRange <- list()
  qtRange[[1]] <- c(0, as.numeric(qt[2]))
  qtRange[[2]] <- c(as.numeric(qt[2]), as.numeric(qt[3]))
  qtRange[[3]] <- c(as.numeric(qt[3]), as.numeric(qt[4]))
  qtRange[[4]] <- c(as.numeric(qt[4]), 1)
  
  ## Total number of agents
  for(i in 1:length(swRange)){
    aux <- nrow(dataI[which((switchPoint > swRange[[i]][1]) &
                    (switchPoint <= swRange[[i]][2])),])
    outputQ[j, i] <- ifelse(is.null(aux), 0, aux)
  }
  
  for(i in 1:length(qtRange)){
    aux <- nrow(dataI[which((switchPoint > qtRange[[i]][1]) &
                    (switchPoint <= qtRange[[i]][2])),])
    outputQQ[j, i] <- ifelse(is.null(aux), 0, aux)
  }
  
  dataR <- fread(paste0(inputDir, "/output-", j, "-raw.csv"))
  setkey(dataR, replication, id)
  
  replications <- length(unique(dataR$replication))
    
  ## 1 - Time to get infected
  ## 2 - Number of agents that get infected
  ## 3 - Proportion of infected + recovered
  timeI <- dataR[which(state == 2),
      list(time=min(time, na.rm = TRUE)), by=list(replication, id)]
  timeI <- timeI[,list(time=mean(time, na.rm=TRUE)), by=id]
  setkey(timeI, id)
  
  data <- merge(dataI, timeI, all.x=TRUE)
  for(i in 1:length(swRange)){
    aux <- data[which((switchPoint > swRange[[i]][1]) &
                (switchPoint <= swRange[[i]][2])),]$time
    outputM[j, TIME_INFECTED, i] <- mean(aux, na.rm=TRUE)
    outputD[j, TIME_INFECTED, i] <- median(aux, na.rm=TRUE)
    outputS[j, TIME_INFECTED, i] <- sd(aux, na.rm=TRUE)
    
    num <- length(aux[!is.na(aux)])
    outputM[j, NUM_INFECTED, i] <- num
    
    prop <- num / length(aux)
    outputM[j, PROP_INFECTED, i] <- prop
  }
  
  for(i in 1:length(qtRange)){
    aux <- data[which((switchPoint > qtRange[[i]][1]) &
                (switchPoint <= qtRange[[i]][2])),]$time
    outputQM[j, TIME_INFECTED, i] <- mean(aux, na.rm=TRUE)
    outputQD[j, TIME_INFECTED, i] <- median(aux, na.rm=TRUE)
    outputQS[j, TIME_INFECTED, i] <- sd(aux, na.rm=TRUE)
    
    num <- length(aux[!is.na(aux)])
    outputQM[j, NUM_INFECTED, i] <- num
    
    prop <- num / length(aux)
    outputQM[j, PROP_INFECTED, i] <- prop
  }
  
  ## 4 - Time to adopt prophylactic behavior
  ## 5 - Number that adopted prophylactic behavior
  ## 6 - Proportion of prophylactic behavior adoption
  timeP <- dataR[which(state == 1),
      list(time=min(time, na.rm = TRUE)), by=list(replication, id)]
  timeP <- timeP[,list(time=mean(time, na.rm=TRUE)), by=id]
  setkey(timeP, id)
  
  data <- merge(dataI, timeP, all.x=TRUE)
  for(i in 1:length(swRange)){
    aux <- data[which((switchPoint > swRange[[i]][1]) &
                (switchPoint <= swRange[[i]][2])),]$time
    outputM[j, TIME_ADOPTED, i] <- mean(aux, na.rm=TRUE)
    outputD[j, TIME_ADOPTED, i] <- median(aux, na.rm=TRUE)
    outputS[j, TIME_ADOPTED, i] <- sd(aux, na.rm=TRUE)
    
    num <- length(aux[!is.na(aux)])
    outputM[j, NUM_ADOPTED, i] <- num
    
    prop <- num / length(aux)
    outputM[j, PROP_ADOPTED, i] <- prop
  }
  
  for(i in 1:length(qtRange)){
    aux <- data[which((switchPoint > qtRange[[i]][1]) &
                (switchPoint <= qtRange[[i]][2])),]$time
    outputQM[j, TIME_ADOPTED, i] <- mean(aux, na.rm=TRUE)
    outputQD[j, TIME_ADOPTED, i] <- median(aux, na.rm=TRUE)
    outputQS[j, TIME_ADOPTED, i] <- sd(aux, na.rm=TRUE)
    
    num <- length(aux[!is.na(aux)])
    outputQM[j, NUM_ADOPTED, i] <- num
    
    prop <- num / length(aux)
    outputQM[j, PROP_ADOPTED, i] <- prop
  }
  
  ## Accumulated payoff
  numS <- dataR[which(state == 0), list(num=.N), by=list(replication, id)]
  numS <- numS[,list(num=mean(num, na.rm=TRUE)), by=id]
  data <- merge(dataI, numS, all.x=TRUE)
  names(data) <- c(names(data)[-length(names(data))], "numS")
  setkey(numS, id)
  
  numP <- dataR[which(state == 1), list(num=.N), by=list(replication, id)]
  numP <- numP[,list(num=mean(num, na.rm=TRUE)), by=id]
  data <- merge(data, numP, all.x=TRUE)
  names(data) <- c(names(data)[-length(names(data))], "numP")
  setkey(numP, id)
  
  numI <- dataR[which(state == 2), list(num=.N), by=list(replication, id)]
  numI <- numI[,list(num=mean(num, na.rm=TRUE)), by=id]
  data <- merge(data, numI, all.x=TRUE)
  names(data) <- c(names(data)[-length(names(data))], "numI")
  setkey(numI, id)
  
  numR <- dataR[which(state == 3), list(num=.N), by=list(replication, id)]
  numR <- numR[,list(num=mean(num, na.rm=TRUE)), by=id]
  data <- merge(data, numR, all.x=TRUE)
  names(data) <- c(names(data)[-length(names(data))], "numR")
  setkey(numP, id)
  
  for(i in 1:length(swRange)){
    aux <- data[which((switchPoint > swRange[[i]][1]) &
                (switchPoint <= swRange[[i]][2])),]
    
    aux$numS[is.na(aux$numS)] <- 0
    aux$numP[is.na(aux$numP)] <- 0
    aux$numI[is.na(aux$numI)] <- 0
    aux$numR[is.na(aux$numR)] <- 0
    
    calc <- (aux$numS * aux$payoffS) + (aux$numP * aux$payoffP) +
        (aux$numI * aux$payoffI) + (aux$numR * aux$payoffR)
    
    outputM[j, ACC_PAYOFF, i] <- mean(calc, na.rm=TRUE)
    outputD[j, ACC_PAYOFF, i] <- median(calc, na.rm=TRUE)
    outputS[j, ACC_PAYOFF, i] <- sd(calc, na.rm=TRUE)
  }
  
  for(i in 1:length(qtRange)){
    aux <- data[which((switchPoint > qtRange[[i]][1]) &
                (switchPoint <= qtRange[[i]][2])),]
    
    aux$numS[is.na(aux$numS)] <- 0
    aux$numP[is.na(aux$numP)] <- 0
    aux$numI[is.na(aux$numI)] <- 0
    aux$numR[is.na(aux$numR)] <- 0
    
    calc <- (aux$numS * aux$payoffS) + (aux$numP * aux$payoffP) +
        (aux$numI * aux$payoffI) + (aux$numR * aux$payoffR)
    
    outputQM[j, ACC_PAYOFF, i] <- mean(calc, na.rm=TRUE)
    outputQD[j, ACC_PAYOFF, i] <- median(calc, na.rm=TRUE)
    outputQS[j, ACC_PAYOFF, i] <- sd(calc, na.rm=TRUE)
  }
}

## Variable
## 0 - Mean
## 1 - Median
## 2 - Standard Deviation
## 3 - Quantity

data <- NULL
for(j in 1:nComb){
  for(l in 1:length(swRange)){
    data <- rbind(data, cbind(j, l, 3, outputQ[j, l]))
  }
}

data <- data.table(data)
names(data) <- c("comb", "range", "var", "value")

write.table(data, file=paste0(outputDir,"/num-sw.csv"),
    append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)


data <- NULL
for(j in 1:nComb){
  for(l in 1:length(qtRange)){
    data <- rbind(data, cbind(j, l, 3, outputQQ[j, l]))
  }
}

data <- data.table(data)
names(data) <- c("comb", "range", "var", "value")

write.table(data, file=paste0(outputDir,"/num-qt.csv"),
    append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)


data <- NULL
for(j in 1:nComb){
  for(t in 1:types){
    for(l in 1:length(swRange)){
      if((t == TIME_INFECTED) | (t == TIME_ADOPTED) | t == ACC_PAYOFF){
        data <- rbind(data, cbind(j, t, l, 0, outputM[j, t, l]))
        data <- rbind(data, cbind(j, t, l, 1, outputD[j, t, l]))
        data <- rbind(data, cbind(j, t, l, 2, outputS[j, t, l]))
      } else if((t == NUM_INFECTED) | (t == PROP_INFECTED) |
          (t == NUM_ADOPTED) | (t == PROP_ADOPTED)){
        data <- rbind(data, cbind(j, t, l, 3, outputM[j, t, l]))
      }
    }
  }
}

data <- data.table(data)
names(data) <- c("comb", "type", "range", "var", "value")

write.table(data, file=paste0(outputDir,"/individual-sw.csv"),
    append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)


data <- NULL
for(j in 1:nComb){
  for(t in 1:types){
    for(l in 1:length(qtRange)){
      if((t == TIME_INFECTED) | (t == TIME_ADOPTED) | t == ACC_PAYOFF){
        data <- rbind(data, cbind(j, t, l, 0, outputQM[j, t, l]))
        data <- rbind(data, cbind(j, t, l, 1, outputQD[j, t, l]))
        data <- rbind(data, cbind(j, t, l, 2, outputQS[j, t, l]))
      } else if((t == NUM_INFECTED) | (t == PROP_INFECTED) |
          (t == NUM_ADOPTED) | (t == PROP_ADOPTED)){
        data <- rbind(data, cbind(j, t, l, 3, outputQM[j, t, l]))
      }
    }
  }
}

data <- data.table(data)
names(data) <- c("comb", "type", "range", "var", "value")

write.table(data, file=paste0(outputDir,"/individual-qt.csv"),
    append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)
