##
## Summarizes heterogeneous simulation results
##
## Author......: Luis Gustavo Nardin
## Last Change.: 02/16/2017
##
library(data.table)
library(ggplot2)

baseDir <- "/data/downloads/garbage/spir"
inputDir <- paste0(baseDir, "/raw/hete")
outputDir <- paste0(baseDir, "/output")

## Combination of parameters
nComb <- 48

## 1 - Time to get infected
## 2 - Time to adopt prophylactic behavior
## 3 - Frequency of prophylactic behavior adoption
## 4 - Proportion of infected + recovered
## 5 - Accumulated payoff
type <- 5

## Switching point range
swRange <- list(c(0, 0.01), c(0.01, 0.02), c(0.02, 0.03), c(0.03, 0.04),
    c(0.04, 0.05), c(0.05, 0.06), c(0.06, 0.07), c(0.07, 0.08), c(0.08, 0.09),
    c(0.09, 0.10), c(0.10, 0.11), c(0.11, 0.12), c(0.12, 0.13), c(0.13, 0.14),
    c(0.14, 0.15), c(0.15, 0.16), c(0.16, 0.17), c(0.17, 0.18), c(0.18, 0.19),
    c(0.19, 0.20), c(0.20, 1.00))

## Output data
outputM <- array(0, dim=c(nComb, type, length(swRange)))
outputD <- array(0, dim=c(nComb, type, length(swRange)))
outputS <- array(0, dim=c(nComb, type, length(swRange)))

outputQM <- array(0, dim=c(nComb, type, 4))
outputQD <- array(0, dim=c(nComb, type, 4))
outputQS <- array(0, dim=c(nComb, type, 4))

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
  
  dataR <- fread(paste0(inputDir, "/output-", j, "-raw.csv"))
  setkey(dataR, replication, id)
  
  replications <- length(unique(dataR$replication))
  
  ## Time to become infected
  timeI <- dataR[which(state == 2),
      list(time=ifelse(!is.infinite(min(time)),
              min(time, na.rm = TRUE), NA)), by=list(replication, id)]
  timeI <- timeI[,list(time=mean(time, na.rm=TRUE)), by=id]
  setkey(timeI, id)
  
  data <- merge(dataI, timeI, all.x=TRUE)
  for(i in 1:length(swRange)){
    aux <- data[which((switchPoint > swRange[[i]][1]) &
                (switchPoint <= swRange[[i]][2])),]$time
    outputM[j, 1, i] <- mean(aux, na.rm=TRUE)
    outputD[j, 1, i] <- median(aux, na.rm=TRUE)
    outputS[j, 1, i] <- sd(aux, na.rm=TRUE)
  }
  
  for(i in 1:length(qtRange)){
    aux <- data[which((switchPoint > qtRange[[i]][1]) &
                (switchPoint <= qtRange[[i]][2])),]$time
    outputQM[j, 1, i] <- mean(aux, na.rm=TRUE)
    outputQD[j, 1, i] <- median(aux, na.rm=TRUE)
    outputQS[j, 1, i] <- sd(aux, na.rm=TRUE)
  }
  
  ## Time to adopt prophylactic behavior
  timeP <- dataR[which(state == 1),
      list(time=ifelse(!is.infinite(min(time)),
              min(time, na.rm = TRUE), NA)), by=list(replication, id)]
  timeP <- timeP[,list(time=mean(time, na.rm=TRUE)), by=id]
  setkey(timeP, id)
  
  data <- merge(dataI, timeP, all.x=TRUE)
  for(i in 1:length(swRange)){
    aux <- data[which((switchPoint > swRange[[i]][1]) &
                (switchPoint <= swRange[[i]][2])),]$time
    outputM[j, 2, i] <- mean(aux, na.rm=TRUE)
    outputD[j, 2, i] <- median(aux, na.rm=TRUE)
    outputS[j, 2, i] <- sd(aux, na.rm=TRUE)
  }
  
  for(i in 1:length(qtRange)){
    aux <- data[which((switchPoint > qtRange[[i]][1]) &
                (switchPoint <= qtRange[[i]][2])),]$time
    outputQM[j, 2, i] <- mean(aux, na.rm=TRUE)
    outputQD[j, 2, i] <- median(aux, na.rm=TRUE)
    outputQS[j, 2, i] <- sd(aux, na.rm=TRUE)
  }
  
  ## Proportion of prophylactic behavior adopters
  freqP <- dataR[which(state == 1), list(num=ifelse(.N > 0, 1, 0)),
      by=list(replication, id)]
  freqP <- freqP[,list(num=sum(num, na.rm=TRUE) / replications), by=id]
  setkey(freqP, id)
  
  data <- merge(dataI, freqP, all.x=TRUE)
  data[which(is.na(data$num)),]$num <- 0
  for(i in 1:length(swRange)){
    aux <- data[which((switchPoint > swRange[[i]][1]) &
                (switchPoint <= swRange[[i]][2])),]$num
    outputM[j, 3, i] <- mean(aux, na.rm=TRUE)
    outputD[j, 3, i] <- median(aux, na.rm=TRUE)
    outputS[j, 3, i] <- sd(aux, na.rm=TRUE)
  }
  
  for(i in 1:length(qtRange)){
    aux <- data[which((switchPoint > qtRange[[i]][1]) &
                (switchPoint <= qtRange[[i]][2])),]$num
    outputQM[j, 3, i] <- mean(aux, na.rm=TRUE)
    outputQD[j, 3, i] <- median(aux, na.rm=TRUE)
    outputQS[j, 3, i] <- sd(aux, na.rm=TRUE)
  }
  
  ## Proportion of infected + recovered
  s <- dataR[, list(num=nrow(.SD[which(state == 0 & time == max(time)),])),
      by=list(replication, id)]
  s <- s[,list(num=sum(num, na.rm=TRUE) / replications), by=id]
  setkey(s, id)
  data <- merge(dataI, s, all.x=TRUE)
  names(data) <- c(names(data)[-length(names(data))], "S")
  
  p <- dataR[, list(num=nrow(.SD[which(state == 1 & time == max(time)),])),
      by=list(replication, id)]
  p <- p[,list(num=sum(num, na.rm=TRUE) / replications), by=id]
  setkey(p, id)
  data <- merge(data, p, all.x=TRUE)
  names(data) <- c(names(data)[-length(names(data))], "P")
  
  i <- dataR[, list(num=nrow(.SD[which(state == 2 & time == max(time)),])),
      by=list(replication, id)]
  i <- i[,list(num=sum(num) / replications), by=id]
  setkey(i, id)
  data <- merge(data, i, all.x=TRUE)
  names(data) <- c(names(data)[-length(names(data))], "I")
  
  r <- dataR[, list(num=nrow(.SD[which(state == 3 & time == max(time)),])),
      by=list(replication, id)]
  r <- r[,list(num=sum(num) / replications), by=id]
  setkey(r, id)
  data <- merge(data, r, all.x=TRUE)
  names(data) <- c(names(data)[-length(names(data))], "R")
  
  for(i in 1:length(swRange)){
    aux <- data[which((switchPoint > swRange[[i]][1]) &
                (switchPoint <= swRange[[i]][2])),]
    
    aux$S[is.na(aux$S)] <- 0
    aux$P[is.na(aux$P)] <- 0
    aux$I[is.na(aux$I)] <- 0
    aux$R[is.na(aux$R)] <- 0
    
    calc <- sum(aux$I, aux$R) / sum(aux$S, aux$P, aux$I, aux$R)
    
    outputM[j, 4, i] <- mean(calc, na.rm=TRUE)
    outputD[j, 4, i] <- median(calc, na.rm=TRUE)
    outputS[j, 4, i] <- sd(calc, na.rm=TRUE)
  }
  
  for(i in 1:length(qtRange)){
    aux <- data[which((switchPoint > qtRange[[i]][1]) &
                (switchPoint <= qtRange[[i]][2])),]
    
    aux$S[is.na(aux$S)] <- 0
    aux$P[is.na(aux$P)] <- 0
    aux$I[is.na(aux$I)] <- 0
    aux$R[is.na(aux$R)] <- 0
    
    calc <- sum(aux$I, aux$R) / sum(aux$S, aux$P, aux$I, aux$R)
    
    outputQM[j, 4, i] <- mean(calc, na.rm=TRUE)
    outputQD[j, 4, i] <- median(calc, na.rm=TRUE)
    outputQS[j, 4, i] <- sd(calc, na.rm=TRUE)
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
    
    outputM[j, 5, i] <- mean(calc, na.rm=TRUE)
    outputD[j, 5, i] <- median(calc, na.rm=TRUE)
    outputS[j, 5, i] <- sd(calc, na.rm=TRUE)
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
    
    outputQM[j, 5, i] <- mean(calc, na.rm=TRUE)
    outputQD[j, 5, i] <- median(calc, na.rm=TRUE)
    outputQS[j, 5, i] <- sd(calc, na.rm=TRUE)
  }
}

data <- NULL
for(j in 1:nComb){
  for(t in 1:type){
    for(l in 1:length(swRange)){
      data <- rbind(data, cbind(j, t, l, outputM[j, t, l], outputD[j, t, l],
              outputS[j, t, l]))
    }
  }
}

data <- data.table(data)
names(data) <- c("comb", "type", "range", "meanValue", "medianValue", "sdValue")

write.table(data, file=paste0(outputDir,"/individual-sw.csv"),
    append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)


data <- NULL
for(j in 1:nComb){
  for(t in 1:type){
    for(l in 1:length(qtRange)){
      data <- rbind(data, cbind(j, t, l, outputQM[j, t, l], outputQD[j, t, l],
              outputQS[j, t, l]))
    }
  }
}

data <- data.table(data)
names(data) <- c("comb", "type", "range", "meanValue", "medianValue", "sdValue")

write.table(data, file=paste0(outputDir,"/individual-qt.csv"),
    append=FALSE, quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)
