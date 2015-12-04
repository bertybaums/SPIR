setwd("/Users/craigmiller/Work/Project--Ebola/Anlaysis of Ebola Stability/test_systems")

files <- list.files()

exp.colnames <- c("mutation", "exp.ddG", "foldX.ddG", "MD.fX.ddG")

file.names.list <- sapply(files, function(x) strsplit(x, split="_"))
fold.files <- which(unlist(lapply(file.names.list, function(x) x[2] == "fold")))
bind.files <- which(unlist(lapply(file.names.list, function(x) x[2] == "bind")))

fold.systems1 <- sapply(names(fold.files), function(x) unlist(strsplit(x, split="_"))[3])
fold.systems2 <- sapply(fold.systems1, function(x) unlist(strsplit(x, split="\\."))[1])
bind.systems1 <- sapply(names(bind.files), function(x) unlist(strsplit(x, split="_"))[3])
bind.systems2 <- sapply(bind.systems1, function(x) unlist(strsplit(x, split="\\."))[1])

systems2 <- fold.systems2
#systems2 <- bind.systems2

system.names <- lapply(file.names.list, function(x) unlist(strsplit(x[3], split="\\."))[1])
n.systems <- length(fold.files)

lm.smry <- as.data.frame(matrix(nrow=length(systems2)+1, ncol=11))
colnames(lm.smry) <- c("Protein", "n.sites", "n.mutations", "FX.Intercept", "FX.Slope", "FX.R2", "FX.rMSE", "MDS.Intercept", "MDS.Slope", "MDS.R2", "MDS.rMSE")
lm.smry$Protein <- c(systems2, "Means")
round.vals <- c(NA, 0,0,2,2,2,2,2,2,2,2)

files.to.use <- fold.files
#files.to.use <- bind.files

pdf(file="Folding ddG experimental systems ().pdf", width=15, height=7)
#pdf(file="Binding ddG experimental systems.pdf", width=15, height=7)
layout(mat=matrix(nrow=2, ncol=5, data=seq(1,10), byrow=TRUE), heights=rep(3,2), widths=rep(3,5)) -> l
#layout.show(l)


for (file.i in 1:length(files.to.use)){
	data <- read.table(file=files[files.to.use[file.i]])
	colnames(data) <- exp.colnames
	
	max.E <- max(data$exp.ddG)
	max.F <- max(data$foldX.ddG)
	max.M <- max(data$MD.fX.ddG)
	min.E <- min(data$exp.ddG)
	min.F <- min(data$foldX.ddG)
	min.b <- min(min.E, min.F)
	max.b <- max(max.E, max.F)
	
	
	lm <- lm(data$exp.ddG ~ data$foldX.ddG)
	lm2 <- lm(data$exp.ddG ~ data$MD.fX.ddG )


	par(mar=c(5,5,4,1))
	grey.col <- "grey75"
	pt.cex <- 0.8
	leg.loc <- "bottomright"
	scale <- 1.2
	plot(data$MD.fX.ddG, data$exp.ddG, pch=24, bg="black", cex=pt.cex, xlim=c(min.b*scale, max.b*scale), ylim=c(min.b*scale, max.b*scale), xlab=expression(paste(Delta, Delta, G, " Predicted", " (kcal/mol)")), ylab=expression(paste(Delta, Delta, G, " Experimentally Observed", " (kcal/mol)")), main=systems2[file.i])
	abline(0,1, lty="solid", col=grey.col, lwd=3)
	points(data$foldX.ddG, data$exp.ddG, pch=4, bg="white", cex=pt.cex)
	#abline(0,0, lty="dotted", col="grey80")
	#abline(v=0, lty="dotted", col="grey80")
	abline(coefficients(lm), lty="dashed")
	abline(coefficients(lm2))
	points(data$foldX.ddG, data$exp.ddG, pch=4, bg="black")
	points(data$MD.fX.ddG, data$exp.ddG, pch=24, bg="black")
	if (file.i == 1){
		legend(leg.loc, legend=c("Perfect Fit", as.expression(bquote(paste("Ex  ", R^2==.(format(round(summary(lm)$r.squared,2), nsmall=2))))), as.expression(bquote(paste("MD ", R^2==.(format(round(summary(lm2)$r.squared,2), nsmall=2)))))), bty="n", lty=c("solid", "dashed", "solid"), col=c(grey.col,"black", "black"), pch=c(NA, 4, 24), pt.bg=c(NA, NA, "black"), lwd=c(3,1,1))
	} else{
		legend(leg.loc, legend=c(as.expression(bquote(paste("Ex  ", R^2==.(format(round(summary(lm)$r.squared,2), nsmall=2))))), as.expression(bquote(paste("MD ", R^2==.(format(round(summary(lm2)$r.squared,2),nsmall=2)))))), bty="n")
	}
	
	
	mean.error.FX <- mean(abs(data$exp.ddG - data$foldX.ddG))
	mean.error.MDS <- mean(abs(data$exp.ddG - data$MD.fX.ddG))
	#mean.error.FX <- mean(abs(data$exp.ddG -  predict(lm)))
	#mean.error.MDS <- mean(abs(data$exp.ddG -  predict(lm2)))
	
	muts <- as.character(data$mutation)
	sites <- sapply(muts, function(x) substring(x, 3, nchar(x)-1))
	
	lm.smry$n.sites[file.i] <- length(unique(sites))
	lm.smry$n.mutations[file.i]<- length(muts)
	lm.smry$FX.Intercept[file.i] <- round(coefficients(lm)[1],2)
	lm.smry$FX.Slope[file.i] <- round(coefficients(lm)[2],2)
	lm.smry$FX.R2[file.i] <- round(summary(lm)$r.squared,2)
	lm.smry$FX.rMSE[file.i] <- round(mean.error.FX,2)
	lm.smry$MDS.Intercept[file.i] <- round(coefficients(lm2)[1],2)
	lm.smry$MDS.Slope[file.i] <- round(coefficients(lm2)[2],2)
	lm.smry$MDS.R2[file.i] <- round(summary(lm2)$r.squared,2)
	lm.smry$MDS.rMSE[file.i] <- round(mean.error.MDS,2)
	
	mean.exp <- mean(data$exp.ddG)
	TSS <- sum((data$exp.ddG - mean.exp)^2)
	
	fX.SS <- sum((data$exp.ddG - data$foldX.ddG)^2)
	fX.Resids <- (data$exp.ddG - data$foldX.ddG)
	MS.SS <- sum((data$exp.ddG - data$MD.fX.ddG)^2)
	MS.resids <- (data$exp.ddG - data$MD.fX.ddG)
	
	R2.fX <- (TSS - fX.SS)/TSS
	R2.MS <- (TSS - MS.SS)/TSS
	
	#exp.ord <- order(data$exp.ddG)
	#plot(data$exp.ddG[exp.ord], pch=21, bg="black", ylim=c(min.b, max.b))
	#points(data$foldX.ddG[exp.ord], pch=1)
	#points(data$MD.fX.ddG[exp.ord], pch=21, bg="deepskyblue")
	#points(data$exp.ddG[exp.ord], pch=21, bg="black")
	
	#plot(data$exp.ddG[exp.ord], y=rep(0, length(data$exp.ddG)), pch=21, bg="red", ylim=c(-5,5))
	#points(data$exp.ddG[exp.ord], fX.Resids, pch=21, bg="yellow")
	
	#n <- length(data$exp.ddG)
	#plot(seq(1, n), rep(0, n), type="l")
	
}

for (col.i in 2:length(lm.smry[1,])){
	colmean <- round(mean(lm.smry[,col.i], na.rm=TRUE), round.vals[col.i])
	lm.smry[n.systems+1,col.i] <- colmean
}

dev.off()

write.table(x=lm.smry, file="Binding Summary Table.txt", col.names=TRUE, row.names=FALSE, sep="\t")


x <- xtable(lm.smry, digits=c(1,round.vals), floating=FALSE, tabular.environment="tabular", hline.after=NULL, include.rownames=FALSE, include.colnames=TRUE, caption="Folding Stability")
print(x, include.rownames=FALSE)




# ======= As above, but no plot. Just to get table. Also adding Bias =======

setwd("/Users/craigmiller/Work/Project--Ebola/Anlaysis of Ebola Stability/test_systems")

files <- list.files()

exp.colnames <- c("mutation", "exp.ddG", "foldX.ddG", "MD.fX.ddG")

file.names.list <- sapply(files, function(x) strsplit(x, split="_"))
fold.files <- which(unlist(lapply(file.names.list, function(x) x[2] == "fold")))
bind.files <- which(unlist(lapply(file.names.list, function(x) x[2] == "bind")))

fold.systems1 <- sapply(names(fold.files), function(x) unlist(strsplit(x, split="_"))[3])
fold.systems2 <- sapply(fold.systems1, function(x) unlist(strsplit(x, split="\\."))[1])
bind.systems1 <- sapply(names(bind.files), function(x) unlist(strsplit(x, split="_"))[3])
bind.systems2 <- sapply(bind.systems1, function(x) unlist(strsplit(x, split="\\."))[1])

systems2 <- fold.systems2
#systems2 <- bind.systems2

system.names <- lapply(file.names.list, function(x) unlist(strsplit(x[3], split="\\."))[1])
n.systems <- length(fold.files)

lm.smry <- as.data.frame(matrix(nrow=length(systems2)+1, ncol=13))
colnames(lm.smry) <- c("Protein", "n.sites", "n.mutations", "FX.bias", "FX.rMSE", "FX.Intercept", "FX.Slope", "FX.R2", "MDS.bias", "MDS.rMSE", "MDS.Intercept", "MDS.Slope", "MDS.R2")
lm.smry$Protein <- c(systems2, "Means")
round.vals <- c(NA, 0,0,2,2,2,2,2,2,2,2)

for (file.i in 1:length(files.to.use)){
	data <- read.table(file=files[files.to.use[file.i]])
	colnames(data) <- exp.colnames
	
	max.E <- max(data$exp.ddG)
	max.F <- max(data$foldX.ddG)
	max.M <- max(data$MD.fX.ddG)
	min.E <- min(data$exp.ddG)
	min.F <- min(data$foldX.ddG)
	min.b <- min(min.E, min.F)
	max.b <- max(max.E, max.F)
	
	
	lm <- lm(data$exp.ddG ~ data$foldX.ddG)
	lm2 <- lm(data$exp.ddG ~ data$MD.fX.ddG )



	
	mean.error.FX <- mean(abs(data$exp.ddG - data$foldX.ddG))
	mean.error.MDS <- mean(abs(data$exp.ddG - data$MD.fX.ddG))
	bias.FX <- mean(data$exp.ddG - data$foldX.ddG)
	bias.MDS <- mean(data$exp.ddG - data$MD.fX.ddG)
	
	muts <- as.character(data$mutation)
	sites <- sapply(muts, function(x) substring(x, 3, nchar(x)-1))
	
	lm.smry$n.sites[file.i] <- length(unique(sites))
	lm.smry$n.mutations[file.i]<- length(muts)
	lm.smry$FX.Intercept[file.i] <- round(coefficients(lm)[1],2)
	lm.smry$FX.Slope[file.i] <- round(coefficients(lm)[2],2)
	lm.smry$FX.R2[file.i] <- round(summary(lm)$r.squared,2)
	lm.smry$FX.rMSE[file.i] <- round(mean.error.FX,2)
	lm.smry$MDS.Intercept[file.i] <- round(coefficients(lm2)[1],2)
	lm.smry$MDS.Slope[file.i] <- round(coefficients(lm2)[2],2)
	lm.smry$MDS.R2[file.i] <- round(summary(lm2)$r.squared,2)
	lm.smry$MDS.rMSE[file.i] <- round(mean.error.MDS,2)
	lm.smry$FX.bias[file.i] <- bias.FX
	lm.smry$MDS.bias[file.i] <- bias.MDS
	
	mean.exp <- mean(data$exp.ddG)
	TSS <- sum((data$exp.ddG - mean.exp)^2)
	
	fX.SS <- sum((data$exp.ddG - data$foldX.ddG)^2)
	fX.Resids <- (data$exp.ddG - data$foldX.ddG)
	MS.SS <- sum((data$exp.ddG - data$MD.fX.ddG)^2)
	MS.resids <- (data$exp.ddG - data$MD.fX.ddG)
	
	R2.fX <- (TSS - fX.SS)/TSS
	R2.MS <- (TSS - MS.SS)/TSS	
}

for (col.i in 2:length(lm.smry[1,])){
	colmean <- round(mean(lm.smry[,col.i], na.rm=TRUE), round.vals[col.i])
	lm.smry[n.systems+1,col.i] <- colmean
}



write.table(x=lm.smry, file="Folding Summary Table v2.txt", col.names=TRUE, row.names=FALSE, sep="\t")


x <- xtable(lm.smry, digits=c(1,round.vals), floating=FALSE, tabular.environment="tabular", hline.after=NULL, include.rownames=FALSE, include.colnames=TRUE, caption="Folding Stability")
print(x, include.rownames=FALSE)









# ============== Jack knife to test for effectiveness of data transformation ===========

stability <- "fold"
if (stability == "fold"){
	files.to.use <- fold.files
	systems2 <- fold.systems2
} else{
	files.to.use <- bind.files
	systems2 <- bind.systems2
}



get.lm.smry <- function(trans.list, trans.data = FALSE, include.v){
	lm.smry <- as.data.frame(matrix(nrow=length(systems2)+1, ncol=11))
	colnames(lm.smry) <- c("Protein", "n.sites", "n.mutations", "FX.Intercept", "FX.Slope", "FX.R2", "FX.rMSE", "MDS.Intercept", "MDS.Slope", "MDS.R2", "MDS.rMSE")
	lm.smry$Protein <- c(systems2, "Means")
	round.vals <- c(NA, 0,0,2,2,2,2,2,2,2,2)	


	for (file.i in 1:length(files.to.use)){
	  if (file.i %in% include.v){
		data <- read.table(file=files[files.to.use[file.i]])
		colnames(data) <- exp.colnames	
		
		if (trans.data == TRUE){
			data$foldX.ddG <- data$foldX.ddG*trans.list$fX.factor + trans.list$fX.off
			data$MD.fX.ddG <- data$MD.fX.ddG*trans.list$MDS.factor + trans.list$MDS.off
		}
	
		lm <- lm(data$exp.ddG ~ data$foldX.ddG)
		lm2 <- lm(data$exp.ddG ~ data$MD.fX.ddG )
	
		mean.error.FX <- mean(abs(data$exp.ddG - data$foldX.ddG))
		mean.error.MDS <- mean(abs(data$exp.ddG - data$MD.fX.ddG))
		#mean.error.FX <- mean(abs(data$exp.ddG -  predict(lm)))
		#mean.error.MDS <- mean(abs(data$exp.ddG -  predict(lm2)))
	
		muts <- as.character(data$mutation)
		sites <- sapply(muts, function(x) substring(x, 3, nchar(x)-1))
	
		lm.smry$n.sites[file.i] <- length(unique(sites))
		lm.smry$n.mutations[file.i]<- length(muts)
		lm.smry$FX.Intercept[file.i] <- round(coefficients(lm)[1],2)
		lm.smry$FX.Slope[file.i] <- round(coefficients(lm)[2],2)
		lm.smry$FX.R2[file.i] <- round(summary(lm)$r.squared,2)
		lm.smry$FX.rMSE[file.i] <- round(mean.error.FX,2)
		lm.smry$MDS.Intercept[file.i] <- round(coefficients(lm2)[1],2)
		lm.smry$MDS.Slope[file.i] <- round(coefficients(lm2)[2],2)
		lm.smry$MDS.R2[file.i] <- round(summary(lm2)$r.squared,2)
		lm.smry$MDS.rMSE[file.i] <- round(mean.error.MDS,2)
	  }
	}

	for (col.i in 2:length(lm.smry[1,])){
		colmean <- round(mean(lm.smry[,col.i], na.rm=TRUE), round.vals[col.i])
		lm.smry[n.systems+1,col.i] <- colmean
	}
	
	fX.off <- lm.smry$FX.Intercept[length(lm.smry[,1])]
	fX.factor <- lm.smry$FX.Slope[length(lm.smry[,1])]
	MDS.off <- lm.smry$MDS.Intercept[length(lm.smry[,1])]
	MDS.factor <- lm.smry$MDS.Slope[length(lm.smry[,1])]
	
	trans.list <- list(fX.off = fX.off, fX.factor = fX.factor, MDS.off = MDS.off, MDS.factor =MDS.factor)
	return(list(lm.smry=lm.smry, trans.list=trans.list))
}

include.v <- seq(1,10)
trans.list.0 <- list(fX.off = 0, fX.factor = 1, MDS.off = 0, MDS.factor = 1)
S.1 <- get.lm.smry(trans.list.0, trans.data=FALSE, include.v)
lm.smry <- S.1$lm.smry
trans.list.1 <- S.1$trans.list

file.i <- 3
data <- read.table(file=files[files.to.use[file.i]])
colnames(data) <- exp.colnames	
t.data <- translate.data(data, trans.list.1)
plot(data$foldX.ddG[1:20], data$exp.ddG[1:20], ylim=c(0,10), xlim=c(0,10))
abline(0,1, lty="dashed")	
points(t.data$foldX.ddG[], t.data$exp.ddG[], pch=21, bg="deepskyblue")	
lm.b <- lm(t.data$exp.ddG ~ t.data$foldX.ddG)
abline(coefficients(lm.b), col="deepskyblue")
lm <- lm(data$exp.ddG ~ data$foldX.ddG)
abline(coefficients(lm), col="black")






# --- OK, now the leave one out approach ---

lou.smry <- as.data.frame(matrix(nrow=length(systems2)+1, ncol=11))
	colnames(lou.smry) <- c("Protein", "n.sites", "n.mutations", "FX.Intercept", "FX.Slope", "FX.R2", "FX.rMSE", "MDS.Intercept", "MDS.Slope", "MDS.R2", "MDS.rMSE")
	lou.smry$Protein <- c(systems2, "Means")
	

S.all <- get.lm.smry(trans.list.0,  trans.data=FALSE, seq(1,10))
trans.list.list <- vector("list", 10)

for (i in 1:10){
	include.v <- seq(1,10)
	include.v <- include.v[-which(include.v == i)]
	S <- get.lm.smry(trans.list.0, trans.data=FALSE, include.v)
	trans.list <- S$trans.list
	trans.list.list[[i]] <- trans.list
	S.one <- get.lm.smry(trans.list, trans.data=TRUE, i)
	lou.smry[i,] <- S.one$lm.smry[i,]
}

for (col.i in 2:length(lou.smry[1,])){
	colmean <- round(mean(lou.smry[,col.i], na.rm=TRUE), round.vals[col.i])
	lou.smry[n.systems+1,col.i] <- colmean
}

t.not.smry <- as.data.frame(cbind(Protein=lou.smry$Protein, FX.rMSE.raw =S.all$lm.smry$FX.rMSE, FX.rMSE.trans=lou.smry$FX.rMSE, MDS.rMSE.raw =S.all$lm.smry$MDS.rMSE, MDS.rMSE.trans=lou.smry$MDS.rMSE))

#x.table <- S.all$lm.smry
x.table <- lou.smry
x <- xtable(x.table, digits=c(1,round.vals), floating=FALSE, tabular.environment="tabular", hline.after=NULL, include.rownames=FALSE, include.colnames=TRUE, caption="Binding Stability.Data transformation based on all datasets except the one it is applied to (i.e. leave one out).")
print(x, include.rownames=FALSE)


x.table <- t.not.smry
digits <- c(NA, 2,2,2,2)
x <- xtable(x.table, digits=c(1,digits), floating=FALSE, tabular.environment="tabular", hline.after=NULL, include.rownames=FALSE, include.colnames=TRUE, caption="Binding Stability. Summary of the effect of raw vs transformed rMSE values.")
print(x, include.rownames=FALSE)



j <- 1
analyze.one.system(trans.list.list[[j]], j)



analyze.one.system <- function(trans.list, i){
	file.i <- i
	data <- read.table(file=files[files.to.use[file.i]])
	colnames(data) <- exp.colnames	
	t.data <- data
	t.data$foldX.ddG <- t.data$foldX.ddG*trans.list$fX.factor + trans.list$fX.off
	t.data$MD.fX.ddG <- t.data$MD.fX.ddG*trans.list$MDS.factor + trans.list$MDS.off
	
	plot(data$MD.fX.ddG, data$exp.ddG)
	abline(0,1, col="grey", lwd=3)
	lm.raw <- lm(data$exp.ddG ~ data$MD.fX.ddG)
	abline(coefficients(lm.raw))
	points(t.data$MD.fX.ddG, t.data$exp.ddG, pch=21, bg="red")
	lm.trans <- lm(t.data$exp.ddG ~ t.data$MD.fX.ddG)
	abline(coefficients(lm.trans), col="red")
}

translate.data <- function(data, trans.list){
	t.data <- data
	t.data$foldX.ddG <- data$foldX.ddG*trans.list$fX.factor + trans.list$fX.off
	t.data$MD.fX.ddG <- data$MD.fX.ddG*trans.list$MDS.factor + trans.list$MDS.off
	return(t.data)
}

plot.data <- function(lm, lm2, data){
	
	max.E <- max(data$exp.ddG)
	max.F <- max(data$foldX.ddG)
	max.M <- max(data$MD.fX.ddG)
	min.E <- min(data$exp.ddG)
	min.F <- min(data$foldX.ddG)
	min.b <- min(min.E, min.F)
	max.b <- max(max.E, max.F)
	
	par(mar=c(5,5,4,1))
	grey.col <- "grey70"
	pt.cex <- 0.8
	leg.loc <- "bottomright"
	scale <- 1.2
	plot(data$MD.fX.ddG, data$exp.ddG, pch=24, bg="black", cex=pt.cex, xlim=c(min.b*scale, max.b*scale), ylim=c(min.b*scale, max.b*scale), xlab=expression(paste(Delta, Delta, G["Predicted"], " (kcal/mol)")), ylab=expression(paste(Delta, Delta, G[" Experimentaly Observed"], " (kcal/mol)")), main=systems2[file.i])
	points(data$foldX.ddG, data$exp.ddG, pch=4, bg="white", cex=pt.cex)
	#abline(0,0, lty="dotted", col="grey80")
	#abline(v=0, lty="dotted", col="grey80")
	abline(coefficients(lm), lty="dashed")
	abline(coefficients(lm2))
	points(data$foldX.ddG, data$exp.ddG, pch=4, bg="black")
	points(data$MD.fX.ddG, data$exp.ddG, pch=24, bg="black")
	abline(0,1, lty="solid", col=grey.col, lwd=2)
	if (file.i == 1){
		legend(leg.loc, legend=c("Perfect Fit", as.expression(bquote(paste("ExS  ", R^2==.(round(summary(lm)$r.squared,2))))), as.expression(bquote(paste("MDS  ", R^2==.(round(summary(lm2)$r.squared,2)))))), bty="n", lty=c("solid", "dashed", "solid"), col=c(grey.col,"black", "black"), pch=c(NA, 4, 24), pt.bg=c(NA, NA, "black"), lwd=c(2,1,1))
	} else{
		legend(leg.loc, legend=c(as.expression(bquote(paste("ExS  ", R^2==.(round(summary(lm)$r.squared,2))))), as.expression(bquote(paste("MDS  ", R^2==.(round(summary(lm2)$r.squared,2)))))), bty="n")
	}
}



m <- matrix(nrow=3, ncol=2, data=c(1,4,3,5, 5,6), byrow=TRUE)
plot(m[,1], m[,2], xlim=c(0,12), ylim=c(0,12))
lm.m <- lm(m[,2]~ m[,1])
abline(coefficients(lm.m))
coes <- coefficients(lm.m)
m2 <- m
m2[,1] <- m[,1]*coes[2] + coes[1]
points(m2[,1], m2[,2], pch=21, bg="red")
abline(0,1, col="grey", lwd=3)
# So the transform is to simply run the data (X) through mX + b




file.i <- 1
data <- read.table(file=files[files.to.use[file.i]])
colnames(data) <- exp.colnames
lm.c <- lm(data$exp.ddG ~ data$foldX.ddG)	
coes <- coefficients(lm.c)
t.data <- data
t.data$foldX.ddG <- data$foldX.ddG*coes[2] + coes[1]

 plot(data$foldX.ddG, data$exp.ddG, ylim=c(0,15), clim=c(0,15))
 abline(0,1, col="grey", lwd=3)
 points(t.data$foldX.ddG, t.data$exp.ddG, pch=21, bg="deepskyblue")
 abline(coes, lty="dashed")
 lm.d <- lm(t.data$exp.ddG ~ t.data$foldX.ddG)	
 abline(coefficients(lm.d), col="deepskyblue")
		
