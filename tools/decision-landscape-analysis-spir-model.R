library(data.table)
library(lattice)
library(rgl)

H <- seq(1,100)
I <- seq(0.01,1.00,0.01)

payoffs = c(1, 0.99, 0, 1)
bs <- 0.1
bp <- 0.01
g <- 0.05

data <- NULL
for(h in H){
  for(i in I){
    p <- i * bs
    Tss <- (1 - ((1 - p)^h)) / p
    if (p != g){
      Tis <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - (((1 - p)^h) / g)
    } else {
      Tis <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - (((1 - p)^h) / g)
    }
    Trs <- h - Tss -Tis
    Us <- (payoffs[1] * Tss) + (payoffs[3] * Tis) + (payoffs[4] * Trs)
    
    p <- i * bp
    Tpp <- (1 - ((1 - p)^h)) / p
    if (p != g){
      Tip <- (1 / g) - (((p * ((1 - g)^h)) / (g * (p - g))) * (1 - (((1 - p) / (1 - g))^h))) - (((1 - p)^h) / g)
    } else {
      Tip <- (1 / g) - ((p * h * ((1 - g)^(h-1))) / g) - (((1 - p)^h) / g)
    }
    Trp <- h - Tpp - Tip
    Up <- (payoffs[2] * Tpp) + (payoffs[3] * Tip) + (payoffs[4] * Trp)
    
    data <- rbind(data, cbind(h, i*100, Us, Up))
  }
}

colnames(data) <- c("h","i","Us","Up")
data <- data.table(data)

##
## Interactive 3D plot
##
plot3d(data$h, data$i, data$Up, col="red")
plot3d(data$h, data$i, data$Us, add = TRUE)

##
## Passive 3D plot
##
grid <- expand.grid(x=H, y=I*100)
wireframe(data$Us + data$Up ~ y + x, grid, xlab="H", ylab="i", zlab="Us (Blue) \n\n\n Up (Red)",
          arrows=FALSE, scales=list(arrows=FALSE), aspect=c(1, 1), col=c("blue","red"))
