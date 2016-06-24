##
## Calculate Expected Times
##
## Author......: Luis Gustavo Nardin
## Last Change.: 04/20/2016
##
calc_expectedTime <- function(h, i, bs, rho, g, l, k, payoffs){
  
  p <- i^(1/k) * bs
  Tss <- ((1 / p) - 1) * (1 - (1 - p)^h)
  if (p != g){
    Tis <- ((1 - g) / g) * (1 - (1 - p)^h - (((p * (1 - g)) / (p - g)) * ((1 - g)^h - (1 - p)^h)))
  } else {
    Tis <- Tss - (h * (1 - p)^(h + 1))
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
  Tpp <- ((1 / p) - 1) * (1 - (1 - p)^h)
  if (p != g){
    Tip <- ((1 - g) / g) * (1 - (1 - p)^h - (((p * (1 - g)) / (p - g)) * ((1 - g)^h - (1 - p)^h)))
  } else {
    Tip <- Tpp - (h * (1 - p)^(h + 1))
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

  return(c(h, i, rho, l, k, Tss, Tis, Trs, Us, Tpp, Tip, Trp, Up))
}