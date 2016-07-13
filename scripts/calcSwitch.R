##
## Calculate Switch Region
##
## Author......: Luis Gustavo Nardin
## Last Change.: 06/23/2016
##
calc_iswitch <- function(h, bs, rho, g, l, k, payoffs){
  
  iswitch <- NULL
  iState <- NULL
  pI <- 0
  pUs <- NULL
  pUp <- NULL
  n <- 0
  
  for(i in seq(0.00001, 1.0, 0.00001)){
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
      ds <- (((-1/l)*exp(-l * Tss)) - (-1/l))
      di <- (((-1/l)*exp(-l * (Tss+Tis))) - ((-1/l)*exp(-l * Tss)))
      dr <- (((-1/l)*exp(-l * (Tss+Tis+Trs))) - ((-1/l)*exp(-l * (Tss+Tis))))
      
      Us <- (ds * payoffs[1]) + (di * payoffs[3]) + (dr * payoffs[4])
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
      dp <- (((-1/l)*exp(-l * Tpp)) - (-1/l))
      di <- (((-1/l)*exp(-l * (Tpp+Tip))) - ((-1/l)*exp(-l * Tpp)))
      dr <- (((-1/l)*exp(-l * (Tpp+Tip+Trp))) - ((-1/l)*exp(-l * (Tpp+Tip))))
      
      Up <- (dp * payoffs[2]) + (di * payoffs[3]) + (dr * payoffs[4])
    }
    
    if (is.null(pUs)){
      pUs <- Us
    }
    
    if (is.null(pUp)){
      pUp <- Up
    }
    
    if (is.null(iState)){
      if (Us > Up){
        # Susceptible
        iState <- 0
      } else {
        # Prophylactic
        iState <- 1
      }
    }
    
    if (((Us >= Up) && (pUs < pUp)) || ((Up >= Us) && (pUp < pUs))) {
      iswitch <- rbind(iswitch, cbind(h, bs, rho, g, l, k, pI, i, iState, n))
      
      if ((Us >= Up) && (pUs < pUp)){
        # Susceptible
        iState <- 0
      } else if ((Up >= Us) && (pUp < pUs)){
        # Prophylactic
        iState <- 1
      }
      
      pI <- i
      pUs <- Us
      pUp <- Up
      n <- n + 1
    }
  }
  
  i <- 1
  iswitch <- rbind(iswitch, cbind(h, bs, rho, g, l, k, pI, i, iState, n))
  
  return(iswitch)
}
