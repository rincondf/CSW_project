

can_pods <- pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                   scale = coef(mod_pods)[2])


can_pods <- sample(can_pods, 351, replace = FALSE)
can_pods <- can_pods[order(can_pods)]


sensi_pest <- function(n) {
  
  weevils <- dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                    scale = coef(mod_CSW1)[2]) * n
  
  
  CSW_dam <- function(t) {
    
    res = 0
    
    pop <- weevils
    
    dama <- rep(NA, length(t))
    dama[1] <- 0
    
    dama1 <- rep(NA, length(t))
    dama1[1] <- 0
    
    apl <- 0
    
    for(i in 1: length(t)) {
      dama[i+1] <- (pop[i] * 0.00264) * can_pods[i]
      dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 600)))
      
    }
    
    dama1
    
  }
  
  
  
  ###########
  
  f <- seq(50, 400)
  
  Canola_A <- function(x) {
    
    ires = 100
    
    t <- seq(1, 350)
    
    res <- rep(NA, length(x))
    
    pop <- weevils
    
    dama <- rep(NA, length(t))
    dama[1] <- 0
    
    dama1 <- rep(NA, length(t))
    dama1[1] <- 0
    
    x <- x - 49
    
    for(i in 1: length(t)) {
      
      if(i >= x & i <= (x + ires)) {
        apl <- pop[x]
        pop[i+1] <- 0.01 * apl
      }
      
      
      dama[i+1] <- (pop[i] * 0.00264) * can_pods[i]
      dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 600)))
    }
    
    list(dama1[length(t)], sum(pop))
    
  }
  
  SPLCanA <- rep(NA, length(f))
  
  for(i in 1: length(f)){
    SPLCanA[i] <- Canola_A(f[i])[[1]]
  }
  
  
  which.min(SPLCanA) + 49
  
  
  
}


sensi_pest(100000)





