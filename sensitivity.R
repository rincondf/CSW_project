# POPULATION SIZE

sensitA <- function(n){
  
  weevils <- dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                    scale = coef(mod_CSW1)[2])*n
  
  
  # Making weevil and and canola development the same size
  
  can_pods <- pgamma(((seq(50, 400) * 1.428) + 128.6), 
                     shape = coef(mod_pods)[1], 
                     scale = coef(mod_pods)[2])
  
  
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
      
      
      dama[i+1] <- (pop[i] * 0.132) * can_pods[i]
      dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 40)))
    }
    
    list(dama1[length(t)], sum(pop))
    
  }
  
  SPLCanA <- rep(NA, length(f))
  
  for(i in 1: length(f)){
    SPLCanA[i] <- Canola_A(f[i])[[1]]
  }
  
  list(SPLCanA[which.max(SPLCanA)] - SPLCanA[which.min(SPLCanA)], which.min(SPLCanA) + 49)
}


test_vals <- seq(1, 700000, 5000)

t_dif <- rep(NA, length(test_vals))
t_res <- rep(NA, length(test_vals))

for(i in 1: length(test_vals)) {
  a <- sensitA(test_vals[i])
  t_dif[i] <- a[[1]]
  t_res[i] <- a[[2]]
}

plot(test_vals, t_dif)
plot(test_vals, t_res)


# DAMAGE COEFFICIENT

sensitB <- function(d){
  
  weevils <- dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                    scale = coef(mod_CSW1)[2])*7000
  
  
  # Making weevil and and canola development the same size
  
  can_pods <- pgamma(((seq(50, 400) * 1.428) + 128.6), 
                     shape = coef(mod_pods)[1], 
                     scale = coef(mod_pods)[2])
  
  
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
      
      
      dama[i+1] <- (pop[i] * d) * can_pods[i]
      dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 40)))
    }
    
    list(dama1[length(t)], sum(pop))
    
  }
  
  SPLCanA <- rep(NA, length(f))
  
  for(i in 1: length(f)){
    SPLCanA[i] <- Canola_A(f[i])[[1]]
  }
  
  list(SPLCanA[which.max(SPLCanA)] - SPLCanA[which.min(SPLCanA)], which.min(SPLCanA) + 49)
}


test_valsB <- seq(0.01, 4, 0.01)

t_difB <- rep(NA, length(test_valsB))
t_resB <- rep(NA, length(test_valsB))

for(i in 1: length(test_valsB)) {
  a <- sensitB(test_valsB[i])
  t_difB[i] <- a[[1]]
  t_resB[i] <- a[[2]]
}

plot(test_valsB, t_difB)
plot(test_valsB, t_resB)




# INSECTICIDE EFFICACY


sensitC <- function(ef){
  
  weevils <- dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                    scale = coef(mod_CSW1)[2])*7000
  
  
  # Making weevil and and canola development the same size
  
  can_pods <- pgamma(((seq(50, 400) * 1.428) + 128.6), 
                     shape = coef(mod_pods)[1], 
                     scale = coef(mod_pods)[2])
  
  
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
        pop[i+1] <- ef * apl
      }
      
      
      dama[i+1] <- (pop[i] * 0.132) * can_pods[i]
      dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 40)))
    }
    
    list(dama1[length(t)], sum(pop))
    
  }
  
  SPLCanA <- rep(NA, length(f))
  
  for(i in 1: length(f)){
    SPLCanA[i] <- Canola_A(f[i])[[1]]
  }
  
  list(SPLCanA[which.max(SPLCanA)] - SPLCanA[which.min(SPLCanA)], which.min(SPLCanA) + 49)
}


test_valsC <- seq(0.01, 0.4, 0.01)

t_difC <- rep(NA, length(test_valsC))
t_resC <- rep(NA, length(test_valsC))

for(i in 1: length(test_valsC)) {
  a <- sensitC(test_valsC[i])
  t_difC[i] <- a[[1]]
  t_resC[i] <- a[[2]]
}

plot(test_valsC, t_difC)
plot(test_valsC, t_resC)



# INSECTICIDE RESIDUAL TIME

sensitD <- function(r){
  
  weevils <- dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                    scale = coef(mod_CSW1)[2])*7000
  
  
  # Making weevil and and canola development the same size
  
  can_pods <- pgamma(((seq(50, 400) * 1.428) + 128.6), 
                     shape = coef(mod_pods)[1], 
                     scale = coef(mod_pods)[2])
  
  
  ###########
  
  f <- seq(50, 400)
  
  Canola_A <- function(x) {
    
    ires = r
    
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
      
      
      dama[i+1] <- (pop[i] * 0.132) * can_pods[i]
      dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 40)))
    }
    
    list(dama1[length(t)], sum(pop))
    
  }
  
  SPLCanA <- rep(NA, length(f))
  
  for(i in 1: length(f)){
    SPLCanA[i] <- Canola_A(f[i])[[1]]
  }
  
  list(SPLCanA[which.max(SPLCanA)] - SPLCanA[which.min(SPLCanA)], which.min(SPLCanA) + 49)
}


test_valsD <- seq(50, 400, 10)

t_difD <- rep(NA, length(test_valsD))
t_resD <- rep(NA, length(test_valsD))

for(i in 1: length(test_valsD)) {
  a <- sensitD(test_valsD[i])
  t_difD[i] <- a[[1]]
  t_resD[i] <- a[[2]]
}

plot(test_valsD, t_difD)
plot(test_valsD, t_resD)



# OVERLAP SIZE BETWEEN CROP AND PEST MODELS

sensitE <- function(del){
  
  weevils <- dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                    scale = coef(mod_CSW1)[2])*7000
  
  
  # Making weevil and and canola development the same size
  
  can_pods <- pgamma(((seq(50, 400) * 1.428) + del), 
                     shape = coef(mod_pods)[1], 
                     scale = coef(mod_pods)[2])
  
  
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
      
      
      dama[i+1] <- (pop[i] * 0.132) * can_pods[i]
      dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 40)))
    }
    
    list(dama1[length(t)], sum(pop))
    
  }
  
  SPLCanA <- rep(NA, length(f))
  
  for(i in 1: length(f)){
    SPLCanA[i] <- Canola_A(f[i])[[1]]
  }
  
  list(SPLCanA[which.max(SPLCanA)] - SPLCanA[which.min(SPLCanA)], which.min(SPLCanA) + 49)
}


test_valsE <- seq(10, 300, 10)

t_difE <- rep(NA, length(test_valsE))
t_resE <- rep(NA, length(test_valsE))

for(i in 1: length(test_valsE)) {
  a <- sensitE(test_valsE[i])
  t_difE[i] <- a[[1]]
  t_resE[i] <- a[[2]]
}

plot(test_valsE, t_difE)
plot(test_valsE, t_resE)
