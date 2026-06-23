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


test_vals <- seq(1, 300000, 1000)

t_dif <- rep(NA, length(test_vals))
t_res <- rep(NA, length(test_vals))

for(i in 1: length(test_vals)) {
  a <- sensitA(test_vals[i])
  t_dif[i] <- a[[1]]
  t_res[i] <- a[[2]]
}


par(mfrow = c(1, 2))

par(mar = c(5, 7, 3, 2))
plot(test_vals, t_dif, xlim = c(0, 300000), cex.axis = 2, yaxt = "n", type = "o",
     lwd = 2, xlab = "Cabbage seedpod weevil population size", cex.lab = 2, ylab = "", ylim = c(0, 25))

axis(side = 2, at = seq(0, 25, 5), cex.axis = 2, las = 2)
mtext("Max-min resulting yield loss\ndifference", side = 2, cex = 2, line = 3.5)

mtext("A", side = 3, cex = 2, adj = 0, line = 1)
abline(v = 7000, lty = 2)

par(mar = c(5, 7, 3, 2))
plot(test_vals, t_res, xlim = c(0, 300000), cex.axis = 2, yaxt = "n", type = "o",
     lwd = 2, xlab = "Cabbage seedpod weevil population size", cex.lab = 2, ylab = "", ylim = c(0, 200))

axis(side = 2, at = seq(0, 200, 20), cex.axis = 2, las = 2)
mtext("Optimal spray time", side = 2, cex = 2, line = 5)
mtext("B", side = 3, cex = 2, adj = 0, line = 1)

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




par(mfrow = c(1, 2))

par(mar = c(5, 7, 3, 2))
plot(test_valsB, t_difB, xlim = c(0, 4), cex.axis = 2, yaxt = "n", type = "o",
     lwd = 2, xlab = "Damage coefficient (%)", cex.lab = 2, ylab = "", ylim = c(0, 25))

axis(side = 2, at = seq(0, 25, 5), cex.axis = 2, las = 2)
mtext("Max-min resulting yield loss\ndifference", side = 2, cex = 2, line = 3.5)

mtext("A", side = 3, cex = 2, adj = 0, line = 1)
abline(v = 0.134, lty = 2)

par(mar = c(5, 7, 3, 2))
plot(test_valsB, t_resB, xlim = c(0, 4), cex.axis = 2, yaxt = "n", type = "o",
     lwd = 2, xlab = "Damage coefficient (%)", cex.lab = 2, ylab = "", ylim = c(0, 200))

axis(side = 2, at = seq(0, 200, 20), cex.axis = 2, las = 2)
mtext("Optimal spray time", side = 2, cex = 2, line = 5)
mtext("B", side = 3, cex = 2, adj = 0, line = 1)



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







sensitCA <- function(ef){
  
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
    
    list(dama1[length(t)], pop)
    
  }
  
  SPLCanA <- rep(NA, length(f))
  
  for(i in 1: length(f)){
    SPLCanA[i] <- Canola_A(f[i])[[1]]
  }
  
  list(Canola_A(f[which.min(SPLCanA)])[[2]], which.min(SPLCanA) + 49)
}




plot(sensitCA(0.9)[[1]], type = "l", ylim = c(0, 100))


######




test_valsC <- seq(0.01, 0.6, 0.01)

t_difC <- rep(NA, length(test_valsC))
t_resC <- rep(NA, length(test_valsC))

for(i in 1: length(test_valsC)) {
  a <- sensitC(test_valsC[i])
  t_difC[i] <- a[[1]]
  t_resC[i] <- a[[2]]
}





par(mfrow = c(1, 2))

par(mar = c(5, 7, 3, 2))
plot(test_valsC, t_difC, xlim = c(0, 0.6), cex.axis = 2, yaxt = "n", xaxt = "n", 
     type = "o", lwd = 2, xlab = "Insecticide efficacy (%)", cex.lab = 2, ylab = "", 
     ylim = c(0, 25))

axis(side = 2, at = seq(0, 25, 5), cex.axis = 2, las = 2)
axis(side = 1, at = seq(0, 0.6, 0.1), labels = seq(100, 40, -10), cex.axis = 2)

mtext("Max-min resulting yield loss\ndifference", side = 2, cex = 2, line = 3.5)

mtext("A", side = 3, cex = 2, adj = 0, line = 1)
abline(v = 0.01, lty = 2)

par(mar = c(5, 7, 3, 2))
plot(test_valsC, t_resC, xlim = c(0, 0.6), cex.axis = 2, yaxt = "n", , xaxt = "n",
     type = "o", lwd = 2, xlab = "Insecticide efficacy (%)", cex.lab = 2, ylab = "", ylim = c(0, 200))

axis(side = 2, at = seq(0, 200, 20), cex.axis = 2, las = 2)
axis(side = 1, at = seq(0, 0.6, 0.1), labels = seq(100, 40, -10), cex.axis = 2)

mtext("Optimal spray time", side = 2, cex = 2, line = 5)
abline(v = 0.01, lty = 2)
mtext("B", side = 3, cex = 2, adj = 0, line = 1)





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



par(mfrow = c(1, 2))

par(mar = c(6.5, 7, 3, 2))
plot(test_valsD, t_difD, xlim = c(50, 400), cex.axis = 2, yaxt = "n", xaxt = "n", 
     type = "o", lwd = 2, xlab = "",
     cex.lab = 2, ylab = "", 
     ylim = c(0, 35))

axis(side = 2, at = seq(0, 35, 5), cex.axis = 2, las = 2)
axis(side = 1, at = seq(50, 400, 30), cex.axis = 2)

mtext("Max-min resulting yield loss\ndifference", side = 2, cex = 2, line = 3.5)
mtext("Insecticide residual time\n(cabbage seedpod weevil degree days)", 
      side = 1, cex = 2, line = 5)

mtext("A", side = 3, cex = 2, adj = 0, line = 1)
abline(v = 100, lty = 2)

par(mar = c(6.5, 7, 3, 2))
plot(test_valsD, t_resD, xlim = c(50, 400), cex.axis = 2, yaxt = "n", , xaxt = "n",
     type = "o", lwd = 2, xlab = "", 
     cex.lab = 2, ylab = "", ylim = c(0, 200))

axis(side = 2, at = seq(0, 200, 20), cex.axis = 2, las = 2)
axis(side = 1, at = seq(50, 400, 30), cex.axis = 2)

mtext("Optimal spray time", side = 2, cex = 2, line = 5)
mtext("Insecticide residual time\n(cabbage seedpod weevil degree days)", 
      side = 1, cex = 2, line = 5)
abline(v = 100, lty = 2)
mtext("B", side = 3, cex = 2, adj = 0, line = 1)







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





par(mfrow = c(1, 2))

par(mar = c(6.5, 7, 3, 2))
plot(test_valsE, t_difE, xlim = c(0, 300), cex.axis = 2, yaxt = "n", xaxt = "n", 
     type = "o", lwd = 2, xlab = "",
     cex.lab = 2, ylab = "", 
     ylim = c(0, 30))

axis(side = 2, at = seq(0, 30, 5), cex.axis = 2, las = 2)
axis(side = 1, at = seq(0, 300, 30), cex.axis = 2)

mtext("Max-min resulting yield loss\ndifference", side = 2, cex = 2, line = 3.5)
mtext("Overlap size\n(cabbage seedpod weevil degree days)", 
      side = 1, cex = 2, line = 5)

mtext("A", side = 3, cex = 2, adj = 0, line = 1)
abline(v = 128.6, lty = 2)

par(mar = c(6.5, 7, 3, 2))
plot(test_valsE, t_resE, xlim = c(0, 300), cex.axis = 2, yaxt = "n", , xaxt = "n",
     type = "o", lwd = 2, xlab = "", 
     cex.lab = 2, ylab = "", ylim = c(0, 250))

axis(side = 2, at = seq(0, 250, 40), cex.axis = 2, las = 2)
axis(side = 1, at = seq(0, 300, 30), cex.axis = 2)

mtext("Optimal spray time", side = 2, cex = 2, line = 5)
mtext("Overlap size\n(cabbage seedpod weevil degree days)", 
      side = 1, cex = 2, line = 5)
abline(v = 128.6, lty = 2)
mtext("B", side = 3, cex = 2, adj = 0, line = 1)




