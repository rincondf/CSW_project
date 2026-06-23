t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}


Cancol <- t_col("darkgreen", percent = 40)
CSWcol <- t_col("brown", percent = 40)



#Canola

plot(seq(100, 2500), pgamma(seq(100, 2500), shape = 8.5, scale = 14*6))

pgamma(850, shape = 8.5, scale = 14*6)


pgamma(1400, shape = 8.5, scale = 14*6)



# cabbage seedpod weevil


dJohnSB_ph <- function(x) {
  gamma = 1.0737
  delta = 1.2394
  xi = 150
  lambda = 950
  dnorm(gamma + delta * (log((x - xi) / (lambda - (x - xi)))), 0 , 1)
}


bug = dJohnSB_ph(seq(100, 2500))
bug[which(is.nan(bug))] <- 0

crop = pgamma(seq(100, 2500), shape = 8.5, scale = 14*6)


plot(seq(100, 2500), bug, ylim = c(0, 1))

points(seq(100, 2500), crop)








CSW_00 <- function(t, th) {
  
  res = 0
  
  pop <- bug*100
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = resT
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * Sur
    }
    dama[i+1] <- (pop[i] * 0.00012) * crop[i]
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}

par(mar = c(4, 4, 2, 4))

plot(CSW_00(seq(0, 2000), th = 200)[[1]], xlab = "Time (GDD)", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 2000), ylim = c(0, 50), yaxt = "n")

axis(side = 2, at = seq(0, 50, 10), cex.axis = 2, las = 1)

polygon(seq(0, 2400), CSW_00(seq(0, 2000), th = 200)[[1]], col = CSWcol, border = NA)

par(new = TRUE)
plot(crop*100, xlab = "Time (GDD)", 
     ylab = "", cex.lab = 2, type = "l",
     cex.axis = 2, lwd  = 2, xlim = c(0, 2000), ylim = c(0, 100), yaxt = "n")

axis(side = 4, at = seq(0, 100, 20), cex.axis = 2, las = 1)
polygon(c(seq(0, 2400), 2400), c(crop*100, 0), col = Cancol, border = NA)


plot(CSW_00(seq(0, 2000), th = 200)[[2]], xlab = "Time (GDD)", 
     ylab = "Cumulative damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 2000), ylim = c(0, 1))

f <- seq(1, 2000)

Canola_A <- function(x) {
  
  ires = 350
  
  t <- seq(1, 2000)
  
  res <- rep(NA, length(x))
  
  pop <- bug*100
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.00012) * crop[i]
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPLCanA <- rep(NA, length(f))

for(i in 1: length(f)){
  SPLCanA[i] <- Canola_A(f[i])
}

SPLCanA[which.min(SPLCanA)]


par(mar = c(4, 4, 2, 2))
plot(f, SPLCanA*100, xlab = "Time (GDD)", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 2000), ylim = c(0, 30))
abline(h = SPLCanA[which.min(SPLCanA)]*100, lty = 2)

abline(v = which.min(SPLCanA), lty = 2)






























