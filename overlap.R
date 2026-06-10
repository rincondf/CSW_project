# Overlap between CSW and canola

load("CSWmodel.RData")
load("canola_model.RData")

library(bbmle)

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

App1col <- t_col("gold4", percent = 80)
App2col <- t_col("blue3", percent = 80)


par(mar = c(4.5, 6, 6, 2))

plot(seq(200, 700), pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                         scale = coef(mod_pods)[2]), xlab = "", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(200, 700), ylim = c(0, 1), yaxt = "n", xaxt = "n")


mtext("Canola growing degree days", side = 1, cex = 2, line = 3, col = "darkgreen")
axis(side = 1, at = seq(200, 700, 100), cex.axis = 2, col.axis = "darkgreen")

axis(side = 2, at = seq(0, 1, 0.1), cex.axis = 2, las = 1)

polygon(c(seq(200, 700), 700), c(pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                            scale = coef(mod_pods)[2]), 0), col = Cancol, border = NA)

title(ylab = "Proportion", cex.lab = 2, line = 4)

par(new = TRUE)
plot(seq(50, 400), dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                          scale = coef(mod_CSW1)[2])*50, 
     ylab = "", cex.lab = 2, type = "l", xlab = "",
     cex.axis = 2, lwd  = 2, ylim = c(0, 1), xlim = c(50, 400), yaxt = "n",
     xaxt = "n")

axis(side = 3, at = seq(50, 400, 50), cex.axis = 2, col.axis = "brown")
polygon(seq(50, 400), dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                             scale = coef(mod_CSW1)[2])*50, col = CSWcol, border = NA)
mtext("Cabbage seedpod weevil degree days", side = 3, cex = 2, line = 3, col = "brown")

#### ***RUN AFTER FINDING OPTIMUM TIMES***


polygon(c((which.min(SPLCanA) + 49), (which.min(SPLCanA) + 49), (which.min(SPLCanA) + 49 + 100), 
          (which.min(SPLCanA) + 49 + 100)), c(0, 1, 1, 0), col = App1col, border = NA)


polygon(c((which.min(SPLCanB) + 49), (which.min(SPLCanB) + 49), (which.min(SPLCanB) + 49 + 100), 
          (which.min(SPLCanB) + 49 + 100)), c(0, 1, 1, 0), col = App2col, border = NA)

############


weevils <- dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                  scale = coef(mod_CSW1)[2])*7000000

can_pods <- pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                   scale = coef(mod_pods)[2])

# Making weevils and pods the same size

can_pods <- sample(can_pods, 351, replace = FALSE)
can_pods <- can_pods[order(can_pods)]


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

par(mar = c(5, 8, 3, 2))

plot(seq(50, 400), CSW_dam(seq(1, 350)), xlab = "Cabbage seedpod weevil degree days", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(50, 400), ylim = c(0, 650), yaxt = "n")

axis(side = 2, at = seq(0, 600, 100), cex.axis = 2, las = 1)
title(ylab = "Cumulative yield loss per ha (kg)", cex.lab = 2, line = 5)


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

SPLCanA[which.min(SPLCanA)]


par(mar = c(5, 7, 6, 2))
plot(f, SPLCanA, xlab = "", 
     ylab = "", cex.lab = 2, type = "l", col = "gold4",
     cex.axis = 2, lwd  = 2, xlim = c(50, 400), ylim = c(0, 650), yaxt = "n",
     xaxt = "n")

axis(side = 2, at = seq(0, 600, 100), cex.axis = 2, las = 1)
title(ylab = "Cumulative yield loss per ha (kg)", cex.lab = 2, line = 5)

mtext("Canola growing degree days", side = 1, cex = 2, line = 3, col = "darkgreen")
axis(side = 1, at = seq(50, 400, 70), labels = seq(200, 700, 100), cex.axis = 2, col.axis = "darkgreen")


axis(side = 3, at = seq(50, 400, 50), cex.axis = 2, col.axis = "brown")
mtext("Cabbage seedpod weevil degree days", side = 3, cex = 2, line = 3, col = "brown")

abline(h = SPLCanA[which.min(SPLCanA)], lty = 2)

abline(v = which.min(SPLCanA) + 49, lty = 2)




SPLCanB <- rep(NA, length(f))

for(i in 1: length(f)){
  SPLCanB[i] <- Canola_A(f[i])[[2]]
}

SPLCanB[which.min(SPLCanB)]


par(mar = c(5, 7, 6, 2))
plot(f, SPLCanB, xlab = "", 
     ylab = "", cex.lab = 2, type = "l", col = "blue3",
     cex.axis = 2, lwd  = 2, xlim = c(50, 400), ylim = c(0, 7000000), yaxt = "n",
     xaxt = "n")

axis(side = 2, at = c(0, 7000000), labels = c("0", "max"), cex.axis = 2, las = 1)
title(ylab = "Cumulative pest population", cex.lab = 2, line = 5)

mtext("Canola growing degree days", side = 1, cex = 2, line = 3, col = "darkgreen")
axis(side = 1, at = seq(50, 400, 70), labels = seq(200, 700, 100), cex.axis = 2, col.axis = "darkgreen")


axis(side = 3, at = seq(50, 400, 50), cex.axis = 2, col.axis = "brown")
mtext("Cabbage seedpod weevil degree days", side = 3, cex = 2, line = 3, col = "brown")

abline(h = SPLCanB[which.min(SPLCanB)], lty = 2)

abline(v = which.min(SPLCanB) + 49, lty = 2)

















#######################
#####################


Canola_B <- function(x) {
  
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
  
  pop
  
}

par(new = TRUE)

plot(f, Canola_B(which.min(SPLCanA) + 49), type = "l")
lines(f, Canola_B(which.min(SPLCanB) + 49))







Canola_C <- function(x) {
  
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
  
  dama1
  
}




par(new = TRUE)
plot(f, Canola_C(167), type = "l")
plot(f, Canola_C(121), type = "l")


