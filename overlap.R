# Overlap between CSW and canola

load("pods.RData")
load("CSWdata.RData")

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


### Canola model

par(mar = c(5, 7, 2, 2))
plot(pods$DDs1, pods$props, xlim = c(300, 800), cex.axis = 2, yaxt = "n", 
     lwd = 2, xlab = "Canola growing degree days", cex.lab = 2, ylab = "", ylim = c(0, 0.12))
lines(seq(300, 800), dgamma(seq(300, 800), shape = coef(mod_pods)[1], scale = coef(mod_pods)[2])*15, lwd = 2)
mtext("Proportion of seedpods formed", side = 2, cex = 2, line = 5)
axis(side = 2, at = seq(0, 0.12, 0.02), cex.axis = 2, las = 2)

### CSW model

par(mar = c(5, 7, 2, 2))
plot(all.data$DDs, all.data$props, xlim = c(10, 350), cex.axis = 2, yaxt = "n", 
     lwd = 2, xlab = "Cabbage seedpod weevil degree days", cex.lab = 2, ylab = "", ylim = c(0, 0.55))
lines(seq(15, 350), dgamma(seq(15, 350), shape = coef(mod_CSW1)[1], scale = coef(mod_CSW1)[2])*35, lwd = 2)

mtext("Proportion of adults emerged", side = 2, cex = 2, line = 5)
axis(side = 2, at = seq(0, 0.55, 0.1), cex.axis = 2, las = 2)


#####

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

arrows((which.min(SPLCanA) + 49), 1.04, (which.min(SPLCanA) + 49), -0.04, 
       col = "gold4", lwd = 2, code = 3, length = 0.1, xpd = NA)

arrows((which.min(SPLCanB) + 49), 1.04, (which.min(SPLCanB) + 49), -0.04, 
       col = "blue3", lwd = 2, code = 3, length = 0.1, xpd = NA)


############


weevils <- dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                  scale = coef(mod_CSW1)[2])*7000

can_pods <- pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                   scale = coef(mod_pods)[2])


# Making weevil and and canola development the same size

can_pods <- pgamma(((seq(50, 400) * 1.428) + 128.6), 
                   shape = coef(mod_pods)[1], 
                   scale = coef(mod_pods)[2])



CSW_dam <- function(t) {
  
  res = 0
  
  pop <- weevils
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    dama[i+1] <- (pop[i] * 0.132) * can_pods[i]
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 40)))
  }
  
  dama1
  
}

par(mar = c(5, 8, 3, 2))

plot(seq(50, 400), CSW_dam(seq(1, 350)), xlab = "Cabbage seedpod weevil degree days", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(50, 400), ylim = c(0, 40), yaxt = "n")

axis(side = 2, at = seq(0, 40, 10), cex.axis = 2, las = 1)
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
    
    
    dama[i+1] <- (pop[i] * 0.132) * can_pods[i]
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 40)))
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
     cex.axis = 2, lwd  = 2, xlim = c(50, 400), ylim = c(0, 40), yaxt = "n",
     xaxt = "n")

axis(side = 2, at = seq(0, 40, 10), cex.axis = 2, las = 1)
title(ylab = "Cumulative yield loss (%)", cex.lab = 2, line = 5)

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
     cex.axis = 2, lwd  = 2, xlim = c(50, 400), ylim = c(0, 7000), yaxt = "n",
     xaxt = "n")

axis(side = 2, at = c(0, 7000), labels = c("0", "max"), cex.axis = 2, las = 1)
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


