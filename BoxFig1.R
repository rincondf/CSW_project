### BOX FIGURE 1

layout(matrix(c(1, 1, 
                2, 3), nrow = 2, byrow = TRUE))

# A

par(mar = c(4.5, 6, 5.5, 2))

plot(seq(200, 700), pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                           scale = coef(mod_pods)[2]), xlab = "", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(200, 700), ylim = c(0, 1), yaxt = "n", xaxt = "n")


mtext("A", side = 3, cex = 2, at = 165, line = 3)

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

####

polygon(c((which.min(SPLCanA) + 49), (which.min(SPLCanA) + 49), (which.min(SPLCanA) + 49 + 100), 
          (which.min(SPLCanA) + 49 + 100)), c(0, 1, 1, 0), col = App1col, border = NA)


polygon(c((which.min(SPLCanB) + 49), (which.min(SPLCanB) + 49), (which.min(SPLCanB) + 49 + 100), 
          (which.min(SPLCanB) + 49 + 100)), c(0, 1, 1, 0), col = App2col, border = NA)


# B

par(mar = c(5, 7, 5.5, 2))
plot(f, SPLCanA, xlab = "", 
     ylab = "", cex.lab = 2, type = "l", col = "gold4",
     cex.axis = 2, lwd  = 2, xlim = c(50, 400), ylim = c(0, 650), yaxt = "n",
     xaxt = "n")

mtext("B", side = 3, cex = 2, at = 15, line = 3)

axis(side = 2, at = seq(0, 600, 100), cex.axis = 1.5, las = 1)
title(ylab = "Cumulative yield loss per ha (kg)", cex.lab = 1.5, line = 4.5)

mtext("Canola growing degree days", side = 1, cex = 1.5, line = 3, col = "darkgreen")
axis(side = 1, at = seq(50, 400, 70), labels = seq(200, 700, 100), cex.axis = 1.5, col.axis = "darkgreen")


axis(side = 3, at = seq(50, 400, 50), cex.axis = 1.5, col.axis = "brown")
mtext("Cabbage seedpod weevil degree days", side = 3, cex = 1.5, line = 3, col = "brown")

abline(h = SPLCanA[which.min(SPLCanA)], lty = 2)

abline(v = which.min(SPLCanA) + 49, lty = 2)

# C


par(mar = c(5, 7, 5.5, 2))
plot(f, SPLCanB, xlab = "", 
     ylab = "", cex.lab = 2, type = "l", col = "blue3",
     cex.axis = 1.5, lwd  = 2, xlim = c(50, 400), ylim = c(0, 7000000), yaxt = "n",
     xaxt = "n")

mtext("C", side = 3, cex = 2, at = 15, line = 3)

axis(side = 2, at = c(0, 7000000), labels = c("0", "max"), cex.axis = 1.5, las = 1)
title(ylab = "Cumulative pest population", cex.lab = 1.5, line = 4.5)

mtext("Canola growing degree days", side = 1, cex = 1.5, line = 3, col = "darkgreen")
axis(side = 1, at = seq(50, 400, 70), labels = seq(200, 700, 100), cex.axis = 1.5, col.axis = "darkgreen")


axis(side = 3, at = seq(50, 400, 50), cex.axis = 1.5, col.axis = "brown")
mtext("Cabbage seedpod weevil degree days", side = 3, cex = 1.5, line = 3, col = "brown")

abline(h = SPLCanB[which.min(SPLCanB)], lty = 2)

abline(v = which.min(SPLCanB) + 49, lty = 2)
