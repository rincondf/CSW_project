
par(mfrow = c(1, 2))


### Canola model

par(mar = c(5, 7, 3, 2))
plot(pods$DDs1, pods$props, xlim = c(300, 800), cex.axis = 2, yaxt = "n", 
     lwd = 2, xlab = "Canola growing degree days", cex.lab = 2, ylab = "", ylim = c(0, 0.12))
lines(seq(300, 800), dgamma(seq(300, 800), shape = coef(mod_pods)[1], scale = coef(mod_pods)[2])*15, lwd = 2)

mtext("A", side = 3, cex = 2, at = 250, line = 1)

mtext("Proportion of seedpods formed", side = 2, cex = 2, line = 5)
axis(side = 2, at = seq(0, 0.12, 0.02), cex.axis = 2, las = 2)

### CSW model

par(mar = c(5, 7, 3, 2))
plot(all.data$DDs, all.data$props, xlim = c(10, 350), cex.axis = 2, yaxt = "n", 
     lwd = 2, xlab = "Cabbage seedpod weevil degree days", cex.lab = 2, ylab = "", ylim = c(0, 0.55))
lines(seq(15, 350), dgamma(seq(15, 350), shape = coef(mod_CSW1)[1], scale = coef(mod_CSW1)[2])*35, lwd = 2)

mtext("B", side = 3, cex = 2, at = -20, line = 1)

mtext("Proportion of adults emerged", side = 2, cex = 2, line = 5)
axis(side = 2, at = seq(0, 0.55, 0.1), cex.axis = 2, las = 2)
