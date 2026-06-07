# Overlap between CSW and canola

load("CSWmodel.RData", "canola_model.RData")

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


par(mar = c(5, 7, 6, 2))

plot(seq(200, 700), pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                         scale = coef(mod_pods)[2]), xlab = "Canola growing degree days", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(200, 700), ylim = c(0, 1), yaxt = "n")

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

axis(side = 3, at = seq(50, 400, 50), cex.axis = 2)
polygon(seq(50, 400), dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                             scale = coef(mod_CSW1)[2])*50, col = CSWcol, border = NA)
mtext("Cabbage seedpod weevil degree days", side = 3, cex = 2, line = 3)


