# Calculate cumulative degree days using constant temp reported by Tayo & Morgan (1975)

upT <- 50 # there is no upper threshold temp for crop development... for some reason.
baseT <- 5 # based on Morrison et al. (1989)


DDs_canola <- calc_dd_vec(tmax = 20, tmin = 20, 
                           lower_threshold = baseT, 
                           upper_threshold = upT, 
                           cutoff = "horizontal")
DDs_canola <- cumsum(DDs_canola)

# Digitizing data reported in Tayo & Morgan (1975)

library(digitize)

pods <- digitize("canola_pods.jpg")