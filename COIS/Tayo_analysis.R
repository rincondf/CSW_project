# Calculate cumulative degree days using constant temp reported by Tayo & Morgan (1975)

upT <- 50 # there is no upper threshold temp for crop development... for some reason.
baseT <- 5 # based on Morrison et al. (1989)


DDs_canola <- calc_dd_vec(tmax = rep(20, 28), tmin = rep(20, 28), 
                           lower_threshold = baseT, 
                           upper_threshold = upT, 
                           cutoff = "horizontal")
DDs_canola <- cumsum(DDs_canola)

# Digitizing data reported in Tayo & Morgan (1975)

library(digitize)

pods <- digitize("canola_pods.jpg")

colnames(pods) <- c("DDs", "pods") 

pods$DDs <- DDs_canola
pods$props <- pods$pods / sum(pods$pods)
pods <- rbind(data.frame(DDs = 0, pods = 0, props = 0), pods)

pods$DDs1 <- pods$DDs + 350

plot(pods$DDs1, pods$props)

save(pods, file = "pods.RData")

# Parameter estimation

library(bbmle)

load("pods.RData")

pods.DDs <- rep(pods$DDs1, round(pods$props * 1000))

MLL_pods1 <- function(shape, scale) {
  -sum(dweibull(x, shape = shape, scale = scale, log = TRUE))
}

mod_pods1 <- mle2(MLL_pods1, start = list(shape = 3, scale  = 328), 
                  data = list(x = pods.DDs))

summary(mod_pods1)


plot(pods$DDs1, pods$props)
lines(seq(1, 700), dweibull(seq(1, 700), shape = coef(mod_pods1)[1], scale = coef(mod_pods1)[2])*19)

################

MLL_pods <- function(shape, scale) {
  -sum(dgamma(x, shape = shape, scale = scale, log = TRUE))
}

mod_pods <- mle2(MLL_pods, start = list(shape = 3, scale  = 328), 
                 data = list(x = pods.DDs))

summary(mod_pods)


plot(pods$DDs1, pods$props)
lines(seq(1, 700), dgamma(seq(1, 700), shape = coef(mod_pods)[1], scale = coef(mod_pods)[2])*15)

save(mod_pods, file = "canola_model.RData")

#######
