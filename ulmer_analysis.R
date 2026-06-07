# Retrieve temp data for the locations reported in Ulmer & Dosdall (2006)

library(daymetr)

temp_recs <- list()

temp_recs <- download_daymet(
  site = "Daymet",
  lat = 49.45,
  lon = -112.65,
  start = 2003,
  end = 2004,
  path = tempdir(),
  internal = TRUE,
  silent = FALSE,
  force = FALSE,
  simplify = FALSE
)


tmax2003 <- as.numeric(temp_recs$data[which(temp_recs$data$year == 2003), 7])
tmin2003 <- as.numeric(temp_recs$data[which(temp_recs$data$year == 2003), 8])

tmax2004 <- as.numeric(temp_recs$data[which(temp_recs$data$year == 2004), 7])
tmin2004 <- as.numeric(temp_recs$data[which(temp_recs$data$year == 2004), 8])

# Calculate cumulative degree days using max and min temps suggested by Ni (1990)

upT <- 10 + 19.8
baseT <- 10


DDs_CSW2003 <- calc_dd_vec(tmax = tmax2003, tmin = tmin2003, 
                           lower_threshold = baseT, 
                           upper_threshold = upT, 
                           cutoff = "horizontal")
DDs_CSW2003 <- cumsum(DDs_CSW2003)


DDs_CSW2004 <- calc_dd_vec(tmax = tmax2004, tmin = tmin2004, 
                           lower_threshold = baseT, 
                           upper_threshold = upT, 
                           cutoff = "horizontal")
DDs_CSW2004 <- cumsum(DDs_CSW2004)



# Digitizing data reported in Ulmer & Dosdall (2006)

library(digitize)


means2003 <- digitize("means_2003.jpg")
means2004 <- digitize("means_2004.jpg")



colnames(means2003) <- c("date", "weevils") 
colnames(means2004) <- c("date", "weevils") 


dates.2003 <- c("4/15/2003", "4/23/2003", "5/1/2003", "5/15/2003", "5/21/2003", 
                "5/28/2003", "6/5/2003", "6/13/2003", "6/18/2003", "6/26/2003", 
                "7/3/2003")

means2003$date <- as.Date(dates.2003, "%m/%d/%Y")
means2003$julian <- as.numeric(format(means2003$date, "%j"))

means2003$DDs <- DDs_CSW2003[means2003$julian]

means2003$props <- means2003$weevils / sum(means2003$weevils)


dates.2004 <- c("4/29/2004", "5/6/2004", "5/14/2004", "5/18/2004", "5/28/2004", 
                "6/4/2004", "6/10/2004", "6/18/2004", "6/25/2004", "7/1/2004", 
                "7/9/2004")

means2004$date <- as.Date(dates.2004, "%m/%d/%Y")
means2004$julian <- as.numeric(format(means2004$date, "%j"))

means2004$DDs <- DDs_CSW2004[means2004$julian]

means2004$props <- means2004$weevils / sum(means2004$weevils)

#######

plot(means2004$DDs, means2004$props, xlim = c(10, 400))
points(means2003$DDs, means2003$props, col = "red")

all.data <- rbind(means2003, means2004)

# Parameter estimation

library(bbmle)

CSW.DDs <- rep(all.data$DDs, round(all.data$weevils * 1000))

MLL_CSW <- function(shape, scale) {
  -sum(dweibull(x, shape = shape, scale = scale, log = TRUE))
}

mod_CSW <- mle2(MLL_CSW, start = list(shape = 3, scale  = 328), 
                 data = list(x = CSW.DDs))
summary(mod_CSW)

hist(CSW.DDs)

plot(all.data$DDs, all.data$props)
lines(seq(50, 350), dweibull(seq(50, 350), shape = coef(mod_CSW)[1], scale = coef(mod_CSW)[2])*40)


MLL_CSW1 <- function(shape, scale) {
  -sum(dgamma(x, shape = shape, scale = scale, log = TRUE))
}

mod_CSW1 <- mle2(MLL_CSW1, start = list(shape = 3, scale  = 328), 
                 data = list(x = CSW.DDs))

summary(mod_CSW1)


plot(all.data$DDs, all.data$props)
lines(seq(50, 350), dgamma(seq(50, 350), shape = coef(mod_CSW1)[1], scale = coef(mod_CSW1)[2])*40)


#######




