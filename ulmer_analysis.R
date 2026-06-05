# Retreive temp data for the locations reported in Ulmer & Dosdall (2006)

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











# Matching data collection days

data_clean$NOW_DD <- rep(NA, length(data_clean$loc))


for(i in 1: length(locations_NOW$loc)) {
  data_clean$NOW_DD[which(data_clean$latitude == locations_NOW$latitude[i] & 
                            data_clean$longitude == locations_NOW$longitude[i] &
                            data_clean$year == locations_NOW$year[i])] <-
    DDs_NOW[[i]][data_clean$julian[which(data_clean$latitude == 
                                           locations_NOW$latitude[i] &
                                           data_clean$longitude == 
                                           locations_NOW$longitude[i] &
                                           data_clean$year == 
                                           locations_NOW$year[i])]]
}
