library(tidyverse)
library(lubridate)
library(terra)
library(sf)
library(raster)
library(spatialEco)
library(viridis)
library(cowplot)
library(mgcv)
library(mgcViz)
library(rgeoda)
# library(doParallel)
# library(foreach)

# read the wind functions
source("R/wind_scr.R")
source("R/wind_ca_model.R")


monsoon <- dir("wind-data/Monsoon-2021-22/")

k = 0
u950 <- list()
u850 <- list()
v950 <- list()
v850 <- list()
for(i in monsoon){
  k = k+1
  r <- rast(paste0("wind-data/Monsoon-2021-22/", i))

  u950[[k]] <- r[[1]]
  u850[[k]] <- r[[2]]
  v950[[k]] <- r[[3]]
  v850[[k]] <- r[[4]]
}

names(u950) <- sapply(monsoon, function(x) str_split(x, pattern = "[.]")[[1]][[3]])
names(u850) <- sapply(monsoon, function(x) str_split(x, pattern = "[.]")[[1]][[3]])
names(v950) <- sapply(monsoon, function(x) str_split(x, pattern = "[.]")[[1]][[3]])
names(v850) <- sapply(monsoon, function(x) str_split(x, pattern = "[.]")[[1]][[3]])

u950 <- rast(u950)
u850 <- rast(u850)
v950 <- rast(v950)
v850 <- rast(v850)

n_points <- readRDS("bgs_north.rds")

n_points <- lapply(seq(1, dim(n_points)[1]), function(x) n_points[x,])

k = 0
test_period <- c("20220101", "20220102", "20220103", "20220104", "20220105", "20220106", "20220107")
test_sim <- list()

start.time <- Sys.time()
for(i in test_period){
  k = k + 1
  test_sim[[k]] <- wind_sim_hist(data_u = u950,
                                 data_v = v950,
                                 coords = n_points,
                                 nforecast = 48,
                                 nsim = 1,
                                 fdate = i,
                                 fhour = "00",
                                 full = T,
                                 parallel = T)
}
end.time <- Sys.time()


sum_sim <- calc(stack(lapply(test_sim, function(x) raster(x[[1]]))), sum)
sum_sim <- rast(sum_sim)

plot(sum_sim / global(sum_sim, max, na.rm = TRUE)[1,1] * 100,
     main = paste("Wind-assisted dispersal for 48h every day from 2022-01-01 00:00 UTC to 2022-01-07 00:00 UTC; n = ", length(n_points), "; Computation time: ", round(time_length(end.time - start.time, "minute"), 2), " minutes", sep = ""))
plot(st_geometry(border), add = T)
plot(st_as_sf(bind_rows(n_points), coords = c("x", "y"), crs = crs(sum_sim)), add = T, col = "red")
