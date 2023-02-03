library(tidyverse)
library(lubridate)
library(terra)
library(sf)
library(raster)

# read the wind functions
source("R/wind_scr.R")
source("R/wind_ca_model.R")
source("Scripts/wind_download.R")

n_points <- readRDS("bgs_north.rds")

n_points <- lapply(seq(1, dim(n_points)[1]), function(x) n_points[x,])

start.time <- Sys.time()
n_movement <- wind_sim(data_path = c("wind-data/20221109/00"), coords = n_points,
                       nsim = 1, nforecast = 8, atm_level = "950mb", full = T, parallel = T)
end.time <- Sys.time()

png("Figures/CA_bgs_north.png", width = 600)
plot(n_movement[[1]] / global(n_movement[[1]], max, na.rm = TRUE)[1,1] * 100,
     main = paste("Wind-assisted dispersal for 48h starting on 2022-01-01 00:00 UTC; n = ", length(n_points), "; Computation time: ", round(time_length(end.time - start.time, "minute"), 2), " minutes", sep = ""))
plot(st_geometry(border), add = T)
plot(st_as_sf(bind_rows(n_points), coords = c("x", "y"), crs = crs(n_movement)), add = T, col = "red")
dev.off()
