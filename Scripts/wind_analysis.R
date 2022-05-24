library(tidyverse)
library(rasterVis)
library(terra)
library(sf)


# read the wind functions
source("R/wind_scr.R")
source("R/wind_ca_model.R")

border <- st_read("SpatialData/australia_states.gpkg", quiet = TRUE)



# # 24 hours simulation
# nforecast <- 24
# # cell size - can be calcualte with np.cos(np.radians(35)) * 111325 * 0.25
# cellsize <- 22000
# # starting point
xx <- 149.062500
yy <- -35.263562
# points <- data.frame(x = colFromX(r, xx),
#                      y = rowFromY(r, yy))
# # weights for the output
# wt <- c(1, 1, 1, 1, 3, 1, 1, 1, 1)
# # number of reapeats for simulation
# nsim <- 10
# files <- gfs_names(path = "Data/")
# fct_raster <- r
# fct_raster[] <- 0



tt <- wind_sim(data_path = "Data/", long = xx, lat = yy, nforecast = 24, nsim = 10)


# specify the crop area
inc <- 10
xt <- c(floor(xx) - inc, floor(xx) + inc, floor(yy) - inc, floor(yy) + inc)
# plot(terra::crop(tt, xt))
# plot(st_geometry(border), add = TRUE, border = "gray30")

# # convert the row/col to long/lat
# pointsall <- points %>%
#   mutate(long = xFromCol(r, x), lat = yFromRow(r, y)) %>%
#   dplyr::select(long, lat)

r_crop <- terra::crop(tt, xt)
r_crop <- r_crop / global(r_crop, max, na.rm = TRUE)[1,1] * 100

gplot(r_crop, maxpixels = 500000) +
  geom_tile(aes(fill = value), alpha = 0.8) +
  viridis::scale_fill_viridis(option = "A", na.value = NA) +
  # geom_point(data = pt, aes(x = long, y = lat, col = id), inherit.aes = FALSE) +
  geom_sf(data = st_crop(border, ext(xt)), inherit.aes = FALSE, fill = NA) +
  coord_sf(crs = 4326) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", fill = "Frequency")

