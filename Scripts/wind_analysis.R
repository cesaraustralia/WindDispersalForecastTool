library(tidyverse)
library(rasterVis)
library(terra)
library(sf)


# read the wind functions
source("R/wind_scr.R")

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


wind_sim <- function(data_path = "Data/",
                     long = 145,
                     lat = -37.8,
                     nforecast = 24, # number of forward forecast hours
                     nsim = 10, # number of simulations to calculate frequency
                     atm_level = "850mb",
                     cellsize = 22000){

  require(tidyverse)
  require(terra)

  files <- gfs_names(path = data_path)

  # get a template raster
  r <- terra::rast(file.path(data_path, files$file[1]))
  # empty raster for simulations
  fct_raster <- r
  fct_raster[] <- 0
  names(fct_raster) <- "wind_forcast"

  xlen <- terra::ncol(r)
  ylen <- terra::nrow(r)

  points <- data.frame(x = colFromX(r, long),
                       y = rowFromY(r, lat))

  # weights for the output
  wt <- c(1, 1, 1, 1, 3, 1, 1, 1, 1)


  for(rep in seq_len(nsim)){

    n <- 1

    for(f in seq_len(nforecast)){

      x <- points[n, 1]
      y <- points[n, 2]

      forecasts <- unique(files$forecast)

      u <- read_u(path = data_path, files_list = files, fcast = forecasts[f], lev = atm_level)
      v <- read_v(path = data_path, files_list = files, fcast = forecasts[f], lev = atm_level)

      speed <- wind_speed(u = u, v = v)
      direction <- wind_direction(u = u, v = v)

      speed_ctr <- speed[y, x][1,1]

      steps <- max(1, ceiling(speed_ctr * 3600 / cellsize)) * 2
      # steps <- max(1, round(speed_ctr * 3600 / cellsize)) * 2

      for(e in seq_len(steps)){
        nbr_dir <- c()
        nbr_spd <- c()
        for(i in c(-1, 0, 1)){
          for(j in c(-1, 0, 1)){
            if(x + i < xlen && y + j < ylen){
              nbr_dir <- c(nbr_dir, direction[y+j, x+i][1,1])
              nbr_spd <- c(nbr_spd, speed[y+j, x+i][1,1])
            }
          }
        }
        # multiply the weight with the speeds
        probs <- wt * nbr_spd
        # add some randomness to the direction
        selected_dir <- sample(x = nbr_dir, size = 1, prob = probs)
        selected_dir <- selected_dir + runif(1, -30, 30)
        # keep the random direction within 0-360
        selected_dir <- selected_dir %% 360
        # calculate the next point
        newpoint <- next_cell(selected_dir, x, y)
        fct_raster[newpoint[2], newpoint[1]] <- fct_raster[newpoint[2], newpoint[1]][1,1] + 1

        n <- n + 1
        points[n, "x"] <- newpoint[1]
        points[n, "y"] <- newpoint[2]
      }

      if(nsim == 1 && n %% 5 == 0){
        cat("forecast:", n, "\n")
      }
    }
    cat("simulation:", rep, "\n")
  }
  # add point or smoothing spline lines? as an option?
  return(fct_raster)
}


tt <- wind_sim(data_path = "Data/", long = xx, lat = yy, nforecast = 24, nsim = 10)


tt[tt == 0] <- NA

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

