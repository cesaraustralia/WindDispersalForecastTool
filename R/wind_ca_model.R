# Roozbeh Valavi
# 2022-05-24
#
# simulate wind dispersal with cellular automata
wind_sim <- function(data_path = "wind-data/",
                     long = 145,
                     lat = -37.8,
                     nforecast = 24, # number of forward forecast hours
                     nsim = 10, # number of simulations to calculate frequency
                     atm_level = "850mb",
                     cellsize = 25000){

  require(tidyverse)
  require(terra)

  files <- gfs_names(path = data_path)

  # get a template raster
  r <- terra::rast(file.path(data_path, files$file[1]))
  # empty raster for simulations
  fct_raster <- r
  fct_raster[] <- 0
  names(fct_raster) <- "wind_forecast"

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

      ## calculate the number of steps based on wind speed and cell size
      # if we choose at least 1 step each time there could be too many steps overall
      # when the speed is low that results in overshooting, i.e. trajectoies longer than reality
      # this could be happening because of course raster resolution
      # so I made it random, to have some movement with low wind speed, but not always
      # better solution is possible; saving distance when lower than one cell?
      steps <- max(sample(0:1, 1), ceiling(speed_ctr * 3600 / cellsize))
      # steps <- max(1, ceiling(speed_ctr * 3600 / cellsize))

      if(steps < 1) next

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

  fct_raster[fct_raster == 0] <- NA
  # add point or smoothing spline lines? as an option?
  return(fct_raster)
}

