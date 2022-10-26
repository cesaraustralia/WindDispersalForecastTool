# Roozbeh Valavi
# 2022-05-24
#
# Additions by Alex Slavenko
# 2022-10-26
#
# simulate wind dispersal with cellular automata
wind_sim <- function(data_path = "wind-data/",
                     coords = list(), # list of starting coordinates, each a vector of c(longitude, latitude)
                     # long = 145,
                     # lat = -37.8,
                     nforecast = 24, # number of forward forecast hours
                     nsim = 10, # number of simulations to calculate frequency
                     fdate = "20220524", # the forecast data
                     fhour = "18", # the forecast hour
                     atm_level = "850mb",
                     cellsize = 25000,
                     full = F, # if TRUE, generate a dataframe of endpoints per time step
                     parallel = F, # if TRUE run in parallel
                     ncores = F){ # if FALSE (default) use max number of cores - 1. Else set to number of cores to use

  require(tidyverse)
  require(terra)

  files <- gfs_names(path = data_path)
  seeds <- sample(seq(1, length(coords)*10), length(coords))

  if(parallel){
    require(doParallel)
    require(foreach)

    cores = detectCores(logical = T)
    if(!is.numeric(ncores))
      ncores <- cores[1]-1 else
        if(ncores > cores[1])
          stop("Number of cores must be equal to or less than available cores in system")

    ncores = min(ncores, length(coords))
    print(paste("Running in parallel using", ncores, "cores"))

    cl <- makeCluster(ncores)
    registerDoParallel(cl)

    npoint <- foreach(point = 1:length(coords), .packages = c("tidyverse", "terra"), .export = c("read_u", "read_v", "wind_speed", "wind_direction", "next_cell")) %dopar% {
      points <- data.frame(x = colFromX(r, long),
                           y = rowFromY(r, lat))

      # weights for the output
      wt <- c(1, 1, 1, 1, 3, 1, 1, 1, 1)

      r <- terra::rast(file.path(data_path, files$file[1]))

      set.seed(seeds[point])

      # extract coordinates
      long = coords[[point]][1]
      lat = coords[[point]][2]
      n <- 1

      # empty raster for simulations
      fct_raster <- r
      fct_raster[] <- 0
      names(fct_raster) <- "wind_forecast"

      xlen <- terra::ncol(r)
      ylen <- terra::nrow(r)

      # weights for the output
      wt <- c(1, 1, 1, 1, 3, 1, 1, 1, 1)

      points_full <- tibble(x = numeric(), y = numeric(), nsim = numeric(), nforecast = numeric())

      for(rep in seq_len(nsim)){

        points <- data.frame(x = colFromX(r, long),
                             y = rowFromY(r, lat),
                             nforecast = 0)

        n <- 1

        for(f in seq_len(nforecast)){

          x <- points[n, 1]
          y <- points[n, 2]

          forecasts <- unique(files$forecast)

          u <- read_u(path = data_path, files_list = files, fcast = forecasts[f], lev = atm_level)
          v <- read_v(path = data_path, files_list = files, fcast = forecasts[f], lev = atm_level)

          # calculate wind speed and direction
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
            points[n, "nforecast"] <- f
          }
        }
      }

      if(full){
        points_full <- bind_rows(points_full,
                                 as_tibble(xyFromCell(r,
                                                      cellFromRowCol(r,
                                                                     points[,"y"],
                                                                     points[,"x"]))) %>%
                                   mutate(nsim = rep,
                                          nforecast = points$nforecast,
                                          x_start = long,
                                          y_start = lat))
        list(raster::raster(fct_raster), points_full)

      } else
        raster::raster(fct_raster)
    }
    stopCluster(cl)
  } else {

    r <- terra::rast(file.path(data_path, files$file[1]))

    npoint <- list()

    print("Simulating:")
    progress_bar = txtProgressBar(min=0, max=length(coords), style = 3, char="=")

    for(point in 1:length(coords)){
      set.seed(seeds[point])

      # extract coordinates
      long = coords[[point]][1]
      lat = coords[[point]][2]

      # empty raster for simulations
      fct_raster <- r
      fct_raster[] <- 0
      names(fct_raster) <- "wind_forecast"

      xlen <- terra::ncol(r)
      ylen <- terra::nrow(r)

      # weights for the output
      wt <- c(1, 1, 1, 1, 3, 1, 1, 1, 1)

      points_full <- tibble(x = numeric(), y = numeric(), nsim = numeric(), nforecast = numeric())

      for(rep in seq_len(nsim)){

        points <- data.frame(x = colFromX(r, long),
                             y = rowFromY(r, lat),
                             nforecast = 0)

        n <- 1

        for(f in seq_len(nforecast)){

          x <- points[n, 1]
          y <- points[n, 2]

          forecasts <- unique(files$forecast)

          u <- read_u(path = data_path, files_list = files, fcast = forecasts[f], lev = atm_level)
          v <- read_v(path = data_path, files_list = files, fcast = forecasts[f], lev = atm_level)

          # calculate wind speed and direction
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
            points[n, "nforecast"] <- f
          }
        }

        if(full){
          points_full <- bind_rows(points_full,
                                   as_tibble(xyFromCell(r,
                                                        cellFromRowCol(r,
                                                                       points[,"y"],
                                                                       points[,"x"]))) %>%
                                     mutate(nsim = rep,
                                            nforecast = points$nforecast,
                                            x_start = long,
                                            y_start = lat))
          npoint[[point]] <- list(raster::raster(fct_raster), points_full)

        } else
          npoint[[point]] <- raster::raster(fct_raster)

        setTxtProgressBar(progress_bar, value = point)
      }
    }
  }

  if(full) {
    fct_raster <- stack(lapply(npoint, "[[", 1))
    fct_raster <- calc(fct_raster, sum)
    fct_raster[fct_raster == 0] <- NA
    points_full <- lapply(npoint, "[[", 2)
    return(list(rast(fct_raster), bind_rows(points_full)))
  } else {
    fct_raster <- stack(npoint)
    fct_raster <- calc(fct_raster, sum)
    fct_raster[fct_raster == 0] <- NA
    return(rast(fct_raster))
  }
  cat("simulation:", rep, "\n")
}

fct_raster[fct_raster == 0] <- NA
# add point or smoothing spline lines? as an option?
return(fct_raster)
}

