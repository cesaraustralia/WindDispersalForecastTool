library(tidyverse)
library(rasterVis)
library(terra)
library(sf)

# connection to PostgreSQL database
.db_connect <- function(){
  DBI::dbConnect(RPostgres::Postgres(),
                 host = "localhost",
                 port = 5432,
                 user = Sys.getenv("POSTGRES_USER"),
                 password = Sys.getenv("POSTGRES_PASSWORD"),
                 dbname = Sys.getenv("POSTGRES_DB")
  )
}

# extract the component from names
gfs_names <- function(path = "Data/"){
  files <- list.files(path, pattern = "^gfs_")

  gfs_names <- strsplit(files, "_") %>%
    map(function(x){
      matrix(x, ncol = 5, byrow = FALSE) %>%
        as.data.frame() %>%
        setNames(c("gfs", "comp", "date", "start", "forecast")) %>%
        dplyr::select(-gfs)
    }) %>%
    do.call(rbind.data.frame, .) %>%
    mutate(file = files) %>%
    relocate(file)

  return(gfs_names)
}

wind_direction <- function(u, v){
  at <- 180 + (atan2(u, v) * 180 / pi)
  ad <- terra::app(at, function(x) x %% 360)
  return(ad)
}

wind_speed <- function(u, v){
  r <- sqrt(v*v + u*u)
  return(r)
}


# read the u component
read_u <- function(path = "Data/", files_list, fcast){
  files_list %>%
    dplyr::filter(comp == "ugrd") %>%
    dplyr::filter(forecast == fcast) %>%
    pull(file) %>%
    file.path(path, .) %>%
    terra::rast()
}
# read the u component
read_v <- function(path = "Data/", files_list, fcast){
  files_list %>%
    dplyr::filter(comp == "vgrd") %>%
    dplyr::filter(forecast == fcast) %>%
    pull(file) %>%
    file.path(path, .) %>%
    terra::rast()
}


next_cell <- function(x, i, j){
  inc <- 1
  # define the direction
  if (x <= 22.5 || x > 337.5){
    ## N
    return(c(i, j + inc))
  } else if (x > 22.5 && x <= 67.5){
    ## NE
    return(c(i - inc, j + inc))
  } else if (x > 67.5 && x <= 112.5){
    ## E
    return(c(i - inc, j))
  } else if (x > 112.5 && x <= 157.5){
    ## SE
    return(c(i - inc, j - inc))
  } else if (x > 157.5 && x <= 202.5){
    ## S
    return(c(i, j - inc))
  } else if (x > 202.5 && x <= 247.5){
    ## SW
    return(c(i + inc, j - inc))
  } else if (x > 247.5 && x <= 292.5){
    ## W
    return(c(i + inc, j))
  } else if (x > 292.5 && x <= 337.5){
    ## NW
    return(c(i + inc, j + inc))
  }
}



border <- st_read(dsn = .db_connect(), layer = 'aus_states')
r <- terra::rast("Data/gfs_ugrd_20220522_t18z_f000")
# plot(r)

xlen <- terra::ncol(r)
ylen <- terra::nrow(r)

# 24 hours simulation
nforecast <- 24
# cell size - can be calcualte with np.cos(np.radians(35)) * 111325 * 0.25
cellsize <- 22000
# starting point
xx <- 130.781250
yy <- -12.511665
points <- data.frame(x = colFromX(r, xx),
                     y = rowFromY(r, yy))

# weights for the output
wt <- c(1, 1, 1, 1, 3, 1, 1, 1, 1)

# number of reapeats for simulation
nrep <- 10

files <- gfs_names(path = "Data/")

fct_raster <- r
fct_raster[] <- 0

for(rep in seq_len(nrep)){

  n <- 1

  for(f in seq_len(nforecast)){

    x <- points[n, 1]
    y <- points[n, 2]

    forecasts <- unique(files$forecast)

    u <- read_u(path = "Data/", files_list = files, fcast = forecasts[f])
    v <- read_v(path = "Data/", files_list = files, fcast = forecasts[f])

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

    if(nrep == 1 && n %% 5 == 0){
      cat("forecast:", n, "\n")
    }
  }
  cat("simulation:", rep, "\n")
}


tt <- fct_raster
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
r_crop <- r_crop / global(r_crop, max, na.rm = TRUE)[1,1]

gplot(r_crop, maxpixels = 500000) +
  geom_tile(aes(fill = value), alpha = 0.8) +
  viridis::scale_fill_viridis(option = "A", na.value = NA) +
  # geom_point(data = pt, aes(x = long, y = lat, col = id), inherit.aes = FALSE) +
  geom_sf(data = st_crop(border, ext(xt)), inherit.aes = FALSE, fill = NA) +
  coord_sf(crs = 4326) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", fill = "Frequency")

