library(tidyverse)
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

wind_direction <- function(u, v){
  at <- 180 + (atan2(u, v) * 180 / pi)
  ad <- terra::app(at, function(x) x %% 360)
  return(ad)
}

wind_speed <- function(u, v){
  r <- sqrt(v*v + u*u)
  return(r)
}

direction <- function(x, i, j){
  inc = 1
  # define the direction
  if (x < 22.5 || x > 337.5){
    ## N
    return(c(i, j + inc))
  } else if (x > 22.5 && x < 67.5){
    ## NE
    return(c(i + inc, j + inc))
  } else if (x > 67.5 && x < 112.5){
    ## E
    return(c(i + inc, j))
  } else if (x > 112.5 && x < 157.5){
    ## SE
    return(c(i + inc, j - inc))
  } else if (x > 157.5 && x < 202.5){
    ## S
    return(c(i, j - inc))
  } else if (x > 202.5 && x < 247.5){
    ## SW
    return(c(i - inc, j - inc))
  } else if (x > 247.5 && x < 292.5){
    ## W
    return(c(i - inc, j))
  } else if (x > 292.5 && x < 337.5){
    ## NW
    return(c(i - inc, j + inc))
  }
}


class_dir <- function(x){
  if (x <= 22.5 || x > 337.5){
    # w <- 'N'
    w <- 1
  } else if (x > 22.5 && x <= 67.5){
    # w <- 'NE'
    w <- 2
  } else if (x > 67.5 && x <= 112.5){
    # w <- 'E'
    w <- 3
  } else if (x > 112.5 && x <= 157.5){
    # w <- 'SE'
    w <- 4
  } else if (x > 157.5 && x <= 202.5){
    # w <- 'S'
    w <- 5
  } else if (x > 202.5 && x <= 247.5){
    # w <- 'SW'
    w <- 6
  } else if (x > 247.5 && x <= 292.5){
    # w <- 'W'
    w <- 7
  } else if (x > 292.5 && x <= 337.5){
    # w <- 'NW'
    w <- 8
  } else{
    w <- 0
  }
  return(w)
}

class_dir(100)
plot(dd)
dd

raster::calc(raster::raster(dd), fun = function(x) class_dir(x)) %>%
  raster::as.factor() %>%
  # rasterVis::levelplo1t() %>%
  plot() %>%
  identity()

plot(st_geometry(border), add = TRUE)




border <- st_read(dsn = .db_connect(), layer = 'aus_states')

plot(border)

u <- rast("C:/Users/61423/Cesar_projects/WindDispersalFAW/Data/gfs_ugrd_20220518_t18z_f000")
v <- rast("C:/Users/61423/Cesar_projects/WindDispersalFAW/Data/gfs_vgrd_20220518_t18z_f000")
plot(u)
plot(v)


speed <- sqrt(u*u + v*v)
plot(speed)
plot(st_geometry(border), add = TRUE)

# rd = np.mod(180 + np.rad2deg(np.arctan2(r[r.Component == "ugrd"], r[r.Component == "vgrd"])), 360)


dd <- wind_direction(u, v)
sp <- wind_speed(u, v)


xt <- c(135, 155, -45, -35)

par(mfrow=c(1,2))
plot(terra::crop(v, xt), main = "v component")
plot(st_geometry(border), add = TRUE)
plot(terra::crop(u, xt), main = "u component")
plot(st_geometry(border), add = TRUE)




wind_dt <- v %>%
  # terra::crop(xt) %>%
  as.data.frame(xy = TRUE)
names(wind_dt) <- c("Lon", "Lat", "mean_wind")
windir <- dd %>%
  # terra::crop(xt) %>%
  as.data.frame() %>%
  setNames("wind_dir") %>%
  mutate(wind_dir = wind_dir * pi / 180)
names(windir)
wind_dt <- cbind(wind_dt, windir)
head(wind_dt)

bord <- border %>%
  # st_crop(ext(crop(sp, xt))) %>%
  identity()

# plot the wind direction
ggplot(wind_dt,
       aes(x = Lon ,
           y = Lat,
           fill = mean_wind,
           angle = wind_dir[c(TRUE, rep(NA, 10))], # causes some values not to plot
           radius = scales::rescale(mean_wind, c(0.5, 2)))) +
  geom_raster() +
  geom_spoke(arrow = grid::arrow(length = unit(0.05, 'inches'))) +
  geom_sf(data = bord,
          fill = NA,
          inherit.aes = FALSE) +
  scale_fill_distiller(palette = "RdYlGn") +
  # coord_equal(expand = 0) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal') +
  labs(fill = "Wind Speed", x = "Longitude", y = "Latitude")












