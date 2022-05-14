library(tidyverse)
library(terra)
library(sf)
library(colorspace)
library(RColorBrewer)


con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "localhost",
  port = 5432,
  user = "admin",
  password = "cesarau",
  dbname = "postgres"
)
au <- sf::st_read(con, layer = "aus_states")
DBI::dbDisconnect(con)
plot(au)


fl <- list.files("C://Users/61423/Desktop/GFStest/", pattern = "^gfs_ugrd")
r <- terra::rast(paste0("C://Users/61423/Desktop/GFStest/", fl))
setMinMax(r)
terra::animate(r, pause = 0.01, n = 1)

# hcl_palettes()
# cl <- viridis::viridis(30, option = "E", direction = 1)
# cl <- sequential_hcl(30, "BrwnYl")
colpal <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
plot(r[[1]], col = colpal(30))
plot(st_geometry(au), add = TRUE)


# read U and V component for wind direction
u_ms <- rast("C://Users/61423/Desktop/GFStest/gfs_ugrd_20220226_t00z_f000")
v_ms <- rast("C://Users/61423/Desktop/GFStest/gfs_vgrd_20220226_t00z_f000")
plot(c(u_ms, v_ms), col = colpal(30))
# calculate wind speed
wind_abs <- sqrt(u_ms^2 + v_ms^2)
plot(wind_abs, col = colpal(30))
# calculate wind direction
wind_dir_trig_to <- atan2(u_ms/wind_abs, v_ms/wind_abs)
wind_dir_trig_to_degrees <- wind_dir_trig_to * 180/pi
wind_dir_trig_to_degrees <- wind_dir_trig_to_degrees + 180
wind_dir_trig_to_degrees <- 90 - wind_dir_trig_to_degrees
plot(wind_dir_trig_to_degrees, col = colpal(30))



## aggregate the data or reduce the arrow in plotting
# wind_dir_agg <- terra::aggregate(wind_dir_trig_to_degrees, fact=5)
# plot(wind_dir_agg)
# wind_speed_agg <- terra::aggregate(wind_abs, fact=5)
# plot(wind_speed_agg)
# max 134°45'5"E 8°27'20"S
# min 119°57'E 14°31'4"S
wind_dir_agg <- wind_dir_trig_to_degrees
wind_speed_agg <- wind_abs

wind_dt <- as.data.frame(wind_speed_agg, xy = TRUE)
names(wind_dt) <- c("Lon", "Lat", "mean_wind")
windir <- wind_dir_agg %>%
  as.data.frame() %>%
  setNames("wind_dir")
names(windir)
wind_dt <- cbind(wind_dt, windir)
head(wind_dt)

# plot the wind direction
ggplot(wind_dt,
       aes(x = Lon ,
           y = Lat,
           fill = mean_wind,
           angle = wind_dir[c(TRUE, rep(NA, 55))], # causes some values not to plot
           radius = scales::rescale(mean_wind, c(0.5, 2)))) +
  geom_raster() +
  geom_spoke(arrow = grid::arrow(length = unit(0.05, 'inches'))) +
  geom_sf(data = au, fill = NA, inherit.aes = FALSE) +
  scale_fill_distiller(palette = "RdYlGn") +
  # coord_equal(expand = 0) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal') +
  labs(fill = "Wind Speed", x = "Longitude", y = "Latitude")





# plot a few cells --------------------------------------------------------
exx <- c(120, 134.75, -14.5, -8.5)
wind_dir_agg <- wind_dir_trig_to_degrees %>%
  terra::crop(exx)
wind_speed_agg <- wind_abs %>%
  terra::crop(exx)

# create colour palette and plot
mycol <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")))
c(wind_dir_agg, wind_speed_agg) %>%
  raster::stack() %>%
  plot(col = colpal(30))

wind_dt <- as.data.frame(wind_speed_agg, xy = TRUE)
names(wind_dt) <- c("Lon", "Lat", "mean_wind")
windir <- wind_dir_agg %>%
  as.data.frame() %>%
  setNames("wind_dir")
names(windir)
wind_dt <- cbind(wind_dt, windir)
head(wind_dt)

# plot the wind direction
ggplot(wind_dt,
       aes(x = Lon ,
           y = Lat,
           fill = mean_wind,
           angle = wind_dir,#[c(TRUE, rep(NA, 5))], # causes some values not to plot
           radius = scales::rescale(mean_wind, c(0.05, 0.5)))) +
  geom_raster() +
  geom_spoke(arrow = grid::arrow(length = unit(0.05, 'inches'))) +
  # geom_sf(data = au, fill = NA, inherit.aes = FALSE) +
  scale_fill_distiller(palette = "RdYlGn") +
  # coord_equal(expand = 0) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal') +
  labs(fill = "Wind Speed", x = "Longitude", y = "Latitude")


# animation ---------------------------------------------------------------
filesv <- list.files("C:/Users/61423/Cesar_projects/WindDispersalFAW/Data",
                    pattern = "^gfs_v",
                    full.names = TRUE)
filesu <- list.files("C:/Users/61423/Cesar_projects/WindDispersalFAW/Data",
                    pattern = "^gfs_u",
                    full.names = TRUE)


rrv <- rast(filesv)
setMinMax(rrv)
rru <- rast(filesu)
setMinMax(rru)

rrs <- sqrt(rru^2 + rrv^2)
plot(rrs, col = colpal(30))
# calculate wind direction
rrd <- atan2(rru/rrs, rrv/rrs)
rrdd <- rrd * 180 / pi
rrdd <- 90 - (rrdd + 180)
plot(rrd, col = colpal(30))


terra::animate(rrs)
terra::animate(rrdd)



c(144.9481201, -37.7750568)
rr <- rrs[[1]] %>%
  terra::crop(c(135, 155, -48, -35))
plot(rr)
plot(au$geom, add = TRUE)
points(144.9481201, -37.7750568, col = "red", pch = 16)

cellFromXY(rr, data.frame(x = 144.9481201, y = -37.7750568))
rowColFromCell(rr, 37536)

x <- 156
y <- 181


vals <- c()
for (i in c(-1, 0, 1)){
  for(j in c(1, 0, -1)){
    vals <- c(vals, as.numeric(rr[x + i, y + j]))
    print(c(x + i, y + j))
  }
}
matrix(vals, nrow = 3)

# as.numeric(rrs[[1]][101, 49])





