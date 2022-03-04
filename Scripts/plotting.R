library(tidyverse)
library(terra)
library(sf)
library(colorspace)
library(RColorBrewer)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = 5432,
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  dbname = Sys.getenv("POSTGRES_DB")
)
au <- sf::st_read(con, layer = "aus_border")
DBI::dbDisconnect(con)


fl <- list.files("C://Users/61423/Desktop/GFStest/", pattern = "^gfs_ugrd")
r <- terra::rast(paste0("C://Users/61423/Desktop/GFStest/", fl))
setMinMax(r)
terra::animate(r, pause = 0.01, n = 1)

# hcl_palettes()
# cl <- viridis::viridis(30, option = "E", direction = 1)
# cl <- sequential_hcl(30, "BrwnYl")
color_palette <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
plot(r[[1]], col = color_palette(30))
plot(st_geometry(au), add = TRUE)


# read U and V component for wind direction
u_ms <- rast("C://Users/61423/Desktop/GFStest/gfs_ugrd_20220228_t18z_f000")
v_ms <- rast("C://Users/61423/Desktop/GFStest/gfs_vgrd_20220228_t18z_f000")
# calculate wind speed
wind_abs <- sqrt(u_ms^2 + v_ms^2)
plot(wind_abs)
# calculate wind direction
wind_dir_trig_to <- atan2(u_ms/wind_abs, v_ms/wind_abs) 
wind_dir_trig_to_degrees <- wind_dir_trig_to * 180/pi

plot(wind_dir_trig_to_degrees)

## aggregate the data or reduce the arrow in plotting
# wind_dir_agg <- terra::aggregate(wind_dir_trig_to_degrees, fact=5)
# plot(wind_dir_agg)
# wind_speed_agg <- terra::aggregate(wind_abs, fact=5)
# plot(wind_speed_agg)
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

# plot the wind directoin
ggplot(wind_dt, 
       aes(x = Lon , 
           y = Lat, 
           fill = mean_wind, 
           angle = wind_dir[c(TRUE, rep(NA, 100))], # causes some values not to plot
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

