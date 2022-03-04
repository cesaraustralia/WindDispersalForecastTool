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


fl <- list.files("C://Users/61423/Desktop/GFStest/", pattern = "^gfs_vg")
r <- terra::rast(paste0("C://Users/61423/Desktop/GFStest/", fl))
r
# terra::writeCDF(r, "C://Users/61423/Desktop/gfsdata.nc")

setMinMax(r)
terra::animate(r, pause = 0.01, n = 1)

# hcl_palettes()
# cl <- viridis::viridis(30, option = "E", direction = 1)
# cl <- sequential_hcl(30, "BrwnYl")
cl <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))



u_ms <- rast("C://Users/61423/Desktop/GFStest/gfs_ugrd_20220302_t00z_f001")
v_ms <- rast("C://Users/61423/Desktop/GFStest/gfs_vgrd_20220302_t00z_f001")
# calculate wind direction
wind_abs <- sqrt(u_ms^2 + v_ms^2)
wind_dir_trig_to <- atan2(u_ms/wind_abs, v_ms/wind_abs) 
wind_dir_trig_to_degrees <- wind_dir_trig_to * 180/pi

plot(wind_dir_trig_to_degrees)
# plot(raster::raster(wind_dir_trig_to_degrees))
ext <- raster::drawExtent()
# rcp <- crop(r, ext)
plot(rcp[[2]])
wind_dt <- as.data.frame(r[[2]], xy = TRUE)
names(wind_dt) <- c("Lon", "Lat", "mean_wind")
windir <- wind_dir_trig_to_degrees %>% 
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
           radius = scales::rescale(mean_wind, c(0.2, 2.0)))) +
  geom_raster() +
  geom_spoke(arrow = grid::arrow(length = unit(0.05, 'inches'))) + 
  geom_sf(data = au, fill = NA, inherit.aes = FALSE) +
  scale_fill_distiller(palette = "RdYlGn") + 
  # coord_equal(expand = 0) + 
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal') +
  labs(fill = "Wind Speed", x = "Longitute", y = "Latitude")




df <- as.data.frame(r, xy = TRUE)
names(df) <- c("x", "y", paste0("r", 1:nlyr(r)))
head(df)

ls <- c(1, 3, 6, 9)
df %>% 
  pivot_longer(cols = 3:ncol(.)) %>% 
  filter(name %in% paste0("r", ls)) %>% 
ggplot(data = ., aes(x = x, y = y, fill = value)) +
  geom_raster() +
  facet_wrap(~name) +
  scale_fill_gradientn(colours = cl(50)) +
  geom_sf(data = au, inherit.aes = FALSE, fill = NA) +
  # coord_equal() +
  theme_bw()




# par(mfrow=c(2,2))
# ls <- c(3, 6, 9, 12)
# dt <- c("2022-03-02 03:00 AEDT", "2022-03-02 06:00 AEDT", 
#         "2022-03-02 09:00 AEDT", "2022-03-02 12:00 AEDT")
# n <- 0
# for(i in ls){
#   n <- n + 1
#   plot(r[[i]], col = cl(50))
#   text(x = 120, y = -5, dt[n])
#   plot(sf::st_geometry(au), add = TRUE)
# }
# par(mfrow=c(1,1))
