library(tidyverse)
library(terra)
library(sf)
library(rasterVis)

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
border <- st_read(dsn = .db_connect(), layer = 'aus_states')



r <- rast("C:/Users/61423/Desktop/wind_ca.tif")
r <- rast("C:/Users/61423/Cesar_projects/WindDispersalFAW/Data/gfs_ugrd_20220522_t18z_f000")


pt <- read.csv("C:/Users/61423/Desktop/points.csv") %>%
  # select(x, y) %>%
  setNames(c("id", "x", "y")) %>%
  data.frame(x = xFromCol(r, .[,"x"]), y = yFromRow(r, .[,"y"])) %>%
  st_as_sf(coords = c("x.1", "y.1"), crs = 4326) %>%
  # st_geometry() %>%
  mutate(long = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  identity()


pt
smline <- smooth.spline(x = st_coordinates(pt),
                        df = 4)

# smline <- spline(x = st_coordinates(pt)[,1],
#                  y = st_coordinates(pt)[,2])

plot(r)
# plot(terra::crop(r, c(130, 150, -45, -30)))
# plot(terra::crop(r, c(135, 155, -45, -35)))
# plot(terra::crop(r, c(130, 150, -20, -5)))
border %>%
  st_geometry() %>%
  plot(add = TRUE, border = "gray30")
plot(pt, col = "red", add = TRUE)
# lines(pt$long, pt$lat, lwd = 2, col = "red")
# lines(smline$x, smline$y, lwd = 2, col = "blue")


xt <- c(135, 155, -45, -30)
r_crop <- terra::crop(r, xt)

gplot(r_crop, maxpixels = 500000) +
  geom_tile(aes(fill = value)) +
  viridis::scale_fill_viridis() +
  # geom_point(data = pt, aes(x = long, y = lat, col = id), inherit.aes = FALSE) +
  geom_sf(data = st_crop(border, ext(xt)), inherit.aes = FALSE, fill = NA) +
  geom_sf(data = pt, aes(col = id), inherit.aes = FALSE) +
  coord_sf(crs = 4326) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", fill = "Wind")



# r <- terra::crop(r, c(135, 155, -45, -35))
colFromX(r, 137.3510742)
rowFromY(r, -32.3799615)

