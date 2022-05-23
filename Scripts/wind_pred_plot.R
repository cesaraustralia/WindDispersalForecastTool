library(tidyverse)
library(terra)
library(sf)

r <- rast("C:/Users/61423/Cesar_projects/WindDispersalFAW/Data/gfs_ugrd_20220522_t18z_f000")
plot(r)

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


pt <- read.csv("C:/Users/61423/Desktop/points.csv") %>%
  select(x, y) %>%
  data.frame(x = xFromCol(r, .[,1]), y = yFromRow(r, .[,2])) %>%
  st_as_sf(coords = c("x.1", "y.1")) %>%
  st_geometry()

smline <- smooth.spline(x = st_coordinates(pt)[,1],
                 y = st_coordinates(pt)[,2],
                 df = 4)

# plot(r)
# plot(terra::crop(r, c(135, 155, -45, -35)))
# st_read(dsn = .db_connect(), layer = 'aus_states') %>%
#   st_geometry() %>%
#   plot(add = TRUE, border = "gray30")
# plot(pt, col = "red", add = TRUE)
lines(smline$x, smline$y, lwd = 2, col = "blue")


# r <- terra::crop(r, c(135, 155, -45, -35))
rowFromY(r, -37.4181633)
colFromX(r, 144.9481201)

