library(tidyverse)
library(terra)
library(sf)

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


fl <- list.files("C://Users/61423/Desktop/GFStest/", pattern = "^gfs")
r <- terra::rast(paste0("C://Users/61423/Desktop/GFStest/", fl))
r

setMinMax(r)
terra::animate(r, pause = 0.01, n = 1)


plot(r[[4]], col = viridis::viridis(30, option = "E", direction = 1))
plot(sf::st_geometry(au), add = TRUE)
