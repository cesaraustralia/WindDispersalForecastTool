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

fl <- list.files("C://Users/61423/Desktop/", pattern = "^gfs.t12z.pgrb2")

r <- terra::rast(paste0("C://Users/61423/Desktop/", fl))
r

plot(r[[1]])
plot(sf::st_geometry(au), add = TRUE)

setMinMax(r)
terra::animate(r)
