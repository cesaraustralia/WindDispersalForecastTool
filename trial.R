library(tidyverse)
library(lubridate)
library(terra)
library(sf)
library(raster)

# read the wind functions
source("R/wind_scr.R")
source("R/wind_ca_model.R")
source("Scripts/wind_download.R")


n_points <- st_join(st_as_sf(stars::st_as_stars(empty_r)), st_transform(border, crs(empty_r))) %>%
  drop_na(FID_world_) %>%
  st_centroid() %>%
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2]) %>%
  as_tibble() %>%
  dplyr::select(c(x, y)) %>%
  group_by(round(x, 2)) %>%
  filter(y == max(y),
         x < 156) %>%
  ungroup() %>%
  dplyr::select(x, y)

n_points <- lapply(seq(1, dim(n_points)[1]), function(x) as.matrix(n_points)[x,])

start.time <- Sys.time()
n_movement <- wind_sim(data_path = c("wind-data/20221011/00"), coords = n_points,
                       nsim = 8, nforecast = 48, atm_level = "850mb", full = T, parallel = T)
end.time <- Sys.time()

pdf("Figures/CA_north.pdf", useDingbats = F, width = 10)
plot(n_movement[[1]] / global(n_movement[[1]], max, na.rm = TRUE)[1,1] * 100,
     main = paste("Wind-assisted dispersal for 48h starting on 2022-10-11 00:00 UTC; n = ", length(n_points), "; Computation time: ", round(time_length(end.time - start.time, "minute"), 2), " minutes", sep = ""))
plot(st_geometry(border), add = T)
plot(st_as_sf(bind_rows(n_points), coords = c("x", "y"), crs = crs(n_movement)), add = T, col = "red")
dev.off()

for(i in 0:48){
  n_r <- rasterize(vect(n_movement[[2]] %>%
                          filter(nforecast == i) %>%
                          st_as_sf(coords = c("x", "y"),
                                   crs = crs(empty_r))),
                   empty_r,
                   fun=sum)

  png(paste("Figures/animation/", i, ".png", sep = ""), width = 600)
  plot(n_r,
       col = "red",
       main = paste("Wind-assisted dispersal starting on 2022-10-11 00:00 UTC; n = ", length(n_points), "; Hour: ", i, sep = ""),
       legend = F)
  plot(st_geometry(border), add = T)
  dev.off()
}

## list file names and read in
imgs <- gtools::mixedsort(list.files("Figures/animation/", full.names = TRUE))
img_list <- lapply(imgs, magick::image_read)

## join the images together
img_joined <- magick::image_join(img_list)

## animate at 2 frames per second
img_animated <- magick::image_animate(img_joined, fps = 5)

## save to disk
magick::image_write(image = img_animated,
                    path = "Figures/CA_north.gif")
