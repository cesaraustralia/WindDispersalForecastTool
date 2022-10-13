library(tidyverse)
library(lubridate)
library(terra)
library(sf)
library(raster)
library(spatialEco)
library(viridis)
library(cowplot)

# read the wind functions
source("R/wind_scr.R")
source("R/wind_ca_model.R")
source("Scripts/wind_download.R")

deg2rad <- function(deg) {(deg * pi) / (180)}

# select 100 random starting points
border <- st_read("SpatialData/borders.gpkg", quiet = TRUE) %>%
  filter(FID_world_ == -1)

set.seed(42)
start_points <- st_sample(border, 100)
start_points <- st_coordinates(start_points)

write_csv(as.data.frame(start_points), "start_points.csv")

# simulate wind_ca model from starting points
set.seed(42)
rand_sim <- list()
for(i in 1:100){
  rand_sim[[i]] <- wind_sim(data_path = c("wind-data/20221011/00"),
                            long = start_points[i,1],
                            lat = start_points[i,2],
                            nforecast = 48,
                            nsim = 8,
                            atm_level = "850mb",
                            full = T)
}

ca_sim_rast <- lapply(rand_sim, "[[", 1)
ca_sim_traj <- lapply(1:100, function(i) lapply(rand_sim, "[[", 2)[[i]] %>% mutate(start_point = i))

lapply(1:100,
       function(x)
         write_csv(ca_sim_traj[[x]], file.path("CA-output", sprintf("%03d.csv", x))))

# import output of HYSPLIT model and convert to raster
hysplit_sim_traj <- lapply(dir("HYSPLIT-output", full.names = T),
                           function(i) read_fwf(i,
                                                fwf_empty(i))[,c(5,6)])

for (i in 1:100){
  if (i == 61) {
    hysplit_sim_traj[[i]] <- hysplit_sim_traj[[i]] %>%
      drop_na() %>%
      rename(y = X5, x = X6) %>%
      mutate(nsim = c(rep(c(1,2,3,4,5,6,7), each = 49), rep(8, 34)),
             start_point = i)
  } else if (i == 83) {
    hysplit_sim_traj[[i]] <- hysplit_sim_traj[[i]] %>%
      drop_na() %>%
      rename(y = X5, x = X6) %>%
      mutate(nsim = c(rep(c(1,2,3,4,5,6,7), each = 49), rep(8, 41)),
             start_point = i)
  } else
    hysplit_sim_traj[[i]] <- hysplit_sim_traj[[i]] %>%
      drop_na() %>%
      rename(y = X5, x = X6) %>%
      mutate(nsim = rep(c(1,2,3,4,5,6,7,8), each = 49),
             start_point = i)
}

empty_r <- ca_sim_rast[[1]]
empty_r[] <- 0

hysplit_sim_rast <- lapply(hysplit_sim_traj,
                           function(i) rasterize(vect(i %>%
                                                        st_as_sf(coords = c("x", "y"),
                                                                 crs = crs(empty_r))),
                                                 empty_r,
                                                 fun=sum))

# correlation between two raster layers
cor_ca_hysplit <- list()
lisa_ca_hysplit <- list()
for (i in 1:100){
  ca_freq <- ca_sim_rast[[i]] / global(ca_sim_rast[[i]], max, na.rm = TRUE)[1,1] * 100
  hysplit_freq <- hysplit_sim_rast[[i]] / global(hysplit_sim_rast[[i]], max, na.rm = TRUE)[1,1] * 100

  freq_tab <- full_join(as.data.frame(ca_freq, xy = T) %>% as_tibble() %>% rename(ca = wind_forecast),
                        as.data.frame(hysplit_freq, xy = T) %>% as_tibble() %>% rename(hysplit = wind_forecast))
  freq_tab[is.na(freq_tab)] <- 0

  cor_ca_hysplit[[i]] <- crossCorrelation(x = freq_tab$ca,
                                          y = freq_tab$hysplit,
                                          coords = as.matrix(freq_tab[,1:2]),
                                          type = "GSCI")

  lisa_ca_hysplit[[i]] <- tibble(x = freq_tab$x,
                                 y = freq_tab$y,
                                 lisa = cor_ca_hysplit[[i]]$clusters)
}

pdf("Figures/random_points.pdf", useDingbats = F)
for(i in 1:100){
  print(lisa_ca_hysplit[[i]] %>%
          st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
          ggplot() +
          geom_sf(data = border) +
          geom_sf(aes(colour = lisa), size = 1) +
          scale_colour_manual(values = c("red", "pink", "light blue", "blue"),
                              labels = c("High CA - High HYSPLIT",
                                         "High CA - Low HYSPLIT",
                                         "Low CA - High HYSPLIT",
                                         "Low CA - Low HYSPLIT"),
                              name = "Frequencies") +
          ggtitle(paste("Global Moran's I:", round(cor_ca_hysplit[[i]]$I, 2),
                        "p:", round(cor_ca_hysplit[[i]]$global.p, 2),
                        sep = " ")))
}
dev.off()

# calculate distances and angles between points
trajectories <- list(tibble(start_point = numeric(),
                            nsim = numeric(),
                            dist = numeric(),
                            azimuth = numeric()),
                     tibble(start_point = numeric(),
                            nsim = numeric(),
                            dist = numeric(),
                            azimuth = numeric()))
for (i in 1:100){
  for (k in 1:8){
    start_end <- ca_sim_traj[[i]] %>%
      group_by(nsim) %>%
      slice(c(1, n())) %>%
      ungroup() %>%
      filter(nsim == k) %>%
      st_as_sf(coords = c("x", "y"), crs = crs(empty_r))

    trajectories[[1]] <-
      bind_rows(trajectories[[1]],
                setNames(c(i,
                           k,
                           as.numeric(start_end %>%
                                        st_distance()/1000)[2],
                           nngeo::st_azimuth(start_end[1,],
                                             start_end[2,])),
                         c("start_point", "nsim", "dist", "azimuth")))

    start_end <- hysplit_sim_traj[[i]] %>%
      group_by(nsim) %>%
      slice(c(1, n())) %>%
      ungroup() %>%
      filter(nsim == k) %>%
      st_as_sf(coords = c("x", "y"), crs = crs(empty_r))

    trajectories[[2]] <-
      bind_rows(trajectories[[2]],
                setNames(c(i,
                           k,
                           as.numeric(start_end %>%
                                        st_distance()/1000)[2],
                           nngeo::st_azimuth(start_end[1,],
                                             start_end[2,])),
                         c("start_point", "nsim", "dist", "azimuth")))
  }
}

trajectories[[1]] <- mutate(trajectories[[1]], model = "CA")
trajectories[[2]] <- mutate(trajectories[[2]], model = "HYSPLIT")

trajectories <- bind_rows(trajectories)

### MIXED EFFECTS MODELS TO COMPARE DISTANCES AND AZIMUTHS

# plot trajectories, directions and distances
model_legend <- get_legend(trajectories %>%
                             filter(start_point == 1) %>%
                             ggplot(aes(x = model, fill = model, y = dist)) +
                             geom_boxplot(position = "identity", alpha = .5) +
                             scale_fill_viridis_d(name = "Model") +
                             theme_bw() +
                             theme(legend.position = "bottom"))

pdf("Figures/trajectories.pdf", useDingbats = F)
for (i in 1:100) {
  print(plot_grid(plot_grid(bind_rows(ca_sim_traj[[i]] %>%
                                        st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
                                        mutate(model = "CA"),
                                      hysplit_sim_traj[[i]] %>%
                                        st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
                                        mutate(model = "HYSPLIT")) %>%
                              ggplot() +
                              geom_sf(data = border, fill = "light grey") +
                              geom_sf(aes(fill = model, group = nsim), shape = 21) +
                              scale_fill_viridis_d(name = "Model") +
                              theme_bw() +
                              theme(axis.title.x = element_blank(),
                                    legend.position = "none"),
                            plot_grid(trajectories %>%
                                        filter(start_point == i) %>%
                                        ggplot(aes(x = azimuth, fill = model)) +
                                        xlim(c(0,360)) +
                                        geom_boxplot(position = "identity", alpha = .5, outlier.shape = NA, coef = 0) +
                                        coord_polar() +
                                        theme_bw() +
                                        scale_fill_viridis_d(name = "Model") +
                                        scale_x_continuous(limits = c(0,360),
                                                           breaks = seq(0, 360, by = 45),
                                                           minor_breaks = seq(0, 360, by = 15)) +
                                        theme(axis.text.y = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.x = element_blank(),
                                              legend.position = "none"),
                                      trajectories %>%
                                        filter(start_point == i) %>%
                                        ggplot(aes(x = model, fill = model, y = dist)) +
                                        geom_boxplot(position = "identity", alpha = .5) +
                                        theme_bw() +
                                        scale_fill_viridis_d(name = "Model") +
                                        theme(axis.text.y = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.x = element_blank(),
                                              legend.position = "none") +
                                        labs(y = "Distance travelled (km)"),
                                      ncol = 1),
                            nrow = 1),
                  model_legend,
                  ncol = 1,
                  rel_heights = c(20, 1)))
}
dev.off()
