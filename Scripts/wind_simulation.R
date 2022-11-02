library(tidyverse)
library(lubridate)
library(terra)
library(sf)
library(raster)
library(spatialEco)
library(viridis)
library(cowplot)
library(mgcv)
library(mgcViz)
library(rgeoda)
library(doParallel)
library(foreach)

# read the wind functions
source("R/wind_scr.R")
source("R/wind_ca_model.R")
source("Scripts/wind_download.R")

deg2rad <- function(deg) {(deg * pi) / (180)}
rad2deg <- function(rad) {(rad * 180) / (pi)}

# select 100 random starting points
border <- st_read("SpatialData/borders.gpkg", quiet = TRUE) %>%
  filter(FID_world_ == -1)

set.seed(42)
start_points <- st_sample(border, 100)
start_points <- st_coordinates(start_points)

write_csv(as.data.frame(start_points), "start_points.csv")

# simulate wind_ca model from starting points
rand_sim <- wind_sim(data_path = c("wind-data/20221011/00"),
                     coords = lapply(seq(1, dim(start_points)[1]), function(x) as.matrix(start_points)[x,]),
                     nforecast = 48,
                     nsim = 50,
                     atm_level = "850mb",
                     full = T,
                     parallel = T)

# export CA trajectories
ca_sim_traj <- lapply(1:100,
                      function(i)
                        rand_sim[[2]] %>%
                        filter(x_start == start_points[i, 1],
                               y_start == start_points[i, 2]) %>%
                        mutate(start_point = i))
lapply(1:100,
       function(i)
         write_csv(ca_sim_traj[[i]],
                   paste0("CA-output/",
                          sprintf("%03d", i),
                          ".csv")))

empty_r <- rand_sim[[1]]
empty_r[] <- 0

# convert CA trajectories to rasters
ca_sim_rast <- lapply(ca_sim_traj,
                      function(i) rasterize(vect(i %>%
                                                   filter(!nforecast == 0) %>%
                                                   st_as_sf(coords = c("x", "y"),
                                                            crs = crs(empty_r))),
                                            empty_r,
                                            fun=sum))

# import output of HYSPLIT model and convert to raster
hysplit_sim_traj <- lapply(dir("HYSPLIT-output", full.names = T),
                           function(i) read_fwf(i,
                                                fwf_empty(i))[12:60,4:6])

for (i in 1:100){
  hysplit_sim_traj[[i]] <- hysplit_sim_traj[[i]] %>%
    drop_na() %>%
    rename(y = X5, x = X6, nforecast = X4) %>%
    mutate(start_point = i)
}

hysplit_sim_rast <- lapply(hysplit_sim_traj,
                           function(i) rasterize(vect(i %>%
                                                        filter(!nforecast == 0) %>%
                                                        st_as_sf(coords = c("x", "y"),
                                                                 crs = crs(empty_r))),
                                                 empty_r,
                                                 fun=sum))


# calculate distances and angles between points
trajectories <- list(tibble(start_point = numeric(),
                            nsim = numeric(),
                            nforecast = numeric(),
                            dist = numeric(),
                            azimuth = numeric()),
                     tibble(start_point = numeric(),
                            nforecast = numeric(),
                            dist = numeric(),
                            azimuth = numeric()))
for (i in 1:100){
  for (k in 1:50){
    start_end <- ca_sim_traj[[i]] %>%
      group_by(nsim, nforecast) %>%
      slice(n()) %>%
      ungroup() %>%
      filter(nsim == k) %>%
      st_as_sf(coords = c("x", "y"), crs = crs(empty_r))

    trajectories[[1]] <-
      bind_rows(trajectories[[1]],
                tibble(start_point =  rep(i, dim(start_end)[1]-1),
                       nsim = rep(k, dim(start_end)[1]-1),
                       nforecast = seq(1, dim(start_end)[1]-1),
                       dist = as.numeric((start_end %>% st_distance()/1000)[-1,1]),
                       azimuth = sapply(2:dim(start_end)[1], function(x) nngeo::st_azimuth(start_end[1,],
                                                                                           start_end[x,])))
      )
  }

  start_end <- hysplit_sim_traj[[i]] %>%
    group_by(nforecast) %>%
    slice(n()) %>%
    ungroup() %>%
    st_as_sf(coords = c("x", "y"), crs = crs(empty_r))

  trajectories[[2]] <-
    bind_rows(trajectories[[2]],
              tibble(start_point =  rep(i, dim(start_end)[1]-1),
                     nforecast = seq(1, dim(start_end)[1]-1),
                     dist = as.numeric((start_end %>% st_distance()/1000)[-1,1]),
                     azimuth = sapply(2:dim(start_end)[1], function(x) nngeo::st_azimuth(start_end[1,],
                                                                                         start_end[x,])))
    )

}

# Approach 1: t-tests
dist_test <- list()
azimuth_test <- list()
for (k in 1:48){
  dist_test[[k]] <- character()
  azimuth_test[[k]] <- character()
  for (i in 1:100){
    range = trajectories[[1]] %>%
      filter(start_point == i,
             nforecast == k) %>%
      dplyr::select(dist, azimuth)

    hysplit = trajectories[[2]] %>%
      filter(start_point == i,
             nforecast == k) %>%
      dplyr::select(dist, azimuth)

    t.dist <- t.test(x = range$dist, mu = hysplit$dist)

    t.azimuth <- t.test(x = deg2rad(range$azimuth), mu = deg2rad(hysplit$azimuth))

    if (t.dist$p.value > 0.05)
      dist_test[[k]][[i]] <- "same" else dist_test[[k]][[i]] <- "different"

    if (t.azimuth$p.value > 0.05)
      azimuth_test[[k]][[i]] <- "same" else azimuth_test[[k]][[i]] <- "different"
  }
}


all_t <- tibble(nforecast = 1:48,
                dist = sapply(dist_test, function(x) length(which(x == "same"))/100),
                azimuth = sapply(azimuth_test, function(x) length(which(x == "same"))/100))

# distance
round(sapply(dist_test,
             function(i) binom.test(x = length(i[which(i == "same")]),
                                    n = length(i))$p.value), 2)

# azimuth
round(sapply(azimuth_test,
             function(i) binom.test(x = length(i[which(i == "same")]),
                                    n = length(i))$p.value), 2)

png("Figures/trajectories.png", width = 600)
all_t %>%
  gather(measure, prop, -nforecast) %>%
  ggplot(aes(x = nforecast, y = 1- prop, colour = measure, fill = measure)) +
  geom_point() +
  geom_smooth()+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(0, 48, by = 4)) +
  scale_color_manual(values = c("dark green", "darkgoldenrod"), labels = c("Azimuth", "Distance"), name = "Metric") +
  scale_fill_manual(values = c("dark green", "darkgoldenrod"), labels = c("Azimuth", "Distance"), name = "Metric") +
  theme_bw() +
  labs(x = "Hour of forecast",
       y = "Proportion of trajectories significantly different")
dev.off()

# plot sample starting points
png("Figures/distances.png", height = 600)
trajectories[[1]] %>%
  dplyr::select(-azimuth) %>%
  rename(ca = dist) %>%
  left_join(trajectories[[2]] %>%
              dplyr::select(-azimuth) %>%
              rename(hysplit = dist)) %>%
  filter(nforecast == 48) %>%
  filter(start_point %in% sample(x = 1:100, size = 6)) %>%
  ggplot(aes(x = ca)) +
  geom_density() +
  geom_vline(aes(xintercept = hysplit), colour = "red") +
  theme_bw() +
  facet_wrap(vars(start_point),
             nrow = 3,
             scales = "free") +
  xlab("Maximum distance travelled (km)")
dev.off()

png("Figures/azimuth.png", height = 600)
trajectories[[1]] %>%
  dplyr::select(-dist) %>%
  rename(ca = azimuth) %>%
  left_join(trajectories[[2]] %>%
              dplyr::select(-dist) %>%
              rename(hysplit = azimuth)) %>%
  filter(nforecast == 48) %>%
  filter(start_point %in% sample(x = 1:100, size = 6)) %>%
  ggplot(aes(x = ca)) +
  geom_boxplot(aes(y = 1), outlier.shape = NA, coef = 0) +
  geom_vline(aes(xintercept = hysplit), colour = "red") +
  theme_bw() +
  coord_polar() +
  scale_x_continuous(limits = c(0, 360),
                     breaks = seq(0, 360, by = 45),
                     minor_breaks = seq(0, 360, by = 15)) +
  facet_wrap(vars(start_point),
             nrow = 3) +
  xlab("Azimuth from start to end point")
dev.off()

# Approach 2: model difference from HYSPLIT with starting point coordinates as a spatial spline
dist_diff <- list()
azimuth_diff <- list()
for (k in 1:48){
  dist_diff[[k]] <- list()
  azimuth_diff[[k]] <- list()
  for (i in 1:100){
    range = trajectories[[1]] %>%
      filter(start_point == i,
             nforecast == k) %>%
      dplyr::select(dist, azimuth)

    hysplit = trajectories[[2]] %>%
      filter(start_point == i,
             nforecast == k) %>%
      dplyr::select(dist, azimuth)

    dist_diff[[k]][[i]] <- range$dist - hysplit$dist

    azimuth_diff[[k]][[i]] <- deg2rad(abs(range$azimuth - hysplit$azimuth))
  }
}

all_diff <- bind_rows(lapply(1:48, function(x)
  tibble(start_point = as.factor(rep(1:100, times = 50)),
         dist = unlist(dist_diff[[x]]),
         azimuth = unlist(azimuth_diff[[x]]),
         nforecast = x)))

all_diff <- all_diff %>%
  mutate(x = sapply(all_diff$start_point, function(i) start_points[as.numeric(i), 1]),
         y = sapply(all_diff$start_point, function(i) start_points[as.numeric(i), 2]))

m_dist <- mgcv::bam(dist ~ s(nforecast) + s(x, y),
                    data = all_diff)

m_azimuth <- mgcv::bam(azimuth ~ s(nforecast) + s(x, y),
                       data = all_diff)

summary(m_dist)
summary(m_azimuth)

m_dist_viz <- getViz(m_dist)
print(plot(m_dist_viz, allTerms = T), pages = 1)

m_azimuth_viz <- getViz(m_azimuth)
print(plot(m_azimuth_viz, allTerms = T), pages = 1)

diff_new <- with(all_diff,
                 expand.grid(nforecast = seq(1, max(nforecast), length = 48),
                             x = seq(min(x), max(x), length = 30),
                             y = seq(min(y), max(y), length = 30)))
diff_new$dist <- stats::predict(m_dist, diff_new, type = "response")
diff_new$azimuth <- stats::predict(m_azimuth, diff_new, type = "response")


err_dist <- stats::predict(m_dist, diff_new, se = T, type = "response")$se.fit
err_azimuth <- stats::predict(m_azimuth, diff_new, se = T, type = "response")$se.fit

png("Figures/distances_gam.png", width = 600)
diff_new %>%
  ggplot(aes(x = nforecast, y = dist)) +
  geom_jitter(data = all_diff, shape = 21, fill = NA, colour = "grey", alpha = .9) +
  geom_ribbon(aes(ymin = lcl_dist, ymax = ucl_dist), fill = "darkgoldenrod", alpha = .5, colour = NA,
              data = diff_new %>%
                mutate(ucl_dist = dist + 1.96*err_dist,
                       lcl_dist = dist - 1.96*err_dist) %>%
                group_by(nforecast) %>%
                mutate(lcl_dist = min(lcl_dist),
                       ucl_dist = max(ucl_dist))) +
  geom_smooth(se = F, colour = "darkgoldenrod") +
  geom_hline(aes(yintercept = 0), colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(1,48), breaks = c(1, 12, 24, 36, 48), name = "Forecast hour") +
  ylab("Difference in distance travelled (km2; CA - HYSPLIT)") +
  theme_bw()
dev.off()

png("Figures/azimuth_gam.png", width = 600)
diff_new %>%
  ggplot(aes(x = nforecast, y = rad2deg(azimuth))) +
  geom_point(data = all_diff, shape = 21, fill = NA, colour = "grey", alpha = .9) +
  geom_ribbon(aes(ymin = rad2deg(lcl_azimuth), ymax = rad2deg(ucl_azimuth)), fill = "dark green", alpha = .5, colour = NA,
              data = diff_new %>%
                mutate(ucl_azimuth = azimuth + 1.96*err_azimuth,
                       lcl_azimuth = azimuth - 1.96*err_azimuth) %>%
                group_by(nforecast) %>%
                mutate(lcl_azimuth = min(lcl_azimuth),
                       ucl_azimuth = max(ucl_azimuth))) +
  geom_smooth(se = F, colour = "dark green") +
  geom_hline(aes(yintercept = 0), colour = "red", linetype = "dashed") +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(1,48), breaks = c(1, 12, 24, 36, 48), name = "Forecast hour") +
  scale_y_continuous(limits = c(0, 360),
                     breaks = seq(0, 360, by = 45),
                     minor_breaks = seq(0, 360, by = 15)) +
  ylab("Offset in angle between CA and HYSPLIT") +
  theme_bw()
dev.off()

png("Figures/gam_smooth.png", width = 600)
plot_grid(border %>%
            ggplot() +
            geom_raster(aes(x = x, y = y, fill = z),
                        data = plot(sm(m_dist_viz, 2))$ggObj$data %>%
                          drop_na()) +
            geom_contour(aes(x = x, y = y, z = z),
                         data = plot(sm(m_dist_viz, 2))$ggObj$data %>%
                           drop_na(), colour = "black") +
            scale_fill_viridis_c(name = "Spatial smooth s(x, y, 24.93)") +
            geom_sf(fill = NA) +
            theme_bw() +
            theme(legend.position = "bottom") +
            ggtitle("Distance"),
          border %>%
            ggplot() +
            geom_raster(aes(x = x, y = y, fill = rad2deg(z)),
                        data = plot(sm(m_azimuth_viz, 2))$ggObj$data %>%
                          drop_na()) +
            geom_contour(aes(x = x, y = y, z = rad2deg(z)),
                         data = plot(sm(m_azimuth_viz, 2))$ggObj$data %>%
                           drop_na(), colour = "black") +
            scale_fill_viridis_c(name = "Spatial smooth s(x, y, 3.92)") +
            geom_sf(fill = NA) +
            theme_bw() +
            theme(legend.position = "bottom") +
            ggtitle("Azimuth"),
          nrow = 1)
dev.off()


# plot trajectories, directions and distances
model_legend <- get_legend(bind_rows(trajectories[[1]] %>% mutate(model = "CA"),
                                     trajectories[[2]] %>% mutate(model = "HYSPLIT")) %>%
                             filter(start_point == 1) %>%
                             ggplot(aes(x = model, fill = model, y = dist)) +
                             geom_boxplot(position = "identity", alpha = .5) +
                             scale_fill_manual(values = c("dark green", "darkgoldenrod"), name = "Model") +
                             theme_bw() +
                             theme(legend.position = "bottom"))

pdf("Figures/trajectories.pdf", useDingbats = F)
for (i in 1:100) {
  print(plot_grid(plot_grid(ca_sim_traj[[i]] %>%
                              st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
                              ggplot() +
                              geom_sf(data = border, fill = "light grey") +
                              geom_path(data = ca_sim_traj[[i]],
                                        aes(x = x, y = y, group = nsim), colour = "dark green", size = 1, alpha = .1) +
                              geom_path(data = hysplit_sim_traj[[i]],
                                        aes(x = x, y = y), size = 1, colour = "darkgoldenrod") +
                              theme_bw() +
                              theme(axis.title.x = element_blank(),
                                    legend.position = "none"),
                            plot_grid(trajectories[[1]] %>%
                                        mutate(model = "CA") %>%
                                        filter(start_point == i,
                                               nforecast == 48) %>%
                                        ggplot(aes(x = azimuth, fill = model)) +
                                        xlim(c(0,360)) +
                                        geom_boxplot(position = "identity", alpha = .5, outlier.shape = NA, coef = 0) +
                                        geom_vline(data = trajectories[[2]] %>%
                                                     filter(start_point == i,
                                                            nforecast == 48),
                                                   aes(xintercept = azimuth),
                                                   colour = "darkgoldenrod",
                                                   size = 1) +
                                        coord_polar() +
                                        theme_bw() +
                                        scale_fill_manual(values = c("dark green"), name = "Model") +
                                        scale_x_continuous(limits = c(0,360),
                                                           breaks = seq(0, 360, by = 45),
                                                           minor_breaks = seq(0, 360, by = 15)) +
                                        theme(axis.text.y = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.x = element_blank(),
                                              legend.position = "none"),
                                      trajectories[[1]] %>%
                                        mutate(model = "CA") %>%
                                        filter(start_point == i,
                                               nforecast == 48) %>%
                                        ggplot(aes(x = model, fill = model, y = dist)) +
                                        geom_boxplot(position = "identity", alpha = .5) +
                                        geom_hline(data = trajectories[[2]] %>%
                                                     filter(start_point == i,
                                                            nforecast == 48),
                                                   aes(yintercept = dist),
                                                   colour = "darkgoldenrod",
                                                   size = 1) +
                                        theme_bw() +
                                        scale_fill_manual(values = c("dark green"), name = "Model") +
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

# plot some specific examples
png("Figures/trajectories - right angle, wrong distance.png", height = 1200, width = 800)
print(plot_grid(plot_grid(ca_sim_traj[[41]] %>%
                            st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
                            ggplot() +
                            geom_sf(data = border, fill = "light grey") +
                            geom_path(data = ca_sim_traj[[41]],
                                      aes(x = x, y = y, group = nsim), colour = "dark green", size = 1, alpha = .1) +
                            geom_path(data = hysplit_sim_traj[[41]],
                                      aes(x = x, y = y), size = 1, colour = "darkgoldenrod") +
                            theme_bw() +
                            theme(axis.title.x = element_blank(),
                                  legend.position = "none"),
                          plot_grid(trajectories[[1]] %>%
                                      mutate(model = "CA") %>%
                                      filter(start_point == 41,
                                             nforecast == 48) %>%
                                      ggplot(aes(x = azimuth, fill = model)) +
                                      xlim(c(0,360)) +
                                      geom_boxplot(position = "identity", alpha = .5, outlier.shape = NA, coef = 0) +
                                      geom_vline(data = trajectories[[2]] %>%
                                                   filter(start_point == 41,
                                                          nforecast == 48),
                                                 aes(xintercept = azimuth),
                                                 colour = "darkgoldenrod",
                                                 size = 1) +
                                      coord_polar() +
                                      theme_bw() +
                                      scale_fill_manual(values = c("dark green"), name = "Model") +
                                      scale_x_continuous(limits = c(0,360),
                                                         breaks = seq(0, 360, by = 45),
                                                         minor_breaks = seq(0, 360, by = 15)) +
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.x = element_blank(),
                                            legend.position = "none"),
                                    trajectories[[1]] %>%
                                      mutate(model = "CA") %>%
                                      filter(start_point == 41,
                                             nforecast == 48) %>%
                                      ggplot(aes(x = model, fill = model, y = dist)) +
                                      geom_boxplot(position = "identity", alpha = .5) +
                                      geom_hline(data = trajectories[[2]] %>%
                                                   filter(start_point == 41,
                                                          nforecast == 48),
                                                 aes(yintercept = dist),
                                                 colour = "darkgoldenrod",
                                                 size = 1) +
                                      theme_bw() +
                                      scale_fill_manual(values = c("dark green"), name = "Model") +
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
dev.off()

png("Figures/trajectories - wrong angle, right distance.png", height = 1200, width = 800)
print(plot_grid(plot_grid(ca_sim_traj[[43]] %>%
                            st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
                            ggplot() +
                            geom_sf(data = border, fill = "light grey") +
                            geom_path(data = ca_sim_traj[[43]],
                                      aes(x = x, y = y, group = nsim), colour = "dark green", size = 1, alpha = .1) +
                            geom_path(data = hysplit_sim_traj[[43]],
                                      aes(x = x, y = y), size = 1, colour = "darkgoldenrod") +
                            theme_bw() +
                            theme(axis.title.x = element_blank(),
                                  legend.position = "none"),
                          plot_grid(trajectories[[1]] %>%
                                      mutate(model = "CA") %>%
                                      filter(start_point == 43,
                                             nforecast == 48) %>%
                                      ggplot(aes(x = azimuth, fill = model)) +
                                      xlim(c(0,360)) +
                                      geom_boxplot(position = "identity", alpha = .5, outlier.shape = NA, coef = 0) +
                                      geom_vline(data = trajectories[[2]] %>%
                                                   filter(start_point == 43,
                                                          nforecast == 48),
                                                 aes(xintercept = azimuth),
                                                 colour = "darkgoldenrod",
                                                 size = 1) +
                                      coord_polar() +
                                      theme_bw() +
                                      scale_fill_manual(values = c("dark green"), name = "Model") +
                                      scale_x_continuous(limits = c(0,360),
                                                         breaks = seq(0, 360, by = 45),
                                                         minor_breaks = seq(0, 360, by = 15)) +
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.x = element_blank(),
                                            legend.position = "none"),
                                    trajectories[[1]] %>%
                                      mutate(model = "CA") %>%
                                      filter(start_point == 43,
                                             nforecast == 48) %>%
                                      ggplot(aes(x = model, fill = model, y = dist)) +
                                      geom_boxplot(position = "identity", alpha = .5) +
                                      geom_hline(data = trajectories[[2]] %>%
                                                   filter(start_point == 43,
                                                          nforecast == 48),
                                                 aes(yintercept = dist),
                                                 colour = "darkgoldenrod",
                                                 size = 1) +
                                      theme_bw() +
                                      scale_fill_manual(values = c("dark green"), name = "Model") +
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
dev.off()

png("Figures/trajectories - both wrong.png", height = 1200, width = 800)
print(plot_grid(plot_grid(ca_sim_traj[[47]] %>%
                            st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
                            ggplot() +
                            geom_sf(data = border, fill = "light grey") +
                            geom_path(data = ca_sim_traj[[47]],
                                      aes(x = x, y = y, group = nsim), colour = "dark green", size = 1, alpha = .1) +
                            geom_path(data = hysplit_sim_traj[[47]],
                                      aes(x = x, y = y), size = 1, colour = "darkgoldenrod") +
                            theme_bw() +
                            theme(axis.title.x = element_blank(),
                                  legend.position = "none"),
                          plot_grid(trajectories[[1]] %>%
                                      mutate(model = "CA") %>%
                                      filter(start_point == 47,
                                             nforecast == 48) %>%
                                      ggplot(aes(x = azimuth, fill = model)) +
                                      xlim(c(0,360)) +
                                      geom_boxplot(position = "identity", alpha = .5, outlier.shape = NA, coef = 0) +
                                      geom_vline(data = trajectories[[2]] %>%
                                                   filter(start_point == 47,
                                                          nforecast == 48),
                                                 aes(xintercept = azimuth),
                                                 colour = "darkgoldenrod",
                                                 size = 1) +
                                      coord_polar() +
                                      theme_bw() +
                                      scale_fill_manual(values = c("dark green"), name = "Model") +
                                      scale_x_continuous(limits = c(0,360),
                                                         breaks = seq(0, 360, by = 45),
                                                         minor_breaks = seq(0, 360, by = 15)) +
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.x = element_blank(),
                                            legend.position = "none"),
                                    trajectories[[1]] %>%
                                      mutate(model = "CA") %>%
                                      filter(start_point == 47,
                                             nforecast == 48) %>%
                                      ggplot(aes(x = model, fill = model, y = dist)) +
                                      geom_boxplot(position = "identity", alpha = .5) +
                                      geom_hline(data = trajectories[[2]] %>%
                                                   filter(start_point == 47,
                                                          nforecast == 48),
                                                 aes(yintercept = dist),
                                                 colour = "darkgoldenrod",
                                                 size = 1) +
                                      theme_bw() +
                                      scale_fill_manual(values = c("dark green"), name = "Model") +
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
dev.off()

png("Figures/trajectories - both very wrong.png", height = 1200, width = 800)
print(plot_grid(plot_grid(ca_sim_traj[[59]] %>%
                            st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
                            ggplot() +
                            geom_sf(data = border, fill = "light grey") +
                            geom_path(data = ca_sim_traj[[59]],
                                      aes(x = x, y = y, group = nsim), colour = "dark green", size = 1, alpha = .1) +
                            geom_path(data = hysplit_sim_traj[[59]],
                                      aes(x = x, y = y), size = 1, colour = "darkgoldenrod") +
                            theme_bw() +
                            theme(axis.title.x = element_blank(),
                                  legend.position = "none"),
                          plot_grid(trajectories[[1]] %>%
                                      mutate(model = "CA") %>%
                                      filter(start_point == 59,
                                             nforecast == 48) %>%
                                      ggplot(aes(x = azimuth, fill = model)) +
                                      xlim(c(0,360)) +
                                      geom_boxplot(position = "identity", alpha = .5, outlier.shape = NA, coef = 0) +
                                      geom_vline(data = trajectories[[2]] %>%
                                                   filter(start_point == 59,
                                                          nforecast == 48),
                                                 aes(xintercept = azimuth),
                                                 colour = "darkgoldenrod",
                                                 size = 1) +
                                      coord_polar() +
                                      theme_bw() +
                                      scale_fill_manual(values = c("dark green"), name = "Model") +
                                      scale_x_continuous(limits = c(0,360),
                                                         breaks = seq(0, 360, by = 45),
                                                         minor_breaks = seq(0, 360, by = 15)) +
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.x = element_blank(),
                                            legend.position = "none"),
                                    trajectories[[1]] %>%
                                      mutate(model = "CA") %>%
                                      filter(start_point == 59,
                                             nforecast == 48) %>%
                                      ggplot(aes(x = model, fill = model, y = dist)) +
                                      geom_boxplot(position = "identity", alpha = .5) +
                                      geom_hline(data = trajectories[[2]] %>%
                                                   filter(start_point == 59,
                                                          nforecast == 48),
                                                 aes(yintercept = dist),
                                                 colour = "darkgoldenrod",
                                                 size = 1) +
                                      theme_bw() +
                                      scale_fill_manual(values = c("dark green"), name = "Model") +
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
dev.off()

png("Figures/trajectories - both right.png", height = 1200, width = 800)
print(plot_grid(plot_grid(ca_sim_traj[[74]] %>%
                            st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
                            ggplot() +
                            geom_sf(data = border, fill = "light grey") +
                            geom_path(data = ca_sim_traj[[74]],
                                      aes(x = x, y = y, group = nsim), colour = "dark green", size = 1, alpha = .1) +
                            geom_path(data = hysplit_sim_traj[[74]],
                                      aes(x = x, y = y), size = 1, colour = "darkgoldenrod") +
                            theme_bw() +
                            theme(axis.title.x = element_blank(),
                                  legend.position = "none"),
                          plot_grid(trajectories[[1]] %>%
                                      mutate(model = "CA") %>%
                                      filter(start_point == 74,
                                             nforecast == 48) %>%
                                      ggplot(aes(x = azimuth, fill = model)) +
                                      xlim(c(0,360)) +
                                      geom_boxplot(position = "identity", alpha = .5, outlier.shape = NA, coef = 0) +
                                      geom_vline(data = trajectories[[2]] %>%
                                                   filter(start_point == 74,
                                                          nforecast == 48),
                                                 aes(xintercept = azimuth),
                                                 colour = "darkgoldenrod",
                                                 size = 1) +
                                      coord_polar() +
                                      theme_bw() +
                                      scale_fill_manual(values = c("dark green"), name = "Model") +
                                      scale_x_continuous(limits = c(0,360),
                                                         breaks = seq(0, 360, by = 45),
                                                         minor_breaks = seq(0, 360, by = 15)) +
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.x = element_blank(),
                                            legend.position = "none"),
                                    trajectories[[1]] %>%
                                      mutate(model = "CA") %>%
                                      filter(start_point == 74,
                                             nforecast == 48) %>%
                                      ggplot(aes(x = model, fill = model, y = dist)) +
                                      geom_boxplot(position = "identity", alpha = .5) +
                                      geom_hline(data = trajectories[[2]] %>%
                                                   filter(start_point == 74,
                                                          nforecast == 48),
                                                 aes(yintercept = dist),
                                                 colour = "darkgoldenrod",
                                                 size = 1) +
                                      theme_bw() +
                                      scale_fill_manual(values = c("dark green"), name = "Model") +
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
dev.off()
