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
                                                fwf_empty(i))[,c(4:6)])

for (i in 1:100){
  if (i == 61) {
    hysplit_sim_traj[[i]] <- hysplit_sim_traj[[i]] %>%
      drop_na() %>%
      rename(y = X5, x = X6, nforecast = X4) %>%
      mutate(nsim = c(rep(c(1,2,3,4,5,6,7), each = 49), rep(8, 34)),
             start_point = i)
  } else if (i == 83) {
    hysplit_sim_traj[[i]] <- hysplit_sim_traj[[i]] %>%
      drop_na() %>%
      rename(y = X5, x = X6, nforecast = X4) %>%
      mutate(nsim = c(rep(c(1,2,3,4,5,6,7), each = 49), rep(8, 41)),
             start_point = i)
  } else
    hysplit_sim_traj[[i]] <- hysplit_sim_traj[[i]] %>%
      drop_na() %>%
      rename(y = X5, x = X6, nforecast = X4) %>%
      mutate(nsim = rep(c(1,2,3,4,5,6,7,8), each = 49),
             start_point = i)
}

empty_r <- ca_sim_rast[[1]]
empty_r[] <- 0

hysplit_sim_rast <- lapply(hysplit_sim_traj,
                           function(i) rasterize(vect(i %>%
                                                        filter(!nforecast == 0) %>%
                                                        st_as_sf(coords = c("x", "y"),
                                                                 crs = crs(empty_r))),
                                                 empty_r,
                                                 fun=sum))

# correlation between two raster layers
cor_ca_hysplit <- list()
lisa_ca_hysplit <- list()
r0_ca_hysplit <- numeric()
for (i in 1:100){
  ca_freq <- ca_sim_rast[[i]] / global(ca_sim_rast[[i]], max, na.rm = TRUE)[1,1] * 100
  hysplit_freq <- hysplit_sim_rast[[i]] / global(hysplit_sim_rast[[i]], max, na.rm = TRUE)[1,1] * 100

  freq_tab <- full_join(as.data.frame(ca_freq, xy = T) %>% as_tibble() %>% rename(ca = wind_forecast),
                        as.data.frame(hysplit_freq, xy = T) %>% as_tibble() %>% rename(hysplit = wind_forecast))
  freq_tab[is.na(freq_tab)] <- 0

  d_thresh <- min_distthreshold(freq_tab %>%
                                  st_as_sf(coords = c("x", "y"),
                                           crs = crs(empty_r)),
                                is_arc = F, is_mile = F)

  w <- distance_weights(freq_tab %>%
                          st_as_sf(coords = c("x", "y"),
                                   crs = crs(empty_r)),
                        is_inverse = T, dist_thres = d_thresh)


  qsa <- local_bimoran(w, as.data.frame(freq_tab[c('ca', 'hysplit')]))

  cor_ca_hysplit[[i]] <- crossCorrelation(x = freq_tab$ca,
                                          y = freq_tab$hysplit,
                                          w = as.matrix(w),
                                          dist.function = "none",
                                          # coords = as.matrix(freq_tab[,1:2]),
                                          scale.partial = T,
                                          scale.matrix = T,
                                          type = "LSCI")

  lisa_ca_hysplit[[i]] <- tibble(x = freq_tab$x,
                                 y = freq_tab$y,
                                 lisa = lisa_clusters(qsa),
                                 ca = freq_tab$ca,
                                 hysplit = freq_tab$hysplit,
                                 lsci.xy = cor_ca_hysplit[[i]]$SCI$lsci.xy,
                                 lsci.yx = cor_ca_hysplit[[i]]$SCI$lsci.yx)

  r0_ca_hysplit[i] <- cor(freq_tab$ca,
                          freq_tab$hysplit)
}

rc_ca_hysplit <- sapply(cor_ca_hysplit, "[[", "I")

rp_ca_hysplit <- r0_ca_hysplit - rc_ca_hysplit

pdf("Figures/rp.pdf", useDingbats = F)
as_tibble(rp_ca_hysplit) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = .01, colour = "grey50", fill = "dark green", alpha = .5) +
  geom_density(colour = "dark green", size = 1.5) +
  geom_vline(aes(xintercept = mean(value)), linetype = "dashed", colour = "darkgoldenrod", size = 1) +
  theme_bw() +
  xlab("Rp") +
  ggtitle("Distribution of partial spatial cross-correlation coefficients")
dev.off()

pdf("Figures/random_points.pdf", useDingbats = F, paper = "a4r")
for(i in 1:100){
  lisa_p <- lisa_ca_hysplit[[i]] %>%
    st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
    ggplot() +
    geom_sf(data = border) +
    geom_sf(aes(colour = factor(lisa, levels = c(0,1,2,3,4,5))), size = 1) +
    geom_point(aes(x = X, y = Y), data = as_tibble(start_points) %>% slice(i), size = 1.5) +
    scale_colour_manual(values = c("light grey", "dark green", "darkolivegreen3", "goldenrod1", "darkgoldenrod"),
                        labels = c("NS",
                                   "High-High",
                                   "High-Low",
                                   "Low-High",
                                   "Low-Low"),
                        name = "Frequencies") +
    ggtitle(paste("PSCC:", round(rp_ca_hysplit[[i]], 2),
                  sep = " ")) +
    theme_bw() +
    theme(legend.position = "none")

  lisa_leg <- get_legend(lisa_p + theme(legend.position = "bottom"))

  lsci_p_ca <- lisa_ca_hysplit[[i]] %>%
    ggplot(aes(x = ca, y = lsci.xy)) +
    geom_point() +
    geom_smooth(colour = "dark green", fill = "dark green") +
    theme_bw() +
    labs(x = "Frequency of overlap",
         y = "LSCI",
         title = "CA")

  lsci_p_hysplit <- lisa_ca_hysplit[[i]] %>%
    ggplot(aes(x = hysplit, y = lsci.yx)) +
    geom_point() +
    geom_smooth(colour = "darkgoldenrod", fill = "darkgoldenrod") +
    theme_bw() +
    labs(x = "Frequency of overlap",
         y = "LSCI",
         title = "HYSPLIT")

  print(plot_grid(plot_grid(lisa_p,
                            plot_grid(lsci_p_ca,
                                      lsci_p_hysplit,
                                      ncol = 1),
                            nrow = 1),
                  lisa_leg,
                  ncol = 1,
                  rel_heights = c(20, 1)))
}
dev.off()

# LISA clusters ~ distance
lisa_ca_hysplit <- lapply(1:100, function(i)
  lisa_ca_hysplit[[i]] %>%
    mutate(dist = as.numeric(st_distance(lisa_ca_hysplit[[i]] %>%
                                           st_as_sf(coords = c("x", "y"),
                                                    crs = crs(empty_r)),
                                         as_tibble(start_points) %>%
                                           slice(i) %>%
                                           st_as_sf(coords = c("X", "Y"),
                                                    crs = crs(empty_r)))/1000)
    )
)

lisa_ca_hysplit_dist <- tibble(lisa = unlist(sapply(lisa_ca_hysplit, "[[", "lisa")),
                               dist = unlist(sapply(lisa_ca_hysplit, "[[", "dist")),
                               x = unlist(sapply(1:100, function(i) rep(i, times = sapply(lisa_ca_hysplit, function(x) dim(x)[1])[i]))),
                               y = unlist(sapply(1:100, function(i) rep(start_points[i, 2], times = sapply(lisa_ca_hysplit, function(x) dim(x)[1])[i]))))

m_lisa <- mgcv::gam(list(lisa ~ s(dist) + s(x,y),
                         ~ s(dist) + s(x,y),
                         ~ s(dist) + s(x,y),
                         ~ s(dist) + s(x,y)),
                    data = lisa_ca_hysplit_dist,
                    family = multinom(K = 4))

summary(m_lisa)

m_lisa_viz <- getViz(m_lisa)

print(plot(m_lisa_viz, allTerms = T), pages = 1)

check(m_lisa_viz)

lisa_new <- with(lisa_ca_hysplit_dist,
                 expand.grid(dist = seq(1, max(dist), length = 100),
                             x = mean(x),
                             y = mean(y)))
lisa_new$ns <- stats::predict(m_lisa, lisa_new, type = "response")[,1]
lisa_new$hh <- stats::predict(m_lisa, lisa_new, type = "response")[,2]
lisa_new$hl <- stats::predict(m_lisa, lisa_new, type = "response")[,3]
lisa_new$lh <- stats::predict(m_lisa, lisa_new, type = "response")[,4]
lisa_new$ll <- stats::predict(m_lisa, lisa_new, type = "response")[,5]

pdf("Figures/lisa.pdf", useDingbats = F, width = 8)
lisa_new %>%
  gather(lisa, prob, c(ns, hh, hl, lh, ll)) %>%
  ggplot(aes(x = dist, y = prob, colour = factor(lisa, levels = c("ns", "hh", "hl", "lh", "ll")))) +
  geom_line() +
  ylim(c(0, 1)) +
  labs(y = "Probability",
       x = "Distance travelled (km)") +
  scale_colour_manual(values = c("light grey", "dark green", "darkolivegreen3", "goldenrod1", "darkgoldenrod"),
                      labels = c("NS",
                                 "High-High",
                                 "High-Low",
                                 "Low-High",
                                 "Low-Low"),
                      name = "LISA Clusters") +
  theme_bw()
dev.off()

# overlap between two raster layers
overlap <- list()
for (i in 1:100){
  hysplit_rast <- hysplit_sim_rast[[i]] / global(hysplit_sim_rast[[i]], max, na.rm = TRUE)[1,1] * 100
  ca_rast <- ca_sim_rast[[i]] / global(ca_sim_rast[[i]], max, na.rm = TRUE)[1,1] * 100

  overlap[[i]] <- full_join(as.data.frame(hysplit_rast, xy = T) %>% rownames_to_column() %>% rename(hysplit = wind_forecast),
                            as.data.frame(ca_rast, xy = T) %>% rownames_to_column() %>% rename(ca = wind_forecast)) %>%
    rowwise() %>%
    left_join(ca_sim_traj[[i]]) %>%
    mutate(overlap_prop = min(c(hysplit, ca))/max(c(hysplit, ca))) %>%
    mutate(overlap = case_when(is.na(overlap_prop) ~ 0,
                               TRUE ~ 1)) %>%
    drop_na(nforecast) %>%
    dplyr::select(nforecast, overlap, overlap_prop) %>%
    mutate(x = start_points[i, 1],
           y = start_points[i, 2])
}

# overlap ~ nforecast + (1 | c(x,y)) - GAM with spatial spline?
# try - count of 1s out of total as binomial, calculate p + ci for each of 100 starting points
m_overlap <- gam(overlap ~
                   s(nforecast) +
                   s(x, y),
                 data = bind_rows(overlap) %>%
                   group_by(x, y, nforecast) %>%
                   summarise(overlap = mean(overlap)),
                 na.action = "na.fail",
                 family = "quasibinomial")
summary(m_overlap)

m_overlap_viz <- getViz(m_overlap)

print(plot(m_overlap_viz, allTerms = T), pages = 1)

check(m_overlap_viz)

overlap_new <- with(bind_rows(overlap),
                    expand.grid(nforecast = seq(1, max(nforecast), length = 48),
                                x = mean(x),
                                y = mean(y)))
overlap_new$overlap <- stats::predict(m_overlap, overlap_new, type = "response")
err <- stats::predict(m_overlap, overlap_new, se = T, type = "response")
overlap_new$ucl_overlap <- err$fit + 1.96*err$se.fit
overlap_new$lcl_overlap <- err$fit - 1.96*err$se.fit

pdf("Figures/overlap.pdf", useDingbats = F, width = 8)
overlap_new %>%
  ggplot(aes(x = nforecast, y = overlap)) +
  geom_boxplot(data = bind_rows(overlap) %>%
                 group_by(x, y, nforecast) %>%
                 summarise(overlap = mean(overlap)),
               aes(group = nforecast),
               fill = "darkolivegreen",
               colour = "dark grey",
               alpha = .5) +
  geom_ribbon(aes(ymin = lcl_overlap, ymax = ucl_overlap), fill = "dark green", alpha = .5) +
  geom_line() +
  scale_x_continuous(limits = c(1,48), breaks = c(1, 12, 24, 36, 48), name = "Forecast hour") +
  ylim(c(0, 1)) +
  ylab("Overlap between CA and HYSPLIT") +
  theme_bw()
dev.off()

# calculate distances and angles between points
trajectories <- list(tibble(start_point = numeric(),
                            nsim = numeric(),
                            nforecast = numeric(),
                            dist = numeric(),
                            azimuth = numeric()),
                     tibble(start_point = numeric(),
                            nsim = numeric(),
                            nforecast = numeric(),
                            dist = numeric(),
                            azimuth = numeric()))
for (i in 1:100){
  for (k in 1:8){
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

    start_end <- hysplit_sim_traj[[i]] %>%
      group_by(nsim, nforecast) %>%
      slice(n()) %>%
      ungroup() %>%
      filter(nsim == k) %>%
      st_as_sf(coords = c("x", "y"), crs = crs(empty_r))

    trajectories[[2]] <-
      bind_rows(trajectories[[2]],
                tibble(start_point =  rep(i, dim(start_end)[1]-1),
                       nsim = rep(k, dim(start_end)[1]-1),
                       nforecast = seq(1, dim(start_end)[1]-1),
                       dist = as.numeric((start_end %>% st_distance()/1000)[-1,1]),
                       azimuth = sapply(2:dim(start_end)[1], function(x) nngeo::st_azimuth(start_end[1,],
                                                                                           start_end[x,])))
      )
  }
}

trajectories[[1]] <- mutate(trajectories[[1]], model = "CA")
trajectories[[2]] <- mutate(trajectories[[2]], model = "HYSPLIT")

trajectories <- bind_rows(trajectories)

trajectories <- trajectories %>%
  left_join(start_points %>%
              as_tibble() %>%
              rownames_to_column() %>%
              rename(start_point = rowname) %>%
              mutate(start_point = as.numeric(start_point)))

m_dist <- mgcv::gam(list(CA ~
                           s(nforecast) +
                           s(X, Y),
                         HYSPLIT ~
                           s(nforecast) +
                           s(X, Y)),
                    data = trajectories %>% dplyr::select(-azimuth) %>% spread(model, dist) %>% drop_na(),
                    na.action = "na.omit",
                    family = mgcv::mvn(d = 2))
summary(m_dist)

m_dist_viz <- getViz(m_dist)

print(plot(m_dist_viz, allTerms = T), pages = 1)

# check(m_dist_viz)

dist_new <- with(trajectories %>% dplyr::select(-azimuth) %>% spread(model, dist) %>% drop_na(),
                 expand.grid(nforecast = seq(1, max(nforecast), length = 48),
                             X = c(min(X), max(X)),
                             Y = c(min(Y), max(Y))))
dist_new$CA <- stats::predict(m_dist, dist_new, type = "response")[,1]
dist_new$HYSPLIT <- stats::predict(m_dist, dist_new, type = "response")[,2]

err_CA <- stats::predict(m_dist, dist_new, se = T, type = "response")$se.fit[,1]
err_HYSPLIT <- stats::predict(m_dist, dist_new, se = T, type = "response")$se.fit[,2]

pdf("Figures/dist.pdf", useDingbats = F, width = 8)
dist_new %>%
  gather(model, dist, c(CA, HYSPLIT)) %>%
  ggplot(aes(x = nforecast, y = dist, colour = model, fill = model)) +
  geom_ribbon(aes(ymin = lcl_dist, ymax = ucl_dist), alpha = .5, colour = NA,
              data = dist_new %>%
                gather(model, dist, c(CA, HYSPLIT)) %>%
                left_join(tibble(nforecast = rep(c(1:48), times = 8),
                                 model = rep(rep(c("CA", "HYSPLIT"), each = 48), times = 4),
                                 err = c(err_CA, err_HYSPLIT))) %>%
                mutate(ucl_dist = dist + 1.96*err,
                       lcl_dist = dist - 1.96*err) %>%
                group_by(nforecast, model) %>%
                mutate(lcl_dist = min(lcl_dist),
                       ucl_dist = max(ucl_dist))) +
  geom_smooth(se = F) +
  scale_x_continuous(limits = c(1,48), breaks = c(1, 12, 24, 36, 48), name = "Forecast hour") +
  scale_fill_manual(values = c("dark green", "darkgoldenrod")) +
  scale_colour_manual(values = c("dark green", "darkgoldenrod")) +
  ylab("Distance travelled") +
  theme_bw()
dev.off


m_azimuth <- mgcv::gam(deg2rad(offset) ~
                         s(nforecast) +
                         s(X, Y),
                       data = trajectories %>%
                         dplyr::select(-dist) %>%
                         spread(model, azimuth) %>%
                         mutate(offset = abs(CA - HYSPLIT)),
                       na.action = "na.omit")
summary(m_azimuth)

m_azimuth_viz <- getViz(m_azimuth)

print(plot(m_azimuth_viz, allTerms = T), pages = 1)

check(m_azimuth_viz)

azimuth_new <- with(trajectories %>% dplyr::select(-dist) %>% spread(model, azimuth),
                    expand.grid(nforecast = seq(1, max(nforecast), length = 48),
                                X = c(min(X), max(X)),
                                Y = c(min(Y), max(Y))))
azimuth_new$offset <- stats::predict(m_azimuth, azimuth_new, type = "response")

err <- stats::predict(m_azimuth, azimuth_new, se = T, type = "response")

azimuth_new$ucl_offset <- err$fit + 1.96*err$se.fit
azimuth_new$lcl_offset <- err$fit - 1.96*err$se.fit

pdf("Figures/azimuth.pdf", useDingbats = F, width = 8)
azimuth_new %>%
  group_by(nforecast) %>%
  mutate(offset = mean(offset),
         ucl_offset = max(ucl_offset),
         lcl_offset = min(lcl_offset)) %>%
  mutate_at(.vars = c("offset", "ucl_offset", "lcl_offset"), rad2deg) %>%
  gather(tempkey, tempval, c(offset, ucl_offset, lcl_offset)) %>%
  mutate(tempval = tempval + 180) %>%
  spread(tempkey, tempval) %>%
  dplyr::select(-c(X, Y)) %>%
  unique() %>%
  ggplot(aes(x = as.factor(nforecast), y = offset)) +
  geom_boxplot(aes(ymax = lcl_offset, upper = lcl_offset, ymin = ucl_offset, lower = ucl_offset, middle = offset), stat = "identity", alpha = .5, fill = "dark green", colour = NA) +
  geom_line(aes(x = nforecast, y = offset), inherit.aes = F, colour = "dark green") +
  scale_x_discrete(breaks = c("1", "12", "24", "36", "48"), name = "Forecast hour") +
  coord_polar(theta = "y",
              start = pi) +
  scale_y_continuous(limits = c(0, 360),
                     breaks = seq(0, 360, by = 45),
                     minor_breaks = seq(0, 360, by = 15),
                     labels = c("180", "225", "270", "315", "0/360", "45", "90", "135", "")) +
  ylab("Offset in angle between CA and HYSPLIT") +
  theme_bw()
dev.off


# plot trajectories, directions and distances
model_legend <- get_legend(trajectories %>%
                             filter(start_point == 1) %>%
                             ggplot(aes(x = model, fill = model, y = dist)) +
                             geom_boxplot(position = "identity", alpha = .5) +
                             scale_fill_manual(values = c("dark green", "darkgoldenrod"), name = "Model") +
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
                              scale_fill_manual(values = c("dark green", "darkgoldenrod"), name = "Model") +
                              theme_bw() +
                              theme(axis.title.x = element_blank(),
                                    legend.position = "none"),
                            plot_grid(trajectories %>%
                                        filter(start_point == i,
                                               nforecast == 48) %>%
                                        ggplot(aes(x = azimuth, fill = model)) +
                                        xlim(c(0,360)) +
                                        geom_boxplot(position = "identity", alpha = .5, outlier.shape = NA, coef = 0) +
                                        coord_polar() +
                                        theme_bw() +
                                        scale_fill_manual(values = c("dark green", "darkgoldenrod"), name = "Model") +
                                        scale_x_continuous(limits = c(0,360),
                                                           breaks = seq(0, 360, by = 45),
                                                           minor_breaks = seq(0, 360, by = 15)) +
                                        theme(axis.text.y = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.x = element_blank(),
                                              legend.position = "none"),
                                      trajectories %>%
                                        filter(start_point == i,
                                               nforecast == 48) %>%
                                        ggplot(aes(x = model, fill = model, y = dist)) +
                                        geom_boxplot(position = "identity", alpha = .5) +
                                        theme_bw() +
                                        scale_fill_manual(values = c("dark green", "darkgoldenrod"), name = "Model") +
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


## CONVERT TO DIFFERENT RESOLUTION (0.5):
empty_r <- aggregate(empty_r, fact = 2)

hysplit_sim_rast <- lapply(hysplit_sim_traj,
                           function(i) rasterize(vect(i %>%
                                                        filter(!nforecast == 0) %>%
                                                        st_as_sf(coords = c("x", "y"),
                                                                 crs = crs(empty_r))),
                                                 empty_r,
                                                 fun=sum))

ca_sim_rast <- lapply(ca_sim_traj,
                      function(i) rasterize(vect(i %>%
                                                   filter(!nforecast == 0) %>%
                                                   st_as_sf(coords = c("x", "y"),
                                                            crs = crs(empty_r))),
                                            empty_r,
                                            fun=sum))

# correlation between two raster layers
cor_ca_hysplit <- list()
lisa_ca_hysplit <- list()
r0_ca_hysplit <- numeric()
for (i in 1:100){
  ca_freq <- ca_sim_rast[[i]] / global(ca_sim_rast[[i]], max, na.rm = TRUE)[1,1] * 100
  hysplit_freq <- hysplit_sim_rast[[i]] / global(hysplit_sim_rast[[i]], max, na.rm = TRUE)[1,1] * 100

  freq_tab <- full_join(as.data.frame(ca_freq, xy = T) %>% as_tibble() %>% rename(ca = wind_forecast),
                        as.data.frame(hysplit_freq, xy = T) %>% as_tibble() %>% rename(hysplit = wind_forecast))
  freq_tab[is.na(freq_tab)] <- 0

  d_thresh <- min_distthreshold(freq_tab %>%
                                  st_as_sf(coords = c("x", "y"),
                                           crs = crs(empty_r)),
                                is_arc = F, is_mile = F)

  w <- distance_weights(freq_tab %>%
                          st_as_sf(coords = c("x", "y"),
                                   crs = crs(empty_r)),
                        is_inverse = T, dist_thres = d_thresh)


  qsa <- local_bimoran(w, as.data.frame(freq_tab[c('ca', 'hysplit')]))

  cor_ca_hysplit[[i]] <- crossCorrelation(x = freq_tab$ca,
                                          y = freq_tab$hysplit,
                                          w = as.matrix(w),
                                          dist.function = "none",
                                          # coords = as.matrix(freq_tab[,1:2]),
                                          scale.partial = T,
                                          scale.matrix = T,
                                          type = "LSCI")

  lisa_ca_hysplit[[i]] <- tibble(x = freq_tab$x,
                                 y = freq_tab$y,
                                 lisa = lisa_clusters(qsa),
                                 ca = freq_tab$ca,
                                 hysplit = freq_tab$hysplit,
                                 lsci.xy = cor_ca_hysplit[[i]]$SCI$lsci.xy,
                                 lsci.yx = cor_ca_hysplit[[i]]$SCI$lsci.yx)

  r0_ca_hysplit[i] <- cor(freq_tab$ca,
                          freq_tab$hysplit)
}

rc_ca_hysplit <- sapply(cor_ca_hysplit, "[[", "I")

rp_ca_hysplit <- r0_ca_hysplit - rc_ca_hysplit

pdf("Figures/rp_05.pdf", useDingbats = F)
as_tibble(rp_ca_hysplit) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = .01, colour = "grey50", fill = "dark green", alpha = .5) +
  geom_density(colour = "dark green", size = 1.5) +
  geom_vline(aes(xintercept = mean(value)), linetype = "dashed", colour = "darkgoldenrod", size = 1) +
  theme_bw() +
  xlab("Rp") +
  ggtitle("Distribution of partial spatial cross-correlation coefficients")
dev.off()

pdf("Figures/random_points_05.pdf", useDingbats = F, paper = "a4r")
for(i in 1:100){
  lisa_p <- lisa_ca_hysplit[[i]] %>%
    st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
    ggplot() +
    geom_sf(data = border) +
    geom_sf(aes(colour = factor(lisa, levels = c(0,1,2,3,4,5))), size = 1) +
    geom_point(aes(x = X, y = Y), data = as_tibble(start_points) %>% slice(i), size = 1.5) +
    scale_colour_manual(values = c("light grey", "dark green", "darkolivegreen3", "goldenrod1", "darkgoldenrod"),
                        labels = c("NS",
                                   "High-High",
                                   "High-Low",
                                   "Low-High",
                                   "Low-Low"),
                        name = "Frequencies") +
    ggtitle(paste("PSCC:", round(rp_ca_hysplit[[i]], 2),
                  sep = " ")) +
    theme_bw() +
    theme(legend.position = "none")

  lisa_leg <- get_legend(lisa_p + theme(legend.position = "bottom"))

  lsci_p_ca <- lisa_ca_hysplit[[i]] %>%
    ggplot(aes(x = ca, y = lsci.xy)) +
    geom_point() +
    geom_smooth(colour = "dark green", fill = "dark green") +
    theme_bw() +
    labs(x = "Frequency of overlap",
         y = "LSCI",
         title = "CA")

  lsci_p_hysplit <- lisa_ca_hysplit[[i]] %>%
    ggplot(aes(x = hysplit, y = lsci.yx)) +
    geom_point() +
    geom_smooth(colour = "darkgoldenrod", fill = "darkgoldenrod") +
    theme_bw() +
    labs(x = "Frequency of overlap",
         y = "LSCI",
         title = "HYSPLIT")

  print(plot_grid(plot_grid(lisa_p,
                            plot_grid(lsci_p_ca,
                                      lsci_p_hysplit,
                                      ncol = 1),
                            nrow = 1),
                  lisa_leg,
                  ncol = 1,
                  rel_heights = c(20, 1)))
}
dev.off()

# LISA clusters ~ distance
lisa_ca_hysplit <- lapply(1:100, function(i)
  lisa_ca_hysplit[[i]] %>%
    mutate(dist = as.numeric(st_distance(lisa_ca_hysplit[[i]] %>%
                                           st_as_sf(coords = c("x", "y"),
                                                    crs = crs(empty_r)),
                                         as_tibble(start_points) %>%
                                           slice(i) %>%
                                           st_as_sf(coords = c("X", "Y"),
                                                    crs = crs(empty_r)))/1000)
    )
)

lisa_ca_hysplit_dist <- tibble(lisa = unlist(sapply(lisa_ca_hysplit, "[[", "lisa")),
                               dist = unlist(sapply(lisa_ca_hysplit, "[[", "dist")),
                               x = unlist(sapply(1:100, function(i) rep(i, times = sapply(lisa_ca_hysplit, function(x) dim(x)[1])[i]))),
                               y = unlist(sapply(1:100, function(i) rep(start_points[i, 2], times = sapply(lisa_ca_hysplit, function(x) dim(x)[1])[i]))))

m_lisa <- mgcv::gam(list(lisa ~ s(dist) + s(x,y),
                         ~ s(dist) + s(x,y),
                         ~ s(dist) + s(x,y),
                         ~ s(dist) + s(x,y)),
                    data = lisa_ca_hysplit_dist,
                    family = multinom(K = 4))

summary(m_lisa)

m_lisa_viz <- getViz(m_lisa)

print(plot(m_lisa_viz, allTerms = T), pages = 1)

check(m_lisa_viz)

lisa_new <- with(lisa_ca_hysplit_dist,
                 expand.grid(dist = seq(1, max(dist), length = 100),
                             x = mean(x),
                             y = mean(y)))
lisa_new$ns <- stats::predict(m_lisa, lisa_new, type = "response")[,1]
lisa_new$hh <- stats::predict(m_lisa, lisa_new, type = "response")[,2]
lisa_new$hl <- stats::predict(m_lisa, lisa_new, type = "response")[,3]
lisa_new$lh <- stats::predict(m_lisa, lisa_new, type = "response")[,4]
lisa_new$ll <- stats::predict(m_lisa, lisa_new, type = "response")[,5]

pdf("Figures/lisa_05.pdf", useDingbats = F, width = 8)
lisa_new %>%
  gather(lisa, prob, c(ns, hh, hl, lh, ll)) %>%
  ggplot(aes(x = dist, y = prob, colour = factor(lisa, levels = c("ns", "hh", "hl", "lh", "ll")))) +
  geom_line() +
  ylim(c(0, 1)) +
  labs(y = "Probability",
       x = "Distance travelled (km)") +
  scale_colour_manual(values = c("light grey", "dark green", "darkolivegreen3", "goldenrod1", "darkgoldenrod"),
                      labels = c("NS",
                                 "High-High",
                                 "High-Low",
                                 "Low-High",
                                 "Low-Low"),
                      name = "LISA Clusters") +
  theme_bw()
dev.off()

## CONVERT TO DIFFERENT RESOLUTION (1):
empty_r <- aggregate(empty_r, fact = 2)

hysplit_sim_rast <- lapply(hysplit_sim_traj,
                           function(i) rasterize(vect(i %>%
                                                        filter(!nforecast == 0) %>%
                                                        st_as_sf(coords = c("x", "y"),
                                                                 crs = crs(empty_r))),
                                                 empty_r,
                                                 fun=sum))

ca_sim_rast <- lapply(ca_sim_traj,
                      function(i) rasterize(vect(i %>%
                                                   filter(!nforecast == 0) %>%
                                                   st_as_sf(coords = c("x", "y"),
                                                            crs = crs(empty_r))),
                                            empty_r,
                                            fun=sum))

# correlation between two raster layers
cor_ca_hysplit <- list()
lisa_ca_hysplit <- list()
r0_ca_hysplit <- numeric()
for (i in 1:100){
  ca_freq <- ca_sim_rast[[i]] / global(ca_sim_rast[[i]], max, na.rm = TRUE)[1,1] * 100
  hysplit_freq <- hysplit_sim_rast[[i]] / global(hysplit_sim_rast[[i]], max, na.rm = TRUE)[1,1] * 100

  freq_tab <- full_join(as.data.frame(ca_freq, xy = T) %>% as_tibble() %>% rename(ca = wind_forecast),
                        as.data.frame(hysplit_freq, xy = T) %>% as_tibble() %>% rename(hysplit = wind_forecast))
  freq_tab[is.na(freq_tab)] <- 0

  d_thresh <- min_distthreshold(freq_tab %>%
                                  st_as_sf(coords = c("x", "y"),
                                           crs = crs(empty_r)),
                                is_arc = F, is_mile = F)

  w <- distance_weights(freq_tab %>%
                          st_as_sf(coords = c("x", "y"),
                                   crs = crs(empty_r)),
                        is_inverse = T, dist_thres = d_thresh)


  qsa <- local_bimoran(w, as.data.frame(freq_tab[c('ca', 'hysplit')]))

  cor_ca_hysplit[[i]] <- crossCorrelation(x = freq_tab$ca,
                                          y = freq_tab$hysplit,
                                          w = as.matrix(w),
                                          dist.function = "none",
                                          # coords = as.matrix(freq_tab[,1:2]),
                                          scale.partial = T,
                                          scale.matrix = T,
                                          type = "LSCI")

  lisa_ca_hysplit[[i]] <- tibble(x = freq_tab$x,
                                 y = freq_tab$y,
                                 lisa = lisa_clusters(qsa),
                                 ca = freq_tab$ca,
                                 hysplit = freq_tab$hysplit,
                                 lsci.xy = cor_ca_hysplit[[i]]$SCI$lsci.xy,
                                 lsci.yx = cor_ca_hysplit[[i]]$SCI$lsci.yx)

  r0_ca_hysplit[i] <- cor(freq_tab$ca,
                          freq_tab$hysplit)
}

rc_ca_hysplit <- sapply(cor_ca_hysplit, "[[", "I")

rp_ca_hysplit <- r0_ca_hysplit - rc_ca_hysplit

pdf("Figures/rp_1.pdf", useDingbats = F)
as_tibble(rp_ca_hysplit) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = .01, colour = "grey50", fill = "dark green", alpha = .5) +
  geom_density(colour = "dark green", size = 1.5) +
  geom_vline(aes(xintercept = mean(value)), linetype = "dashed", colour = "darkgoldenrod", size = 1) +
  theme_bw() +
  xlab("Rp") +
  ggtitle("Distribution of partial spatial cross-correlation coefficients")
dev.off()

pdf("Figures/random_points_1.pdf", useDingbats = F, paper = "a4r")
for(i in 1:100){
  lisa_p <- lisa_ca_hysplit[[i]] %>%
    st_as_sf(coords = c("x", "y"), crs = crs(empty_r)) %>%
    ggplot() +
    geom_sf(data = border) +
    geom_sf(aes(colour = factor(lisa, levels = c(0,1,2,3,4,5))), size = 1) +
    geom_point(aes(x = X, y = Y), data = as_tibble(start_points) %>% slice(i), size = 1.5) +
    scale_colour_manual(values = c("light grey", "dark green", "darkolivegreen3", "goldenrod1", "darkgoldenrod"),
                        labels = c("NS",
                                   "High-High",
                                   "High-Low",
                                   "Low-High",
                                   "Low-Low"),
                        name = "Frequencies") +
    ggtitle(paste("PSCC:", round(rp_ca_hysplit[[i]], 2),
                  sep = " ")) +
    theme_bw() +
    theme(legend.position = "none")

  lisa_leg <- get_legend(lisa_p + theme(legend.position = "bottom"))

  lsci_p_ca <- lisa_ca_hysplit[[i]] %>%
    ggplot(aes(x = ca, y = lsci.xy)) +
    geom_point() +
    geom_smooth(colour = "dark green", fill = "dark green") +
    theme_bw() +
    labs(x = "Frequency of overlap",
         y = "LSCI",
         title = "CA")

  lsci_p_hysplit <- lisa_ca_hysplit[[i]] %>%
    ggplot(aes(x = hysplit, y = lsci.yx)) +
    geom_point() +
    geom_smooth(colour = "darkgoldenrod", fill = "darkgoldenrod") +
    theme_bw() +
    labs(x = "Frequency of overlap",
         y = "LSCI",
         title = "HYSPLIT")

  print(plot_grid(plot_grid(lisa_p,
                            plot_grid(lsci_p_ca,
                                      lsci_p_hysplit,
                                      ncol = 1),
                            nrow = 1),
                  lisa_leg,
                  ncol = 1,
                  rel_heights = c(20, 1)))
}
dev.off()

# LISA clusters ~ distance
lisa_ca_hysplit <- lapply(1:100, function(i)
  lisa_ca_hysplit[[i]] %>%
    mutate(dist = as.numeric(st_distance(lisa_ca_hysplit[[i]] %>%
                                           st_as_sf(coords = c("x", "y"),
                                                    crs = crs(empty_r)),
                                         as_tibble(start_points) %>%
                                           slice(i) %>%
                                           st_as_sf(coords = c("X", "Y"),
                                                    crs = crs(empty_r)))/1000)
    )
)

lisa_ca_hysplit_dist <- tibble(lisa = unlist(sapply(lisa_ca_hysplit, "[[", "lisa")),
                               dist = unlist(sapply(lisa_ca_hysplit, "[[", "dist")),
                               x = unlist(sapply(1:100, function(i) rep(i, times = sapply(lisa_ca_hysplit, function(x) dim(x)[1])[i]))),
                               y = unlist(sapply(1:100, function(i) rep(start_points[i, 2], times = sapply(lisa_ca_hysplit, function(x) dim(x)[1])[i]))))

m_lisa <- mgcv::gam(list(lisa ~ s(dist) + s(x,y),
                         ~ s(dist) + s(x,y),
                         ~ s(dist) + s(x,y),
                         ~ s(dist) + s(x,y)),
                    data = lisa_ca_hysplit_dist,
                    family = multinom(K = 4))

summary(m_lisa)

m_lisa_viz <- getViz(m_lisa)

print(plot(m_lisa_viz, allTerms = T), pages = 1)

check(m_lisa_viz)

lisa_new <- with(lisa_ca_hysplit_dist,
                 expand.grid(dist = seq(1, max(dist), length = 100),
                             x = mean(x),
                             y = mean(y)))
lisa_new$ns <- stats::predict(m_lisa, lisa_new, type = "response")[,1]
lisa_new$hh <- stats::predict(m_lisa, lisa_new, type = "response")[,2]
lisa_new$hl <- stats::predict(m_lisa, lisa_new, type = "response")[,3]
lisa_new$lh <- stats::predict(m_lisa, lisa_new, type = "response")[,4]
lisa_new$ll <- stats::predict(m_lisa, lisa_new, type = "response")[,5]

pdf("Figures/lisa_1.pdf", useDingbats = F, width = 8)
lisa_new %>%
  gather(lisa, prob, c(ns, hh, hl, lh, ll)) %>%
  ggplot(aes(x = dist, y = prob, colour = factor(lisa, levels = c("ns", "hh", "hl", "lh", "ll")))) +
  geom_line() +
  ylim(c(0, 1)) +
  labs(y = "Probability",
       x = "Distance travelled (km)") +
  scale_colour_manual(values = c("light grey", "dark green", "darkolivegreen3", "goldenrod1", "darkgoldenrod"),
                      labels = c("NS",
                                 "High-High",
                                 "High-Low",
                                 "Low-High",
                                 "Low-Low"),
                      name = "LISA Clusters") +
  theme_bw()
dev.off()
