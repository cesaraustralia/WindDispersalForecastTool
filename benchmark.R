#### BENCHMARK WITH 1:12 STARTING POINTS AND 8 SIMULATIONS FOR 24H EACH ####

benchmark <- tibble(n = numeric(),
                    parallel = numeric(),
                    sequential = numeric())

for (i in 1:12){
  bm_points <- lapply(seq(1, i), function(x) start_points[x,])

  benchmark[i,1] <- i

  start.time <- Sys.time()
  wind_sim(data_path = c("wind-data/20221011/00"), coords = bm_points,
           nsim = 8, atm_level = "850mb", full = F, parallel = T)
  end.time <- Sys.time()
  benchmark[i,2] <- time_length(end.time - start.time, "second")


  start.time <- Sys.time()
  wind_sim(data_path = c("wind-data/20221011/00"), coords = bm_points,
           nsim = 8, atm_level = "850mb", full = F, parallel = F)
  end.time <- Sys.time()
  benchmark[i,3] <- time_length(end.time - start.time, "second")
}

pdf("Figures/benchmark.pdf", useDingbats = F)
benchmark %>%
  gather(computation, time, -n) %>%
  ggplot(aes(x = n, y = time/60, colour = computation)) +
  geom_line() +
  scale_colour_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  theme_bw() +
  xlab("starting points") +
  ylab("computation time (minutes)")
dev.off
