# Quick self-check of the C++ core (and the pure-R core, if wind_ca_fixed_r.R is present).
# Run from the folder containing wind_ca.cpp; takes a few seconds.
# Optional: put your original wind_scr.R alongside to also compare against the R helpers.
suppressMessages({library(Rcpp); library(terra)})
sourceCpp("wind_ca.cpp")
set.seed(1)
rad <- pi / 180; KM <- 6371.0088 * rad
ok <- function(cond, label) cat(sprintf("%-6s %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))

# ---- helpers against the R originals ---------------------------------------------
if (file.exists("wind_scr.R")) {
  e1 <- new.env(); sys.source("wind_scr.R", envir = e1)
  u <- rast(nrows = 50, ncols = 50); v <- u
  values(u) <- rnorm(2500, 0, 8); values(v) <- rnorm(2500, 0, 8)
  ok(max(abs(values(e1$wind_speed(u, v))[, 1] - wind_speed_cpp(values(u)[, 1], values(v)[, 1]))) < 1e-12,
     "wind_speed matches the R original")
  ok(max(abs(values(e1$wind_direction(u, v))[, 1] - wind_direction_cpp(values(u)[, 1], values(v)[, 1]))) < 1e-9,
     "wind_direction matches the R original")
  dirs <- c(seq(0, 360, by = 0.37), 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5)
  same <- vapply(dirs, function(d) identical(as.integer(e1$next_cell(d, 10, 20)), next_cell_cpp(d, 10L, 20L)), logical(1))
  ok(all(same), "next_cell matches the R original for every tested direction")
}

# ---- legacy mode: uniform 10 m/s westerly = 2 steps/hour, 1 cell/hour -------------
nr <- nc <- 41; nh <- 24
U <- matrix(10, nr * nc, nh); V <- matrix(0, nr * nc, nh)
res <- ca_simulate_cpp(U, V, nr, nc, 0:(nh - 1), 1:nh, 3L, 20L, 500L, 25000, NA_real_, NA_real_, NA_real_,
                       FALSE, TRUE, 0L)
ok(sum(res$counts) == 500 * 2 * nh, "legacy: total visits = nsim x 2 steps x hours")
ok(abs(mean(res$col[res$hour == nh]) - 27) < 0.01, "legacy: moves one cell per hour (mean final col = 27)")

# ---- fixed mode: path length = speed * time, at several latitudes ------------------
n <- 201; d <- 0.25
for (lat0 in c(-15, -55)) {
  c0 <- (n + 1) / 2; lat_top <- lat0 + (c0 - 1) * d
  Uu <- matrix(10, n * n, 1); Vv <- matrix(0, n * n, 1)         # westerly: moves east
  r <- ca_simulate_cpp(Uu, Vv, n, n, rep(0L, nh), 1:nh, as.integer(c0), as.integer(c0), 300L, 25000,
                       lat_top, d, d, FALSE, TRUE, 3L)
  lat_row <- lat_top - (r$row - 1) * d; m <- length(r$col)
  dx <- diff(r$col) * d * KM * cos((lat_row[-1] + lat_row[-m]) / 2 * rad); dy <- diff(r$row) * d * KM
  same <- r$sim[-1] == r$sim[-m]
  path <- mean(tapply(sqrt(dx^2 + dy^2)[same], r$sim[-1][same], sum))
  ok(abs(path / (10 * 3.6 * nh) - 1) < 0.02,
     sprintf("fixed: mean path length within 2%% of speed x time at %d S (ratio %.3f)", -lat0, path / (10 * 3.6 * nh)))
}

# ---- fixed mode: heading follows the wind at high latitude (south-east wind at 55 S) ----
lat0 <- -55; c0 <- (n + 1) / 2; lat_top <- lat0 + (c0 - 1) * d
Uu <- matrix(10 * sin(135 * rad), n * n, 1); Vv <- matrix(10 * cos(135 * rad), n * n, 1)
heading_off <- function(mode) {
  r <- ca_simulate_cpp(Uu, Vv, n, n, rep(0L, nh), 1:nh, as.integer(c0), as.integer(c0), 300L, 25000,
                       lat_top, d, d, FALSE, TRUE, mode)
  last <- !duplicated(r$sim, fromLast = TRUE)
  e <- median((r$col[last] - c0) * d * KM * cos(lat0 * rad)); n_ <- median(-(r$row[last] - c0) * d * KM)
  (atan2(e, n_) / rad + 360) %% 360 - 135                        # flat-plane heading minus true heading
}
ok(abs(heading_off(3L)) < 3, sprintf("fixed: heading within 3 degrees of the wind at 55 S (offset %.1f)", heading_off(3L)))
ok(abs(heading_off(2L)) > 8, sprintf("budget (no heading fix): heading is rotated at 55 S, as expected (offset %.1f)", heading_off(2L)))

# ---- robustness ----------------------------------------------------------------------
z <- matrix(0, n * n, 3)
ok(sum(ca_simulate_cpp(z, z, n, n, 0:2, 1:3, 100L, 100L, 10L, 25000, -30, d, d, FALSE, FALSE, 3L)$counts) == 0,
   "fixed: calm field runs and nothing moves")
na <- matrix(NA_real_, n * n, 3)
ok(sum(ca_simulate_cpp(na, na, n, n, 0:2, 1:3, 100L, 100L, 10L, 25000, -30, d, d, FALSE, FALSE, 3L)$counts) == 0,
   "fixed: all-NA field runs and nothing moves")
e <- ca_simulate_cpp(matrix(10, n * n, 1), matrix(0, n * n, 1), n, n, rep(0L, nh), 1:nh, as.integer(n), 100L, 20L,
                     25000, -30, d, d, FALSE, FALSE, 3L)
ok(sum(e$counts) == 0, "fixed: particle starting on the downwind edge is dropped without error")
ok(e$n_exit == 20L, "fixed: n_exit counts every replicate that left the grid (20 of 20)")
inside <- ca_simulate_cpp(matrix(10, n * n, 1), matrix(0, n * n, 1), n, n, rep(0L, 6), 1:6, 20L, 100L, 20L,
                          25000, -30, d, d, FALSE, FALSE, 3L)
ok(inside$n_exit == 0L, "fixed: n_exit is 0 when no replicate reaches the edge")
ok(inherits(try(ca_simulate_cpp(matrix(1, 4, 1), matrix(1, 4, 1), 2L, 2L, 0L, 1L, 1L, 1L, 1L, 25000,
                                NA_real_, NA_real_, NA_real_, FALSE, FALSE, 3L), silent = TRUE), "try-error"),
   "fixed: missing lat/lon geometry is rejected with an error")

# ---- pure-R core agrees with the C++ core ----------------------------------------------
if (file.exists("wind_ca_fixed_r.R")) {
  source("wind_ca_fixed_r.R")
  lat0 <- -35; c0 <- (n + 1) / 2; lat_top <- lat0 + (c0 - 1) * d
  Uu <- matrix(10 * sin(225 * rad), n * n, 1); Vv <- matrix(10 * cos(225 * rad), n * n, 1)
  mean_end <- function(r) { last <- !duplicated(r$sim, fromLast = TRUE); c(mean(r$col[last]), mean(r$row[last])) }
  rc <- ca_simulate_cpp(Uu, Vv, n, n, rep(0L, nh), 1:nh, as.integer(c0), as.integer(c0), 600L, 25000, lat_top, d, d, FALSE, TRUE, 3L)
  rr <- ca_simulate_r(Uu, Vv, n, n, rep(0L, nh), 1:nh, as.integer(c0), as.integer(c0), 600L, lat_top, d, d, FALSE, TRUE)
  ok(all(abs(mean_end(rc) - mean_end(rr)) < 1.5),
     sprintf("pure-R core agrees with the C++ core (mean end cell C++ %s, R %s)",
             paste(round(mean_end(rc), 1), collapse = "/"), paste(round(mean_end(rr), 1), collapse = "/")))
}
