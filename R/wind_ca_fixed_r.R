# =============================================================================
# wind_ca_fixed_r.R  -  pure-R wind cellular automaton with ALL the fixes (no Rcpp needed)
#
#   source("wind_ca_fixed_r.R")
#   res <- wind_sim_r_fixed(...)          # forecast data, same arguments as wind_sim()
#   res <- wind_sim_hist_r_fixed(...)     # historical stack, same arguments as wind_sim_hist()
#
# This is the readable reference implementation of the "fixed" model in wind_ca.cpp
# (mode = "fixed"). Both do exactly the same thing; only the random number streams differ.
#
# Fixes relative to the original wind_sim() / wind_sim_hist():
#   1. WITHIN-HOUR MOVEMENT   the original never updated x / y inside the step loop, so every
#                             step of an hour started from the same cell and a particle moved
#                             at most one cell per hour. Here it advances cell by cell.
#   2. DISTANCE BUDGET        each hour the particle gets speed * 3.6 km and each move spends
#                             its true ground length (cell height dlat * 111.2 km, width
#                             dlon * 111.2 * cos(lat) km, diagonals = the hypotenuse), instead of
#                             a step count from ceiling(speed * 3600 / cellsize) on a nominal
#                             `cellsize`.
#   3. PROBABILISTIC ROUNDING the last move, which would overshoot the budget, is taken with
#                             probability remaining_budget / move_length, so the expected
#                             distance per hour is exact (no rounding-up bias, no "at least one
#                             cell" floor for light winds).
#   4. GEOMETRY-AWARE HEADINGS the 8 neighbours are not 45 degrees apart on the ground away from
#                             the equator (cells are narrower than they are tall), so picking
#                             a compass sector rotates tracks toward north-south. The heading is
#                             converted to a move by mixing the two neighbours that bracket it
#                             on the ground, so the expected displacement follows the heading.
#   5. NO CRASHES AT EDGES    neighbours outside the grid are skipped (the original errored when
#                             a particle got within a cell of the edge); a particle that leaves
#                             the grid is dropped. Calm and NA cells mean no movement.
#   6. NO STATE LEAK          the original reset `difference` to 0 once f > 48, which then
#                             applied to later replicates; here it is applied per hour.
#   7. READ ONCE              every u / v field is read into memory once, not once per step per
#                             replicate (this is where most of the original's time went).
#
# The grid must be lon/lat (cell size in degrees). `cellsize`, `parallel` and `ncores` are
# accepted only so calls written for wind_sim() keep working; they are ignored.
#
# Requires: terra, lubridate, tibble
# =============================================================================

# ---- file helpers (same as wind_ca_model_cpp.R) ---------------------------------

.wca_gfs_names <- function(path = "Data/") {
  files <- list.files(path, pattern = "^gfs_", recursive = FALSE)
  parts <- do.call(rbind, strsplit(files, "_"))
  data.frame(file = files,
             comp = parts[, 2], level = parts[, 3], date = parts[, 4],
             start = parts[, 5], forecast = parts[, 6],
             stringsAsFactors = FALSE)
}

.wca_read_wind_values <- function(path, files_list, comp, fcast, lev) {
  hit <- files_list$file[files_list$comp == comp &
                           files_list$level == lev &
                           files_list$forecast == fcast]
  if (length(hit) == 0)
    stop(sprintf("No '%s' file found for level %s, forecast %s in %s", comp, lev, fcast, path))
  terra::values(terra::rast(file.path(path, hit[1]))[[1]], mat = FALSE)
}

.wca_interval <- c("00", "06", "12", "18")

.wca_resolve_cycle <- function(fdate, fhour, nforecast, backwards) {
  interval <- .wca_interval
  if (backwards) {
    fdate <- as.character(format(as.POSIXct(lubridate::ymd_h(paste(fdate, fhour)) -
                                              lubridate::hours(as.numeric(nforecast)),
                                            format = '%m/%d/%Y %H:%M:%S'), format = '%Y%m%d'))
    fhour <- as.character(lubridate::hour(lubridate::hours(as.numeric(fhour)) -
                                            lubridate::hours(nforecast)) %% 24)
  }
  diffs <- as.numeric(fhour) - as.numeric(interval)
  diffs[which(diffs < 0)] <- NA
  index <- which.min(diffs)
  difference <- as.numeric(fhour) - as.numeric(interval[index])
  list(fdate = fdate, fhour = interval[index], index = index, difference = difference)
}

# ---- simulation core -------------------------------------------------------------
# Same arguments and return value as ca_simulate_cpp(..., mode = 3) in wind_ca.cpp:
#   U, V          ncell x nlayers matrices of u / v (cells row-major, as terra::values())
#   nr, nc        grid rows / columns
#   layers        for each time step, 0-based column of U / V to use
#   hours         for each time step, the value stored in the `hour` output column
#   start_col/row 1-based start cell;  nsim replicates
#   lat_top       latitude of the centre of row 1;  dlat, dlon  cell size in degrees
# Returns list(counts, n_exit, and if `full`: col, row, hour, sim)   (n_exit = replicates that left the grid)

ca_simulate_r <- function(U, V, nr, nc, layers, hours, start_col, start_row, nsim,
                          lat_top, dlat, dlon, backwards = FALSE, full = FALSE) {

  RAD <- pi / 180
  KM_PER_DEG <- 6371.0088 * RAD
  ncell <- nr * nc
  nsteps <- length(layers)
  stopifnot(nrow(U) == ncell, nrow(V) == ncell, ncol(U) == ncol(V), length(hours) == nsteps,
            all(layers >= 0L & layers < ncol(U)),
            start_col >= 1L, start_col <= nc, start_row >= 1L, start_row <= nr,
            is.finite(lat_top), dlat > 0, dlon > 0)

  # neighbourhood in the same order as the original loops (col offset outer, row offset inner);
  # the centre cell (5th) counts three times
  nbx <- rep(c(-1L, 0L, 1L), each = 3L)
  nby <- rep(c(-1L, 0L, 1L), times = 3L)
  wt  <- c(1, 1, 1, 1, 3, 1, 1, 1, 1)

  # the 8 possible moves in clockwise order of ground bearing: N, NE, E, SE, S, SW, W, NW
  mv_dx <- c(0L, 1L, 1L, 1L, 0L, -1L, -1L, -1L)
  mv_dy <- c(-1L, -1L, 0L, 1L, 1L, 1L, 0L, -1L)          # rows grow southward

  # one vector per time step (copied once, not once per replicate)
  Ul <- lapply(seq_len(ncol(U)), function(k) U[, k])
  Vl <- lapply(seq_len(ncol(V)), function(k) V[, k])

  counts <- integer(ncell)
  n_exit <- 0L
  cap <- if (full) as.integer(nsim * (nsteps * 3L + 1L)) else 0L
  o_col <- o_row <- o_hour <- o_sim <- integer(cap)
  n_out <- 0L
  push <- function(cx, cy, hh, ss) {                      # grow-by-doubling output buffers
    n_out <<- n_out + 1L
    if (n_out > cap) {
      cap <<- cap * 2L
      length(o_col) <<- cap; length(o_row) <<- cap; length(o_hour) <<- cap; length(o_sim) <<- cap
    }
    o_col[n_out] <<- cx; o_row[n_out] <<- cy; o_hour[n_out] <<- hh; o_sim[n_out] <<- ss
  }

  # weighted draw of a neighbour, then its "coming from" heading plus +/-30 degrees
  pick_heading <- function(u, v, ax, ay) {
    xi <- ax + nbx; yj <- ay + nby
    ok <- xi >= 1L & xi <= nc & yj >= 1L & yj <= nr
    cell <- (yj[ok] - 1L) * nc + xi[ok]
    sp <- sqrt(v[cell]^2 + u[cell]^2)
    sp[is.na(sp)] <- 0
    p <- wt[ok] * sp
    tot <- sum(p)
    if (!(tot > 0)) return(NA_real_)                      # dead calm: no movement
    cp <- cumsum(p)
    q <- which(cp > runif(1) * tot)[1]
    if (is.na(q)) q <- max(which(p > 0))                  # rounding guard
    cs <- cell[q]
    d <- (180 + atan2(u[cs], v[cs]) * 180 / pi) %% 360
    if (backwards) d <- (d + 180) %% 360
    (d + runif(1, -30, 30)) %% 360
  }

  # turn a heading into a move that follows it on the ground; returns c(nx, ny, cost_km)
  choose_move <- function(heading, ax, ay) {
    mb <- (heading + 180) %% 360                          # bearing the particle travels along
    lat_a <- lat_top - (ay - 1L) * dlat
    lat_b <- lat_a - mv_dy * dlat
    e <- mv_dx * dlon * KM_PER_DEG * cos((lat_a + lat_b) / 2 * RAD)   # km east of each move
    n <- -mv_dy * dlat * KM_PER_DEG                                    # km north of each move
    th <- atan2(e, n) / RAD; th[th < 0] <- th[th < 0] + 360
    ka <- findInterval(mb, th)                            # neighbour just before the heading
    kb <- if (ka == 8L) 1L else ka + 1L                   # ... and just after (wraps NW -> N)
    sx <- sin(mb * RAD); sy <- cos(mb * RAD)
    det <- e[ka] * n[kb] - e[kb] * n[ka]
    alpha <- max((sx * n[kb] - e[kb] * sy) / det, 0)      # unit heading = alpha*move_a + beta*move_b
    beta  <- max((e[ka] * sy - n[ka] * sx) / det, 0)
    qa <- if (alpha + beta > 0) alpha / (alpha + beta) else 0.5
    k <- if (runif(1) < qa) ka else kb
    c(ax + mv_dx[k], ay + mv_dy[k], sqrt(e[k]^2 + n[k]^2))
  }

  for (s in seq_len(nsim)) {
    x <- start_col; y <- start_row
    if (full) push(x, y, 0L, s)
    alive <- TRUE

    for (k in seq_len(nsteps)) {
      if (!alive) break
      u <- Ul[[layers[k] + 1L]]; v <- Vl[[layers[k] + 1L]]
      f <- hours[k]

      c0 <- (y - 1L) * nc + x
      spd <- sqrt(v[c0]^2 + u[c0]^2)
      if (is.na(spd)) next                                # no wind data here: particle waits

      budget <- spd * 3.6                                 # km this hour
      for (guard in seq_len(100000L)) {
        if (!(budget > 0)) break
        heading <- pick_heading(u, v, x, y)
        if (is.na(heading)) break
        mv <- choose_move(heading, x, y)
        cost <- mv[3]
        if (!(cost > 0)) break
        if (cost > budget && runif(1) >= budget / cost) break      # last, partial move not taken
        budget <- budget - cost
        nx <- as.integer(mv[1]); ny <- as.integer(mv[2])
        if (nx < 1L || nx > nc || ny < 1L || ny > nr) { alive <- FALSE; break }   # left the grid
        idx <- (ny - 1L) * nc + nx
        counts[idx] <- counts[idx] + 1L
        if (full) push(nx, ny, f, s)
        x <- nx; y <- ny
      }
    }
    if (!alive) n_exit <- n_exit + 1L
  }

  out <- list(counts = counts, n_exit = n_exit)
  if (full) out <- c(out, list(col = o_col[seq_len(n_out)], row = o_row[seq_len(n_out)],
                               hour = o_hour[seq_len(n_out)], sim = o_sim[seq_len(n_out)]))
  out
}

# ---- driver: loop over start points, call the core, assemble outputs ---------------

.run_ca_r <- function(r, U, V, layers0, hours, coords, nsim, nforecast,
                      backwards, full, updateProgress, run_info = NULL) {

  nr <- terra::nrow(r); nc <- terra::ncol(r); ncell <- nr * nc
  if (nrow(U) != ncell || nrow(V) != ncell)
    stop("Wind rasters do not have the same dimensions as the template raster")
  if (!isTRUE(terra::is.lonlat(r, perhaps = TRUE, warn = FALSE)))
    stop("This implementation needs a lon/lat raster: the distance budget uses degrees of ",
         "latitude/longitude.")
  lat_top <- terra::yFromRow(r, 1); dlat <- terra::yres(r); dlon <- terra::xres(r)

  longs <- vapply(coords, function(z) z[1], numeric(1))
  lats  <- vapply(coords, function(z) z[2], numeric(1))
  cols  <- terra::colFromX(r, longs)
  rows  <- terra::rowFromY(r, lats)
  bad <- which(is.na(cols) | is.na(rows))
  if (length(bad))
    stop("These start coordinates fall outside the raster extent: point(s) ",
         paste(bad, collapse = ", "))

  counts <- numeric(ncell)
  n_exit <- 0L
  pts <- vector("list", length(coords))

  cat("Simulating:\n")
  pb <- utils::txtProgressBar(min = 0, max = length(coords) * nsim * nforecast,
                              style = 3, char = "=")
  for (p in seq_along(coords)) {
    res <- ca_simulate_r(U, V, nr, nc, layers0, as.integer(hours), cols[p], rows[p],
                         as.integer(nsim), lat_top, dlat, dlon, backwards, full)
    counts <- counts + res$counts
    n_exit <- n_exit + res$n_exit
    if (full) pts[[p]] <- c(res[c("col", "row", "hour", "sim")], list(p = p))
    done <- p * nsim * nforecast
    utils::setTxtProgressBar(pb, done)
    if (is.function(updateProgress)) updateProgress(value = done)
  }
  close(pb)

  if (is.environment(run_info)) {
    run_info$n_replicates <- as.integer(length(coords) * nsim)
    run_info$n_exited <- n_exit
    run_info$engine <- "r"
    run_info$mode <- "fixed"
  }

  out <- terra::rast(r[[1]])
  names(out) <- "wind_forecast"
  counts[counts == 0] <- NA
  terra::values(out) <- counts
  if (!full) return(out)

  pcol <- unlist(lapply(pts, `[[`, "col"))
  prow <- unlist(lapply(pts, `[[`, "row"))
  pid  <- unlist(lapply(pts, function(z) rep(z$p, length(z$col))))
  xy   <- terra::xyFromCell(r, terra::cellFromRowCol(r, prow, pcol))
  points_full <- tibble::tibble(x = xy[, "x"], y = xy[, "y"],
                                nsim = as.numeric(unlist(lapply(pts, `[[`, "sim"))),
                                nforecast = as.numeric(unlist(lapply(pts, `[[`, "hour"))),
                                x_start = longs[pid], y_start = lats[pid])
  list(out, points_full)
}

# ------------------------------------------------------------------------------
# Forecast-based simulation  (replaces wind_sim)
# Returns the visit-count raster, or list(raster, points) when full = TRUE.
# ------------------------------------------------------------------------------
wind_sim_r_fixed <- function(data_path = "wind-data",
                             coords = list(),      # list of c(longitude, latitude)
                             nforecast = 24,
                             nsim = 10,
                             fdate = "20220524",
                             fhour = "18",
                             atm_level = "850mb",
                             cellsize = NULL,      # ignored (the cell size comes from the raster)
                             full = FALSE,
                             parallel = FALSE,     # ignored
                             ncores = FALSE,       # ignored
                             backwards = FALSE,
                             updateProgress = NULL,
                             run_info = NULL) {

  if (length(coords) == 0) stop("`coords` is empty")

  cyc <- .wca_resolve_cycle(fdate, fhour, nforecast, backwards)
  fdate <- cyc$fdate; fhour <- cyc$fhour; difference <- cyc$difference

  pathway <- file.path(data_path, fdate, fhour)
  files <- .wca_gfs_names(path = pathway)

  npathway <- NULL
  if (nforecast > 48) {
    nfdate <- as.character(format(lubridate::ymd(fdate) + 2, "%Y%m%d"))
    npathway <- file.path(data_path, nfdate, fhour)
    nfiles <- .wca_gfs_names(path = npathway)
    nfiles$forecast <- sprintf("f%03d", as.numeric(sub("f", "", nfiles$forecast)) + 48)
    files <- rbind(files, nfiles)
  }
  forecasts <- unique(files$forecast)

  r <- terra::rast(file.path(pathway, files$file[1]))
  ncell <- terra::ncell(r)

  forecast_hours <- seq_len(nforecast)
  if (backwards) forecast_hours <- rev(forecast_hours)

  U <- matrix(NA_real_, ncell, length(forecast_hours))
  V <- matrix(NA_real_, ncell, length(forecast_hours))
  for (k in seq_along(forecast_hours)) {
    f <- forecast_hours[k]
    if (f > 48) { read_path <- npathway; diff_f <- 0 } else { read_path <- pathway; diff_f <- difference }
    fc <- forecasts[f + diff_f]
    if (is.na(fc)) stop("Forecast hour ", f, " is not available in the data")
    U[, k] <- .wca_read_wind_values(read_path, files, "ugrd", fc, atm_level)
    V[, k] <- .wca_read_wind_values(read_path, files, "vgrd", fc, atm_level)
  }

  .run_ca_r(r, U, V, layers0 = seq_along(forecast_hours) - 1L, hours = forecast_hours,
            coords = coords, nsim = nsim, nforecast = nforecast,
            backwards = backwards, full = full, updateProgress = updateProgress, run_info = run_info)
}

# ------------------------------------------------------------------------------
# Historical-data simulation  (replaces wind_sim_hist)
# data_u / data_v: multi-layer SpatRasters with layers named YYYYMMDDHH (6-hourly)
# ------------------------------------------------------------------------------
wind_sim_hist_r_fixed <- function(data_u = NULL,
                                  data_v = NULL,
                                  coords = list(),
                                  nforecast = 24,
                                  nsim = 10,
                                  fdate = "20220524",
                                  fhour = "18",
                                  cellsize = NULL,   # ignored
                                  full = FALSE,
                                  backwards = FALSE,
                                  parallel = FALSE,  # ignored
                                  ncores = FALSE,    # ignored
                                  run_info = NULL) {

  if (length(coords) == 0) stop("`coords` is empty")
  r <- data_u[[1]]

  cyc <- .wca_resolve_cycle(fdate, fhour, nforecast, backwards)
  fdate <- cyc$fdate; difference <- cyc$difference; index <- cyc$index

  start_layer <- which(names(data_u) == paste0(fdate, .wca_interval[index]))
  if (length(start_layer) != 1)
    stop("Layer '", paste0(fdate, .wca_interval[index]), "' not found in names(data_u)")

  forecast_hours <- seq_len(nforecast)
  if (backwards) forecast_hours <- rev(forecast_hours)

  lyr <- start_layer + floor((forecast_hours + difference) / 6)
  if (any(lyr > terra::nlyr(data_u) | lyr > terra::nlyr(data_v)))
    stop("The simulation needs wind layers beyond the end of data_u / data_v")

  used <- sort(unique(lyr))
  U <- terra::values(data_u[[used]], mat = TRUE)
  V <- terra::values(data_v[[used]], mat = TRUE)

  .run_ca_r(r, U, V, layers0 = match(lyr, used) - 1L, hours = forecast_hours,
            coords = coords, nsim = nsim, nforecast = nforecast,
            backwards = backwards, full = full, updateProgress = NULL, run_info = run_info)
}
