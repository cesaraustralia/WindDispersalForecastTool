# Rcpp version of wind_ca_model.R / wind_scr.R
#
# Usage:
#   Rcpp::sourceCpp("wind_ca.cpp")        # compiles the C++ core
#   source("wind_ca_model_cpp.R")         # R wrappers (this file)
#   res <- wind_sim_cpp(...)              # same arguments as wind_sim() + `mode`
#   res <- wind_sim_hist_cpp(...)         # same arguments as wind_sim_hist() + `mode`
#
# The simulation loops (and wind_speed / wind_direction / next_cell) live in wind_ca.cpp.
# This file handles file discovery, forecast-cycle / date logic, reading the rasters ONCE,
# and packaging the results.
#
# `mode` selects the model variant (see the top of wind_ca.cpp for the details):
#   "fixed"   (default) within-hour movement + distance budget + probabilistic rounding +
#             geometry-aware headings
#   "budget"  as "fixed" but headings picked in compass sectors (for comparison)
#   "move"    original step count, but the particle advances within the hour
#   "legacy"  reproduces the original wind_sim() behaviour
# `cellsize` is only used by "legacy" and "move". "fixed" and "budget" take the cell size
# from the raster, which must be in lon/lat.
#
# `run_info` (optional): pass an environment and it is filled with n_replicates, n_exited (replicates
# that left the raster and were dropped), engine and mode - the Shiny app uses it for its warning.
#
# Requires: Rcpp, terra, lubridate, tibble

# ---- file helpers (base-R equivalents of .wca_gfs_names / read_u / read_v) --------

# file names look like gfs_<comp>_<level>_<date>_t<hh>z_<forecast>
.wca_gfs_names <- function(path = "Data/") {
  files <- list.files(path, pattern = "^gfs_", recursive = FALSE)
  parts <- do.call(rbind, strsplit(files, "_"))
  data.frame(file = files,
             comp = parts[, 2], level = parts[, 3], date = parts[, 4],
             start = parts[, 5], forecast = parts[, 6],
             stringsAsFactors = FALSE)
}

# values of one wind component (ugrd / vgrd) for one forecast step, as a plain vector
.wca_read_wind_values <- function(path, files_list, comp, fcast, lev) {
  hit <- files_list$file[files_list$comp == comp &
                           files_list$level == lev &
                           files_list$forecast == fcast]
  if (length(hit) == 0)
    stop(sprintf("No '%s' file found for level %s, forecast %s in %s", comp, lev, fcast, path))
  terra::values(terra::rast(file.path(path, hit[1]))[[1]], mat = FALSE)
}

# ---- forecast-cycle logic (unchanged from the original) -----------------------

.wca_interval <- c("00", "06", "12", "18")

# For each simulation step, reads the best available GFS wind files for that valid time.
# Uses the most recent 6h cycle at or before `valid_time`; if that folder is missing,
# falls back to older cycles (up to 48h back). This means we need only f000-f011 from
# each cycle, so any run length works as long as enough cycles are on disk.
.wca_read_hour <- function(data_path, valid_time, level) {
  secs <- as.numeric(valid_time)
  for (lag in 0:7) {
    cs <- floor(secs / (6 * 3600)) * (6 * 3600) - lag * 21600L   # 21600 = 6 * 3600
    ct <- as.POSIXct(cs, origin = "1970-01-01", tz = "UTC")
    d  <- format(as.Date(ct), "%Y%m%d")
    hh <- sprintf("%02d", lubridate::hour(ct))
    fh <- as.integer(round((secs - cs) / 3600))
    fu <- file.path(data_path, d, hh, sprintf("gfs_ugrd_%s_%s_t%sz_f%03d", level, d, hh, fh))
    fv <- file.path(data_path, d, hh, sprintf("gfs_vgrd_%s_%s_t%sz_f%03d", level, d, hh, fh))
    if (file.exists(fu) && file.exists(fv)) {
      ru <- terra::rast(fu)
      return(list(u = terra::values(ru[[1]], mat = FALSE),
                  v = terra::values(terra::rast(fv)[[1]], mat = FALSE),
                  r = ru))
    }
  }
  stop(sprintf(
    "No GFS wind data found for %s (level %s): tried 8 cycles (48 h) back and found nothing.",
    format(valid_time, "%Y-%m-%d %H:%M UTC"), level))
}


.wca_resolve_cycle <- function(fdate, fhour, nforecast, backwards) {
  interval <- .wca_interval
  if (backwards) {
    # update date and time based on length of hindcast
    fdate <- as.character(format(as.POSIXct(lubridate::ymd_h(paste(fdate, fhour)) -
                                              lubridate::hours(as.numeric(nforecast)),
                                            format = '%m/%d/%Y %H:%M:%S'), format = '%Y%m%d'))
    fhour <- as.character(lubridate::hour(lubridate::hours(as.numeric(fhour)) -
                                            lubridate::hours(nforecast)) %% 24)
  }
  # find closest interval
  diffs <- as.numeric(fhour) - as.numeric(interval)
  diffs[which(diffs < 0)] <- NA
  index <- which.min(diffs)
  difference <- as.numeric(fhour) - as.numeric(interval[index])
  list(fdate = fdate, fhour = interval[index], index = index, difference = difference)
}

.wca_MODES <- c(legacy = 0L, move = 1L, budget = 2L, fixed = 3L)

# ---- shared driver: loop over start points, call C++, assemble outputs --------

.run_ca <- function(r, U, V, layers0, hours, coords, nsim, nforecast, cellsize,
                    backwards, full, updateProgress, mode, run_info = NULL) {

  nr <- terra::nrow(r); nc <- terra::ncol(r); ncell <- nr * nc
  if (nrow(U) != ncell || nrow(V) != ncell)
    stop("Wind rasters do not have the same dimensions as the template raster")

  mode_int <- .wca_MODES[[mode]]
  if (mode_int >= 2L) {
    if (!isTRUE(terra::is.lonlat(r, perhaps = TRUE, warn = FALSE)))
      stop("mode '", mode, "' needs a lon/lat raster: the distance budget uses degrees of ",
           "latitude/longitude. Use mode = 'legacy' or 'move' for projected grids.")
    lat_top <- terra::yFromRow(r, 1)
    dlat    <- terra::yres(r)
    dlon    <- terra::xres(r)
  } else {
    lat_top <- NA_real_; dlat <- NA_real_; dlon <- NA_real_
  }

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
    res <- ca_simulate_cpp(U, V, nr, nc, layers0, as.integer(hours),
                           cols[p], rows[p], as.integer(nsim), cellsize,
                           lat_top, dlat, dlon, backwards, full, mode_int)
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
    run_info$engine <- "cpp"
    run_info$mode <- mode
  }

  # output raster: visit counts summed over all start points, zeros -> NA
  out <- terra::rast(r[[1]])
  names(out) <- "wind_forecast"
  counts[counts == 0] <- NA
  terra::values(out) <- counts

  if (!full) return(out)

  pcol  <- unlist(lapply(pts, `[[`, "col"))
  prow  <- unlist(lapply(pts, `[[`, "row"))
  pid   <- unlist(lapply(pts, function(z) rep(z$p, length(z$col))))
  xy    <- terra::xyFromCell(r, terra::cellFromRowCol(r, prow, pcol))
  points_full <- tibble::tibble(x = xy[, "x"], y = xy[, "y"],
                                nsim = as.numeric(unlist(lapply(pts, `[[`, "sim"))),
                                nforecast = as.numeric(unlist(lapply(pts, `[[`, "hour"))),
                                x_start = longs[pid], y_start = lats[pid])
  list(out, points_full)
}

# ------------------------------------------------------------------------------
# Forecast-based simulation  (replaces wind_sim)
# `parallel` and `ncores` are accepted for compatibility but ignored: the C++ core
# is fast enough that it is not needed.
# ------------------------------------------------------------------------------
wind_sim_cpp <- function(data_path = "wind-data",
                         coords = list(),      # list of c(longitude, latitude)
                         nforecast = 24,
                         nsim = 10,
                         fdate = "20220524",
                         fhour = "18",
                         atm_level = "850mb",
                         cellsize = 25000,     # used by mode "legacy" / "move" only
                         full = FALSE,
                         parallel = FALSE,
                         ncores = FALSE,
                         backwards = FALSE,
                         updateProgress = NULL,
                         mode = c("fixed", "legacy", "move", "budget"),
                         run_info = NULL) {

  mode <- match.arg(mode)
  if (length(coords) == 0) stop("`coords` is empty")
  if (isTRUE(parallel)) message("`parallel` is ignored: the C++ version runs single-threaded and is fast.")

  # Reference time in UTC.
  #   forward:  ref_utc = start of the simulation
  #   backward: ref_utc = observation / arrival time (the simulation traces back from here)
  ref_utc <- lubridate::ymd_h(paste(fdate, fhour), tz = "UTC")

  # For each simulation step k (1..nforecast) read the best available GFS cycle for that
  # valid time.  Option B: the most recent 6h cycle at or before the valid time is used,
  # with automatic fallback to older cycles.  This means no run length is limited by how
  # many files a single cycle contains.
  r_tmpl <- NULL
  U <- V <- NULL
  forecast_hours <- if (backwards) rev(seq_len(nforecast)) else seq_len(nforecast)

  for (k in seq_len(nforecast)) {
    valid_time <- if (backwards) ref_utc - k * 3600 else ref_utc + k * 3600
    w <- .wca_read_hour(data_path, valid_time, atm_level)
    if (is.null(r_tmpl)) {
      r_tmpl <- w$r
      ncell  <- terra::ncell(r_tmpl)
      U <- matrix(NA_real_, ncell, nforecast)
      V <- matrix(NA_real_, ncell, nforecast)
    }
    U[, k] <- w$u
    V[, k] <- w$v
  }

  .run_ca(r_tmpl, U, V, layers0 = seq_len(nforecast) - 1L, hours = forecast_hours,
          coords = coords, nsim = nsim, nforecast = nforecast, cellsize = cellsize,
          backwards = backwards, full = full, updateProgress = updateProgress, mode = mode,
          run_info = run_info)
}

# ------------------------------------------------------------------------------
# Historical-data simulation  (replaces wind_sim_hist)
# data_u / data_v: multi-layer SpatRasters with layers named YYYYMMDDHH (6-hourly)
# ------------------------------------------------------------------------------
wind_sim_hist_cpp <- function(data_u = NULL,
                              data_v = NULL,
                              coords = list(),
                              nforecast = 24,
                              nsim = 10,
                              fdate = "20220524",
                              fhour = "18",
                              cellsize = 25000,
                              full = FALSE,
                              backwards = FALSE,
                              parallel = FALSE,
                              ncores = FALSE,
                              mode = c("fixed", "legacy", "move", "budget"),
                              run_info = NULL) {

  mode <- match.arg(mode)
  if (length(coords) == 0) stop("`coords` is empty")
  if (isTRUE(parallel)) message("`parallel` is ignored: the C++ version runs single-threaded and is fast.")

  r <- data_u[[1]]

  # Reference time in UTC (start for forward, observation time for backward).
  ref_utc     <- lubridate::ymd_h(paste(fdate, fhour), tz = "UTC")
  layer_times <- lubridate::ymd_h(names(data_u), tz = "UTC")

  forecast_hours <- if (backwards) rev(seq_len(nforecast)) else seq_len(nforecast)

  # For each simulation step k, pick the most recent available layer whose timestamp is
  # at or before the valid time for that step (Option B).
  lyr <- vapply(seq_len(nforecast), function(k) {
    vt  <- if (backwards) ref_utc - k * 3600 else ref_utc + k * 3600
    idx <- which(layer_times <= vt)
    if (!length(idx))
      stop(sprintf("No wind layer found at or before %s in the historical stack",
                   format(vt, "%Y-%m-%d %H UTC")))
    max(idx)
  }, integer(1))

  if (any(lyr > terra::nlyr(data_u) | lyr > terra::nlyr(data_v)))
    stop("The simulation needs wind layers beyond the end of data_u / data_v")

  used <- sort(unique(lyr))
  U <- terra::values(data_u[[used]], mat = TRUE)
  V <- terra::values(data_v[[used]], mat = TRUE)

  .run_ca(r, U, V, layers0 = match(lyr, used) - 1L, hours = forecast_hours,
          coords = coords, nsim = nsim, nforecast = nforecast, cellsize = cellsize,
          backwards = backwards, full = full, updateProgress = NULL, mode = mode,
          run_info = run_info)
}
