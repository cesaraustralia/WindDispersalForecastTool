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

  # read every needed u / v field exactly once
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

  .run_ca(r, U, V, layers0 = seq_along(forecast_hours) - 1L, hours = forecast_hours,
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

  # only pull the layers that are actually used
  used <- sort(unique(lyr))
  U <- terra::values(data_u[[used]], mat = TRUE)
  V <- terra::values(data_v[[used]], mat = TRUE)

  .run_ca(r, U, V, layers0 = match(lyr, used) - 1L, hours = forecast_hours,
          coords = coords, nsim = nsim, nforecast = nforecast, cellsize = cellsize,
          backwards = backwards, full = full, updateProgress = NULL, mode = mode,
          run_info = run_info)
}
