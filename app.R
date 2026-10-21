library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(lubridate)
library(leaflet)
library(terra)
library(rasterVis)
library(sf)
library(photobiology)


# settings ----------------------------------------------------------------------

APP_VERSION <- "v1.1.0"
NSIM <- 100                          # replicates per start location
WIND_DIR <- "wind-data"
ATM_LEVEL <- "950mb"
MAX_POINTS <- 5                      # max start locations on the custom-simulation tab
EXIT_WARN_FRACTION <- 0.10           # warn when more than this share of replicates leave the model area
LATEST_TZ <- "Australia/Brisbane"    # forecasts are available from about 08:00 Brisbane time
MODEL_VERSION <- "fixed-v1"          # bump to invalidate the cached multi-site forecasts

# the multi-site forecast is identical for every user on a given day, so it is cached here
# (shared by all R processes in the container; set WIND_CACHE_DIR to use another folder)
CACHE_DIR <- Sys.getenv("WIND_CACHE_DIR",
                        unset = file.path(dirname(tempdir()), "wind-forecast-cache"))
CACHE_MAX_AGE_DAYS <- 3


# wind model --------------------------------------------------------------------
# Shiny loads everything in R/ automatically; these lines only make sure the functions exist.
# R/wind_scr.R and R/wind_ca_model.R (the original model) are no longer used by the app.
if (!exists("wind_sim_cpp", mode = "function")) source("R/wind_ca_model_cpp.R")
if (!exists("wind_sim_r_fixed", mode = "function")) source("R/wind_ca_fixed_r.R")

# The "fixed" model is run in C++ when a compiler is available. If compiling fails the app falls
# back to the pure-R implementation of the same model (a little slower, same results).
WIND_ENGINE <- tryCatch({
  if (!requireNamespace("Rcpp", quietly = TRUE)) stop("package Rcpp is not installed")
  Rcpp::sourceCpp("R/wind_ca.cpp", env = globalenv())
  "cpp"
}, error = function(e) {
  message("[wind model] C++ model unavailable (",
          substr(gsub("\\s+", " ", conditionMessage(e)), 1, 300),
          ") - using the pure-R implementation of the same model.")
  "r"
})
message("[wind model] engine: ", WIND_ENGINE)

run_wind_model <- function(coords, nforecast, fdate, fhour, backwards, updateProgress, run_info) {
  args <- list(
    data_path = WIND_DIR,
    coords = coords,
    nforecast = nforecast,
    nsim = NSIM,
    fdate = fdate,
    fhour = fhour,
    atm_level = ATM_LEVEL,
    backwards = backwards,
    updateProgress = updateProgress,
    run_info = run_info
  )
  if (WIND_ENGINE == "cpp") {
    do.call(wind_sim_cpp, c(args, list(mode = "fixed")))
  } else {
    do.call(wind_sim_r_fixed, args)
  }
}

# runs the model with a progress bar; returns the raster and the model's run information
simulate_with_progress <- function(coords, nforecast, fdate, fhour, backwards) {
  # Create a Progress object
  progress <- shiny::Progress$new(max = NSIM * nforecast * length(coords))

  progress$set(message = "Simulating", value = 0)
  # Close the progress when this function exits (even if there's an error)
  on.exit(progress$close())

  # Create a callback function to update progress.
  # Each time this is called:
  # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
  #   distance. If non-NULL, it will set the progress to that value.
  # - It also accepts optional detail text.
  updateProgress <- function(value = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value)
  }

  info <- new.env()
  r <- run_wind_model(coords, nforecast, fdate, fhour, backwards, updateProgress, info)
  list(raster = r, info = as.list(info))
}

# Which 6h GFS cycles does this run need that are not on the server (yet)?
# Uses Option B: each simulation hour uses the most recent available cycle, so the set of
# required cycles is all 6h boundaries bracketed by the run's time window, plus each one's
# immediate predecessor (needed as a fallback when .wca_read_hour cannot find the ideal cycle).
# Returns a character vector of missing cycles (empty if everything is present).
missing_forecasts <- function(fdate, fhour, nforecast, backwards) {
  ref_utc <- lubridate::ymd_h(paste(fdate, fhour), tz = "UTC")
  secs    <- as.numeric(ref_utc)

  # all valid times this run will need
  hour_secs <- secs + (if (backwards) -seq_len(nforecast) else seq_len(nforecast)) * 3600L

  # unique ideal cycles (the most recent 6h boundary at or before each valid time)
  ideal_cs <- unique(floor(hour_secs / (6 * 3600)) * (6 * 3600))

  missing <- character(0)
  for (cs in ideal_cs) {
    # a cycle is "covered" when it OR the immediately preceding cycle (the fallback) has data
    found <- FALSE
    for (lag in 0:1) {
      check_cs <- cs - lag * 21600L
      ct <- as.POSIXct(check_cs, origin = "1970-01-01", tz = "UTC")
      d  <- format(as.Date(ct), "%Y%m%d")
      hh <- sprintf("%02d", lubridate::hour(ct))
      folder <- file.path(WIND_DIR, d, hh)
      if (dir.exists(folder) &&
          length(list.files(folder, pattern = paste0("^gfs_ugrd_", ATM_LEVEL, "_"))) > 0) {
        found <- TRUE; break
      }
    }
    if (!found) {
      ct  <- as.POSIXct(cs, origin = "1970-01-01", tz = "UTC")
      lbl <- sprintf("%s %s:00 UTC",
                     format(as.Date(ct), "%d %b %Y"),
                     sprintf("%02d", lubridate::hour(ct)))
      if (!lbl %in% missing) missing <- c(missing, lbl)
    }
  }
  missing
}

# runs `fun`; on failure logs the error, tells the user, and returns NULL instead of ending the session
run_safely <- function(fun) {
  tryCatch(fun(), error = function(e) {
    message("[wind app] simulation failed: ", conditionMessage(e))
    showNotification(
      tagList(
        strong("The simulation could not be completed."),
        br(),
        "If you are running it close to the latest forecast time, the forecast may still be ",
        "downloading - please try again in a few minutes.",
        br(),
        tags$small(conditionMessage(e))
      ),
      type = "error",
      duration = 20
    )
    NULL
  })
}


# cache for the multi-site forecast ---------------------------------------------------

md5_of <- function(x) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(paste(deparse(x), collapse = "\n"), f)
  unname(tools::md5sum(f))
}

CACHE_OK <- tryCatch({
  dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  old <- list.files(CACHE_DIR, full.names = TRUE)
  old <- old[difftime(Sys.time(), file.mtime(old), units = "days") > CACHE_MAX_AGE_DAYS]
  unlink(old)
  file.access(CACHE_DIR, 2) == 0
}, error = function(e) FALSE)

# the key changes whenever the model, its settings or the localities change
CODE_HASH <- md5_of(unname(tools::md5sum(c("R/wind_ca.cpp", "R/wind_ca_model_cpp.R",
                                           "R/wind_ca_fixed_r.R", "localities.csv"))))

cache_file <- function(r_date, fhour, night_length) {
  key <- md5_of(list(MODEL_VERSION, NSIM, ATM_LEVEL, r_date, fhour, as.numeric(night_length), CODE_HASH))
  file.path(CACHE_DIR, sprintf("multisite_%s_%s.rds", r_date, substr(key, 1, 12)))
}

read_cache <- function(file) {
  if (!CACHE_OK || !file.exists(file)) return(NULL)
  tryCatch({
    obj <- readRDS(file)
    list(raster = terra::unwrap(obj$raster), info = obj$info)
  }, error = function(e) NULL)
}

write_cache <- function(file, res) {
  if (!CACHE_OK) return(invisible(FALSE))
  tryCatch({
    tmp <- tempfile(tmpdir = CACHE_DIR, fileext = ".tmp")
    saveRDS(list(raster = terra::wrap(res$raster), info = res$info), tmp)
    file.rename(tmp, file)   # atomic, so another session never reads a half-written file
  }, error = function(e) invisible(FALSE))
}


# spatial data -------------------------------------------------------------------

# read Australian border
border <- st_read("SpatialData/borders.gpkg", quiet = TRUE) %>%
  sf::st_cast(to = "MULTILINESTRING")
# add the bbox of the raster as the allowed region
bound <- st_read("SpatialData/boundary.gpkg", quiet = TRUE)
# outline of the model area, drawn on the result plots
bound_outline <- sf::st_boundary(sf::st_transform(sf::st_geometry(bound), 4326))

# returns true or false: is the point inside the model area?
xy_in_aus <- function(long, lat) {
  pts <- data.frame(x = long, y = lat) %>%
    sf::st_as_sf(coords = 1:2, crs = 4326) %>%
    sf::st_transform(crs = sf::st_crs(bound))
  any(lengths(sf::st_intersects(pts, sf::st_geometry(bound))) > 0)
}

# load localities for rapid prediction
localities <- as.data.frame(read_csv("localities.csv"))
loc_xy <- data.frame(long = as.numeric(localities[, 2]), lat = as.numeric(localities[, 3]))

# Define options for timezone dropdown menu
tz_choices <- c(
  "Australia/Adelaide",
  "Australia/Brisbane",
  "Australia/Broken_Hill",
  "Australia/Darwin",
  "Australia/Eucla",
  "Australia/Hobart",
  "Australia/Lindeman",
  "Australia/Lord_Howe",
  "Australia/Melbourne",
  "Australia/Perth",
  "Australia/Sydney"
)


# helpers ------------------------------------------------------------------------

# the latest start time for which forecasts are available (08:00 Brisbane time today), in `tz`
latest_start <- function(tz) {
  today_bne <- lubridate::today(tzone = LATEST_TZ)
  lubridate::with_tz(
    lubridate::ymd_hms(paste0(format(today_bne, "%Y-%m-%d"), " ", "08:00:00"), tz = LATEST_TZ),
    tz
  )
}

# start times offered for `date` in timezone `tz` (whole UTC hours, shown in local time)
time_choices_for <- function(date, tz) {
  # original choices
  original_choices <-
    lubridate::with_tz(lubridate::ymd_hms(paste0(
      format(date, "%Y-%m-%d"),
      " ",
      sprintf("%s:00:00", seq(0, 23))
    ),
    tz = "Australia/Melbourne"),
    tz = tz)

  original_choices <- lubridate::ymd_hms(paste0(
    format(date, "%Y-%m-%d"),
    " ",
    format(original_choices, "%H:%M:%S")
  ),
  tz = tz)

  # filter original choices by the latest available start time
  filtered_choices <- sort(original_choices[original_choices <= latest_start(tz)])

  format(filtered_choices, "%H:%M")
}

# map extent used for a set of start locations
plot_extent <- function(pts) {
  c(
    floor(min(pts$long, na.rm = TRUE)) - 18,
    floor(max(pts$long, na.rm = TRUE)) + 18,
    floor(min(pts$lat, na.rm = TRUE)) - 15,
    floor(max(pts$lat, na.rm = TRUE)) + 15
  )
}

# warning shown above the map when many replicates left the model area
exit_notice <- function(info) {
  if (is.null(info) || is.null(info$n_replicates) || info$n_replicates == 0) return(NULL)
  frac <- info$n_exited / info$n_replicates
  if (frac <= EXIT_WARN_FRACTION) return(NULL)
  div(
    class = "alert alert-warning",
    style = "margin-top: 10px;",
    strong("Warning: "),
    sprintf(
      paste0("%.0f%% of the simulated trajectories left the model area (dashed outline) and could ",
             "not be followed further, so the pattern is cut off at its edge. ",
             "Treat the map near the edge with caution."),
      100 * frac
    )
  )
}

# result map (used by both tabs)
make_prediction_plot <- function(r, pts, title) {
  xt <- plot_extent(pts)

  r_crop <- terra::crop(r, xt)
  r_crop <- r_crop / global(r_crop, max, na.rm = TRUE)[1, 1] * 100

  # the part of the model-area outline that falls inside the map (may be none)
  outline <- tryCatch(suppressWarnings(st_crop(bound_outline, ext(xt))),
                      error = function(e) NULL)

  p <- gplot(r_crop, maxpixels = 500000) +
    geom_tile(aes(fill = value), alpha = 0.8) +
    viridis::scale_fill_viridis(option = "D",
                                direction = -1,
                                na.value = NA) +
    geom_point(
      data = pts,
      aes(x = long, y = lat),
      inherit.aes = FALSE,
      col = "red",
      alpha = 0.8,
      shape = "\u2605",
      size = 7
    ) +
    geom_sf(
      data = st_crop(border, ext(xt)),
      inherit.aes = FALSE,
      fill = NA
    )

  if (!is.null(outline) && length(outline) > 0) {
    p <- p + geom_sf(
      data = outline,
      inherit.aes = FALSE,
      colour = "#d9534f",
      linetype = "dashed",
      linewidth = 0.8,
      fill = NA
    )
  }

  p +
    coord_sf(crs = 4326) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14),
      plot.caption = element_text(size = 11, hjust = 0),
      plot.caption.position = "plot",
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 15),
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13),
      axis.title.x = element_text(margin = margin(
        t = 15,
        r = 0,
        b = 0,
        l = 0
      )),
      axis.title.y = element_text(margin = margin(
        t = 0,
        r = 15,
        b = 0,
        l = 0
      ))
    ) +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "Frequency",
         caption = "Dashed red line: edge of the model area.\nTrajectories that leave it are not followed.") +
    ggtitle(title)
}

# base map for both tabs, with the model area outlined
base_map <- function() {
  leaflet(bound) %>%
    setView(lng = 135.51,
            lat = -25.98,
            zoom = 3) %>%
    addTiles() %>%
    addPolygons(fillOpacity = 0, color = "#d9534f", weight = 2, dashArray = "6 4") %>%
    addControl(html = "<small>Dashed outline: model area</small>", position = "bottomleft")
}

# one row of the coordinate list on the custom-simulation tab
coord_rows <- lapply(seq_len(MAX_POINTS), function(i) {
  fluidRow(
    column(4, textOutput(paste0("coord", 2 * i - 1))),
    column(4, textOutput(paste0("coord", 2 * i))),
    column(4, actionButton(paste0("delete", i), "Delete"))
  )
})

model_note <- h6(
  sprintf(
    paste0("Model update (%s): particles now travel the full distance carried by the wind ",
           "(previously capped at roughly one grid cell, about 25 km, per hour), ",
           "so maps are not directly comparable with those from earlier versions."),
    APP_VERSION
  )
)


# ui ------------------------------------------------------------------------------

ui <- shinyUI(
  navbarPage(
    paste("Wind Forecast Tool", APP_VERSION),
    selected = "Select location for rapid simulation",
    theme = shinytheme("yeti"),

    # Tab 1: custom simulation --------------------------------------------------

    tabPanel(
      "Select location for rapid simulation",

      column(
        width = 4,

        h5("Select simulation direction:"),
        selectInput(
          "direction",
          "Direction",
          choices = c("Forward", "Backward"),
          selected = "Forward"
        ),

        h5("Select start time for simulation:"),
        selectInput(
          "timezone",
          "Timezone",
          choices = tz_choices,
          selected = "Australia/Melbourne"
        ),

        uiOutput("warning_text"),

        shiny::splitLayout(
          uiOutput("forec_date"),
          uiOutput("forec_time")
        ),

        uiOutput("duration_slider"),

        coord_rows,

        # add a leaflet map
        leafletOutput("smap", height = 250),

        h6(
          "* If your simulation fails around this time, try again in a few minutes - the forecasts may not have finished downloading yet."
        ),

        model_note

      ),
      column(
        width = 8,

        HTML("<br/>"),
        actionButton("run", "Run forecast"),
        HTML("<br/>"),

        uiOutput("prediction_notice"),

        plotOutput("prediction", height = "500px") %>%
          withSpinner(color = "#428bca")

      )
    )
    ,

    # Tab 2: multi-site northern prediction -----------------------------------------

    tabPanel(
      "Multi-site Northern Prediction",

      column(
        width = 4,

        h5("Select timezone:"),
        selectInput(
          "timezone_r",
          "Timezone",
          choices = tz_choices,
          selected = "Australia/Melbourne"
        ),

        h5("Simulate overnight wind-assisted dispersal from the following localities:"),

        leafletOutput("map_r", height = 250),

        h6(
          "* The forecast is calculated once per day and shared, so the first run of the day may take a little longer than later ones."
        ),

        model_note

      ),
      column(
        width = 8,

        HTML("<br/>"),
        actionButton("run_r", "Run forecast"),
        HTML("<br/>"),

        uiOutput("prediction_r_notice"),

        plotOutput("prediction_r", height = "500px") %>%
          withSpinner(color = "#428bca")

      )
    )

  )
)


server <- function(input, output, session) {
  # general
  wind_info <- reactiveValues(
    predmap = NULL,
    coords = NULL,
    title_text = NULL,
    exit = NULL,
    predmap_r = NULL,
    title_text_r = NULL,
    exit_r = NULL
  )

  # Tab 2: multi-site northern prediction ------------------------------------------

  # create small map
  output$map_r <- renderLeaflet({
    isolate({
      base_map() %>%
        addMarkers(lng = loc_xy$long, lat = loc_xy$lat, label = as.character(localities[, 1]))
    })
  })

  # run the simulation
  observeEvent(input$run_r, {
    tz <- input$timezone_r
    yesterday <- today(tz) - 1
    geo <- data.frame(lon = 136.52561, lat = -16.12613)

    sunset_utc <- sunset_time(
      date = yesterday,
      tz = "UTC",
      geocode = geo
    )

    r_date <- format(sunset_utc, "%Y%m%d")

    sunset_selected <- sprintf("%02d", hour(round_date(sunset_time(
      date = yesterday,
      tz = tz,
      geocode = geo
    ), "hour")))
    sunset_utc <- sprintf("%02d", hour(round_date(sunset_utc, "hour")))

    night_l <- round(
      night_length(
        date = yesterday,
        tz = "UTC",
        geocode = geo
      ),
      0
    )

    # the same forecast is shared by every user on a given day
    file <- cache_file(r_date, sunset_utc, night_l)
    res <- read_cache(file)

    if (is.null(res)) {
      missing <- missing_forecasts(r_date, sunset_utc, as.numeric(night_l), FALSE)
      if (length(missing) > 0) {
        showNotification(
          paste0("The wind forecast needed for this simulation is not on the server yet (",
                 paste(missing, collapse = ", "),
                 "). It may still be downloading - please try again in a few minutes."),
          type = "error",
          duration = 20
        )
        return()
      }

      res <- run_safely(function() {
        simulate_with_progress(
          coords = lapply(seq_len(nrow(localities)), function(x) as.numeric(localities[x, 2:3])),
          nforecast = as.numeric(night_l),
          fdate = r_date,
          fhour = sunset_utc,
          backwards = FALSE
        )
      })
      if (is.null(res)) return()

      write_cache(file, res)
    }

    wind_info$predmap_r <- res$raster
    wind_info$exit_r <- res$info

    wind_info$title_text_r <-
      sprintf(
        "Forecast initiated at %s %s %s | Duration: %s hours",
        as.character(yesterday),
        as.character(sunset_selected),
        as.character(tz),
        as.character(night_l)
      )

  })

  output$prediction_r_notice <- renderUI({
    req(wind_info$predmap_r)
    exit_notice(wind_info$exit_r)
  })

  output$prediction_r <- renderPlot({
    req(wind_info$predmap_r)
    make_prediction_plot(wind_info$predmap_r, loc_xy, wind_info$title_text_r)
  })


  # Tab 1: custom simulation --------------------------------------------------------

  output$warning_text <- renderUI({
    req(input$timezone)
    end_time <- latest_start(input$timezone)
    h5(class = "warning-message",
       sprintf("Latest available starting time is %s*.", format(end_time, "%Y-%m-%d %H:%M:%S")))
  })

  output$forec_date <- renderUI({
    req(input$timezone)
    tz <- input$timezone
    today_tz <- lubridate::today(tzone = tz)

    # keep the date the user already chose, if it is still allowed
    keep <- isolate(input$forec_date)
    value <- if (!is.null(keep) && length(keep) == 1 && !is.na(keep) &&
                 keep >= today_tz - 7 && keep <= today_tz) keep else today_tz

    dateInput(
      "forec_date",
      label = "Date",
      value = value,
      min = today_tz - lubridate::days(7),
      max = today_tz
    )
  })

  output$duration_slider <- renderUI({
    req(input$direction)
    max_hours <- if (input$direction == "Forward") 24 else 96

    # keep the run time the user already chose, if it is still allowed
    keep <- isolate(input$nforecast)
    selected <- if (!is.null(keep) && length(keep) == 1 && keep %in% seq_len(max_hours)) keep else 12

    sliderTextInput(
      inputId = "nforecast",
      label = "Total run time (hours)",
      choices = seq(1, max_hours, 1),
      selected = selected,
      grid = TRUE
    )
  })

  output$forec_time <- renderUI({
    req(input$forec_date, input$timezone)
    time_choices <- time_choices_for(input$forec_date, input$timezone)

    if (length(time_choices) == 0) {
      return(helpText("No forecast start times are available yet for this date."))
    }

    keep <- isolate(input$forec_time)

    selectizeInput(
      inputId = "forec_time",
      label = "Time",
      choices = time_choices,
      options = list(dropdownParent = 'body', create = 0),
      selected = if (!is.null(keep) && keep %in% time_choices) keep else min(time_choices)
    )
  })

  selected_time_utc <- reactive({
    req(input$forec_date, input$forec_time, input$timezone)
    selected_time <-
      lubridate::ymd_hms(paste0(
        format(input$forec_date, "%Y-%m-%d"),
        " ",
        hour(hm(input$forec_time)),
        ":00:00"
      ), tz = input$timezone)
    lubridate::with_tz(selected_time, "UTC")
  })

  selected_localities <- reactiveValues(
    coordinates = matrix(
      c(145, -37.8, rep(NA_real_, 2 * MAX_POINTS - 2)),
      nrow = MAX_POINTS,
      ncol = 2,
      byrow = TRUE
    )
  )

  # number of locations currently selected
  n_selected <- reactive(sum(!is.na(selected_localities$coordinates[, 1])))

  # add the small map
  output$smap <- renderLeaflet({
    isolate({
      pts <- selected_localities$coordinates
      pts <- pts[!is.na(pts[, 1]), , drop = FALSE]
      base_map() %>%
        addMarkers(lng = pts[, 1], lat = pts[, 2])
    })
  })

  observeEvent(input$smap_click, {
    click <- input$smap_click
    lng <- ((click$lng + 180) %% 360) - 180   # the map repeats east-west; bring the click back to -180..180

    if (n_selected() >= MAX_POINTS) {
      showNotification("Maximum no. of localities reached", type = "error")
    } else if (!xy_in_aus(lng, click$lat)) {
      showNotification(
        "That location is outside the model area (dashed outline). Please click inside it.",
        type = "error"
      )
    } else {
      first_empty_row <- which(is.na(selected_localities$coordinates[, 1]))[1]
      selected_localities$coordinates[first_empty_row, 1] <- lng
      selected_localities$coordinates[first_empty_row, 2] <- click$lat
    }
  })

  observe({
    pts <- selected_localities$coordinates
    pts <- pts[!is.na(pts[, 1]), , drop = FALSE]
    leafletProxy("smap") %>%
      clearMarkers() %>%
      addMarkers(lng = pts[, 1], lat = pts[, 2])
  })

  # delete buttons and coordinate read-outs
  lapply(seq_len(MAX_POINTS), function(i) {
    observeEvent(input[[paste0("delete", i)]], {
      selected_localities$coordinates[i, ] <- NA_real_
    }, ignoreInit = TRUE)

    output[[paste0("coord", 2 * i - 1)]] <- renderText({
      if (!is.na(selected_localities$coordinates[i, 1])) {
        paste("Longitude:", round(selected_localities$coordinates[i, 1], 3))
      } else {
        ""
      }
    })

    output[[paste0("coord", 2 * i)]] <- renderText({
      if (!is.na(selected_localities$coordinates[i, 2])) {
        paste("Latitude:", round(selected_localities$coordinates[i, 2], 3))
      } else {
        ""
      }
    })
  })

  # run the simulation
  observeEvent(input$run, {
    coords <- selected_localities$coordinates
    coords <- coords[stats::complete.cases(coords), , drop = FALSE]

    if (nrow(coords) == 0) {
      showNotification("Please select at least one location on the map first.", type = "warning")
      return()
    }
    if (is.null(input$forec_date) || is.null(input$forec_time)) {
      showNotification("Please select a start date and time first.", type = "warning")
      return()
    }

    utc <- selected_time_utc()
    fdate <- format(as.Date(utc), "%Y%m%d")
    fhour <- format(utc, "%H")
    nforecast <- as.numeric(input$nforecast)
    backwards <- input$direction == "Backward"

    missing <- missing_forecasts(fdate, fhour, nforecast, backwards)
    if (length(missing) > 0) {
      showNotification(
        paste0("The wind forecast needed for this simulation is not on the server yet (",
               paste(missing, collapse = ", "),
               "). It may still be downloading - please try again in a few minutes."),
        type = "error",
        duration = 20
      )
      return()
    }

    res <- run_safely(function() {
      simulate_with_progress(
        coords = lapply(seq_len(nrow(coords)), function(x) coords[x, ]),
        nforecast = nforecast,
        fdate = fdate,
        fhour = fhour,
        backwards = backwards
      )
    })
    if (is.null(res)) return()   # the previous map (if any) stays on screen

    wind_info$predmap <- res$raster
    wind_info$exit <- res$info
    # the locations this map was made for (not whatever is selected later)
    wind_info$coords <- data.frame(long = coords[, 1], lat = coords[, 2])

    if (input$direction == "Backward")
      wind_info$title_text <-
      sprintf(
        "Backwards projection initiated at %s %s %s | Duration: %s hours",
        input$forec_date,
        input$forec_time,
        input$timezone,
        input$nforecast
      )
    else
      wind_info$title_text <-
      sprintf(
        "Forecast initiated at %s %s %s | Duration: %s hours",
        input$forec_date,
        input$forec_time,
        input$timezone,
        input$nforecast
      )

  })

  output$prediction_notice <- renderUI({
    req(wind_info$predmap)
    exit_notice(wind_info$exit)
  })

  output$prediction <- renderPlot({
    req(wind_info$predmap, wind_info$coords)
    make_prediction_plot(wind_info$predmap, wind_info$coords, wind_info$title_text)
  })

}


shinyApp(ui, server)
