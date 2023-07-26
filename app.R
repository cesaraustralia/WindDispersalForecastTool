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


# read the wind functions
source("R/wind_scr.R")
source("R/wind_ca_model.R")

# gdal option to allow terra read raster files with http links
# without this terra can't read files with http from S3 bucket API
# setGDALconfig("GDAL_HTTP_UNSAFESSL", "YES")

# read Australian border
border <- st_read("SpatialData/borders.gpkg", quiet = TRUE) %>%
  sf::st_cast(to = "MULTILINESTRING")
# add the bbox of the raster as the allowed region
bound <- st_read("SpatialData/boundary.gpkg", quiet = TRUE)

# returns true or false
xy_in_aus <- function(long, lat) {
  data.frame(x = long, y = lat) %>%
    sf::st_as_sf(coords = 1:2, crs = 4326) %>%
    sf::st_transform(crs = sf::st_crs(bound)) %>%
    sf::st_intersection(sf::st_geometry(bound)) %>%
    nrow() != 0
}

# load localities for rapid prediction
localities <- as.data.frame(read_csv("localities.csv"))

# base url for Cesar S3 API for wind data
# apiurl <- "https://gwtiioyhfg.execute-api.ap-southeast-2.amazonaws.com/api/cesar-storage/wind-data"

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

ui <- shinyUI(
  navbarPage(
    "Wind Forecast Tool v1.0.0",
    selected = "Select location for rapid simulation",
    theme = shinytheme("yeti"),

    # Panel 1 -----------------------------------------------------------------

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

        fluidRow(
          column(4, textOutput("coord1")),
          column(4, textOutput("coord2")),
          column(4, actionButton("delete1", "Delete"))
        ),
        fluidRow(
          column(4, textOutput("coord3")),
          column(4, textOutput("coord4")),
          column(4, actionButton("delete2", "Delete"))
        ),
        fluidRow(
          column(4, textOutput("coord5")),
          column(4, textOutput("coord6")),
          column(4, actionButton("delete3", "Delete"))
        ),
        fluidRow(
          column(4, textOutput("coord7")),
          column(4, textOutput("coord8")),
          column(4, actionButton("delete4", "Delete"))
        ),
        fluidRow(
          column(4, textOutput("coord9")),
          column(4, textOutput("coord10")),
          column(4, actionButton("delete5", "Delete"))
        ),

        # add a leaflet map
        leafletOutput("smap", height = 250),

        h6(
          "* If your simulation fails around this time, try again in a few minutes - the forecasts may not have finished downloading yet."
        )

      ),
      column(
        width = 8,

        HTML("<br/>"),
        actionButton("run", "Run forecast"),
        HTML("<br/>"),

        plotOutput("prediction", height = "500px") %>%
          withSpinner(color = "#428bca")

      )
    )
    ,

    # Panel 2 -----------------------------------------------------------------

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
          "* Simulation should take approximately eight minutes"
        )

      ),
      column(
        width = 8,

        HTML("<br/>"),
        actionButton("run_r", "Run forecast"),
        HTML("<br/>"),

        plotOutput("prediction_r", height = "500px") %>%
          withSpinner(color = "#428bca")

      )
    )

  )
)


server <- function(input, output, session) {
  # general
  wind_info <- reactiveValues()
  wind_info$predmap <- NULL
  wind_info$predmap_r <- NULL
  wind_info$selected_time <- NULL
  wind_info$selected_time_utc <- NULL

  # Panel 1: rapid prediction

  # create small map
  output$map_r <- renderLeaflet({
    isolate({
      leaflet(bound) %>%
        setView(lng = 135.51,
                lat = -25.98,
                zoom = 3) %>%
        addTiles() %>%
        addPolygons(fillOpacity = 0) %>%
        addMarkers(lng = as.numeric(localities[,2]), lat = as.numeric(localities[,3]), label = as.character(localities[,1]))
    })
  })

  # run the simulation
  observeEvent(input$run_r, {
    coords_r <- lapply(1:nrow(localities), function(x) as.numeric(localities[x, 2:3]))

    sunset_utc <- sunset_time(
      date = today(input$timezone_r) - 1,
      tz = "UTC",
      geocode = data.frame(lon = 136.52561, lat = -16.12613)
    )

    r_date <- format(sunset_utc, "%Y%m%d")

    sunset_selected <- sprintf("%02d", hour(round_date(sunset_time(
      date = today(input$timezone_r) - 1,
      tz = input$timezone_r,
      geocode = data.frame(lon = 136.52561, lat = -16.12613)
    ), "hour")))
    sunset_utc <- sprintf("%02d", hour(round_date(sunset_utc, "hour")))

    night_l <- round(
      night_length(
        date = today(input$timezone_r) - 1,
        tz = "UTC",
        geocode = data.frame(lon = 136.52561, lat = -16.12613)
      ),
      0
    )

    wind_info$predmap_r <- {

      # Create a Progress object
      progress <- shiny::Progress$new(max = 10 * length(coords_r) * night_l)

      progress$set(message = "Simulating", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
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

      wind_sim(
        data_path = "wind-data",
        coords = coords_r,
        nforecast = night_l,
        nsim = 10,
        fdate = r_date,
        fhour =  sunset_utc,
        atm_level = "950mb",
        updateProgress = updateProgress
      )
    }

    wind_info$title_text_r <-
      sprintf(
        "Forecast initiated at %s %s %s | Duration: %s hours",
        as.character(today(input$timezone_r) - 1),
        as.character(sunset_selected),
        as.character(input$timezone_r),
        as.character(night_l)
      )

  })

  output$prediction_r <- renderPlot({
    # plot(generate_plot())

    req(input$run_r)

    # make plot only react to the run button
    isolate({
      xt <- c(
        floor(min(as.numeric(localities[,2]), na.rm = T)) - 18,
        floor(max(as.numeric(localities[,2]), na.rm = T)) + 18,
        floor(min(as.numeric(localities[,3]), na.rm = T)) - 15,
        floor(max(as.numeric(localities[,3]), na.rm = T)) + 15
      )

      pt <-
        data.frame(long = as.numeric(localities[,2]), lat = as.numeric(localities[,3]))

      if (!is.null(wind_info$predmap_r)) {
        r_crop <- terra::crop(wind_info$predmap_r, xt)
        r_crop <-
          r_crop / global(r_crop, max, na.rm = TRUE)[1, 1] * 100

        gplot(r_crop, maxpixels = 500000) +
          geom_tile(aes(fill = value), alpha = 0.8) +
          viridis::scale_fill_viridis(option = "D",
                                      direction = -1,
                                      na.value = NA) +
          geom_point(
            data = pt,
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
          ) +
          coord_sf(crs = 4326) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14),
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
               fill = "Frequency") +
          ggtitle(wind_info$title_text_r)
      }

    })

  })


  # Panel 2: custom simulation
  output$warning_text <- renderUI({
    tz <- input$timezone

    end_time <- lubridate::with_tz(lubridate::ymd_hms(paste0(format(lubridate::today(), "%Y-%m-%d"), " ", "08:00:00"), tz = "Australia/Brisbane"), tz)
    h5(class = "warning-message", sprintf("Latest available starting time is %s*.", end_time))
  })

  output$forec_date <- renderUI({
    tz <- input$timezone

    dateInput(
      "forec_date",
      label = "Date",
      value = lubridate::today(tzone = tz),
      min = lubridate::today(tzone = tz) - lubridate::days(7),
      max = lubridate::today(tzone = tz)
    )
  })

  observeEvent(input$direction, {
    direction <- input$direction

    if(direction == "Forward"){
      output$duration_slider <- renderUI({
        sliderTextInput(
          inputId = "nforecast",
          label = "Total run time (hours)",
          choices = seq(1, 24, 1),
          selected = 12,
          grid = TRUE
        )
      })
    }

    if(direction == "Backward"){
      output$duration_slider <- renderUI({
        sliderTextInput(
          inputId = "nforecast",
          label = "Total run time (hours)",
          choices = seq(1, 96, 1),
          selected = 12,
          grid = TRUE
        )
      })
    }
  })

  observeEvent(c(input$forec_date, input$timezone), {
    tz <- input$timezone

    # original choices
    original_choices <-
      lubridate::with_tz(lubridate::ymd_hms(paste0(
        format(input$forec_date, "%Y-%m-%d"),
        " ",
        sprintf("%s:00:00", seq(0, 23))
      ),
      tz = "Australia/Melbourne"),
      tz = tz)

    original_choices <- lubridate::ymd_hms(paste0(
      format(input$forec_date, "%Y-%m-%d"),
      " ",
      format(original_choices, "%H:%M:%S")
    ),
    tz = tz)

    # filter original choices by selected timezone
    filtered_choices <-
      sort(original_choices[
        original_choices <= with_tz(lubridate::ymd_hms(paste0(format(lubridate::today(), "%Y-%m-%d"), " ", "08:00:00"), tz = "Australia/Brisbane"), tz)
      ])

    # Adjust time choices based on selected date
    time_choices <- format(filtered_choices, "%H:%M")

    output$forec_time <- renderUI({
      selectizeInput(
        inputId = "forec_time",
        label = "Time",
        choices = time_choices,
        options = list(dropdownParent = 'body', create = 0),
        selected = min(time_choices)
      )
    })
  })

  observeEvent(c(input$forec_date, input$forec_time), {
    tz <- input$timezone
    selected_time <-
      lubridate::ymd_hms(paste0(
        format(input$forec_date, "%Y-%m-%d"),
        " ",
        hour(hm(input$forec_time)),
        ":00:00"
      ), tz = tz)
    wind_info$selected_time_utc <-
      lubridate::with_tz(selected_time, "UTC")
  })

  selected_localities <- reactiveValues(
    coordinates = matrix(
      c(145, -37.8, NA, NA, NA, NA, NA, NA, NA, NA),
      nrow = 5,
      ncol = 2,
      byrow = TRUE
    ),
    count = 1
  )

  # add the small map
  output$smap <- renderLeaflet({
    isolate({
      leaflet(bound) %>%
        setView(lng = 135.51,
                lat = -25.98,
                zoom = 3) %>%
        addTiles() %>%
        addPolygons(fillOpacity = 0) %>%
        addMarkers(lng = na.omit(selected_localities$coordinates[, 1]), lat = na.omit(selected_localities$coordinates[, 2]))
    })
  })

  observeEvent(input$smap_click, {
    if (selected_localities$count < 5) {
      selected_localities$count <- selected_localities$count + 1
      empty_rows <- which(is.na(selected_localities$coordinates[, 1]))
      first_empty_row <- empty_rows[1]
      selected_localities$coordinates[first_empty_row, 1] <- input$smap_click$lng
      selected_localities$coordinates[first_empty_row, 2] <- input$smap_click$lat
      updateNumericInput(session, paste0("x", first_empty_row), value = round(input$smap_click$lng, 3))
      updateNumericInput(session, paste0("y", first_empty_row), value = round(input$smap_click$lat, 3))
    } else {
      showNotification("Maximum no. of localities reached", type = "error")
    }
  })

  observe({
    leafletProxy("smap") %>%
      clearMarkers() %>%
      addMarkers(lng = na.omit(selected_localities$coordinates[, 1]), lat = na.omit(selected_localities$coordinates[, 2]))
  })

  # delete buttons
  observeEvent(input$delete1, {
    selected_localities$count <- selected_localities$count - 1
    selected_localities$coordinates[1, ] <- NA
    updateNumericInput(session, "x1", value = NA)
    updateNumericInput(session, "y1", value = NA)
    leafletProxy("smap") %>%
      clearMarkers()
  })

  observeEvent(input$delete2, {
    selected_localities$count <- selected_localities$count - 1
    selected_localities$coordinates[2, ] <- NA
    updateNumericInput(session, "x2", value = NA)
    updateNumericInput(session, "y2", value = NA)
    leafletProxy("smap") %>%
      clearMarkers()
  })

  observeEvent(input$delete3, {
    selected_localities$count <- selected_localities$count - 1
    selected_localities$coordinates[3, ] <- NA
    updateNumericInput(session, "x3", value = NA)
    updateNumericInput(session, "y3", value = NA)
    leafletProxy("smap") %>%
      clearMarkers()
  })

  observeEvent(input$delete4, {
    selected_localities$count <- selected_localities$count - 1
    selected_localities$coordinates[4, ] <- NA
    updateNumericInput(session, "x4", value = NA)
    updateNumericInput(session, "y4", value = NA)
    leafletProxy("smap") %>%
      clearMarkers()
  })

  observeEvent(input$delete5, {
    selected_localities$count <- selected_localities$count - 1
    selected_localities$coordinates[5, ] <- NA
    updateNumericInput(session, "x5", value = NA)
    updateNumericInput(session, "y5", value = NA)
    leafletProxy("smap") %>%
      clearMarkers()
  })

  output$coord1 <- renderText({
    if (!is.na(selected_localities$coordinates[1, 1])) {
      paste("Longitude:", round(selected_localities$coordinates[1, 1],3))
    } else {
      ""
    }
  })

  output$coord2 <- renderText({
    if (!is.na(selected_localities$coordinates[1, 2])) {
      paste("Latitude:", round(selected_localities$coordinates[1, 2],3))
    } else {
      ""
    }
  })

  output$coord3 <- renderText({
    if (!is.na(selected_localities$coordinates[2, 1])) {
      paste("Longitude:", round(selected_localities$coordinates[2, 1],3))
    } else {
      ""
    }
  })

  output$coord4 <- renderText({
    if (!is.na(selected_localities$coordinates[2, 2])) {
      paste("Latitude:", round(selected_localities$coordinates[2, 2],3))
    } else {
      ""
    }
  })

  output$coord5 <- renderText({
    if (!is.na(selected_localities$coordinates[3, 1])) {
      paste("Longitude:", round(selected_localities$coordinates[3, 1],3))
    } else {
      ""
    }
  })

  output$coord6 <- renderText({
    if (!is.na(selected_localities$coordinates[3, 2])) {
      paste("Latitude:", round(selected_localities$coordinates[3, 2],3))
    } else {
      ""
    }
  })

  output$coord7 <- renderText({
    if (!is.na(selected_localities$coordinates[4, 1])) {
      paste("Longitude:", round(selected_localities$coordinates[4, 1],3))
    } else {
      ""
    }
  })

  output$coord8 <- renderText({
    if (!is.na(selected_localities$coordinates[4, 2])) {
      paste("Latitude:", round(selected_localities$coordinates[4, 2],3))
    } else {
      ""
    }
  })

  output$coord9 <- renderText({
    if (!is.na(selected_localities$coordinates[5, 1])) {
      paste("Longitude:", round(selected_localities$coordinates[5, 1],3))
    } else {
      ""
    }
  })

  output$coord10 <- renderText({
    if (!is.na(selected_localities$coordinates[5, 2])) {
      paste("Latitude:", round(selected_localities$coordinates[5, 2],3))
    } else {
      ""
    }
  })

  # run the simulation
  observeEvent(input$run, {
    coords <- na.omit(selected_localities$coordinates)

    wind_info$predmap <- {
      # Create a Progress object
      progress <- shiny::Progress$new(max = 10 * input$nforecast * dim(coords)[1])

      progress$set(message = "Simulating", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
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

      wind_sim(
        data_path = "wind-data",
        coords = lapply(1:dim(coords)[1], function(x) coords[x,]),
        nforecast = input$nforecast,
        nsim = 10,
        fdate = format(as.Date(wind_info$selected_time_utc), "%Y%m%d"),
        fhour =  format(wind_info$selected_time_utc, "%H"),
        atm_level = "950mb",
        backwards = ifelse(input$direction == "Forward", F, T),
        updateProgress = updateProgress
      )
    }

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


  output$prediction <- renderPlot({
    # plot(generate_plot())

    req(input$run)

    # make plot only react to the run button
    isolate({
      xt <- c(
        floor(min(selected_localities$coordinates[,1], na.rm = T)) - 18,
        floor(max(selected_localities$coordinates[,1], na.rm = T)) + 18,
        floor(min(selected_localities$coordinates[,2], na.rm = T)) - 15,
        floor(max(selected_localities$coordinates[,2], na.rm = T)) + 15
      )

      pt <-
        data.frame(long = na.omit(selected_localities$coordinates[,1]), lat = na.omit(selected_localities$coordinates[,2]))

      if (!is.null(wind_info$predmap)) {
        r_crop <- terra::crop(wind_info$predmap, xt)
        r_crop <-
          r_crop / global(r_crop, max, na.rm = TRUE)[1, 1] * 100

        gplot(r_crop, maxpixels = 500000) +
          geom_tile(aes(fill = value), alpha = 0.8) +
          viridis::scale_fill_viridis(option = "D",
                                      direction = -1,
                                      na.value = NA) +
          geom_point(
            data = pt,
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
          ) +
          coord_sf(crs = 4326) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14),
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
               fill = "Frequency") +
          ggtitle(wind_info$title_text)
      }

    })

  })

}


shinyApp(ui, server)
