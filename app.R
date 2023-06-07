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
    "Wind Forecast Tool v0.4.1",
    # selected = "Simulation",
    theme = shinytheme("yeti"),

    # # Panel 1 -----------------------------------------------------------------
    # tabPanel(
    #   "Simulation",

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

        sliderTextInput(
          inputId = "nforecast",
          label = "Total run time (hours)",
          choices = seq(1, 24, 1),
          selected = 12,
          grid = TRUE
        ),

        shiny::splitLayout(
          numericInput(
            inputId = "x",
            label = "Longitude",
            value = 145.0
          ),
          numericInput(
            inputId = "y",
            label = "Latitude",
            value = -37.8
          )
        ),

        # HTML("<br/>"),
        # show selected region
        span(textOutput("checklatlong"), style = "color:red"),
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
  #   ,
  #
  #   # Panel 2 -----------------------------------------------------------------
  #   tabPanel("About",
  #
  #            includeMarkdown("README.md"))
  #
  # )
)


server <- function(input, output, session) {
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

  observeEvent(c(input$forec_date, input$timezone), {
    tz <- input$timezone

    # original choices in UTC
    original_choices <-
      lubridate::with_tz(lubridate::ymd_hms(paste0(
        format(input$forec_date, "%Y-%m-%d"),
        " ",
        sprintf("%s:00:00", seq(0, 23))
      ),
      tz = "Australia/Melbourne"),
      tz = tz)

    # convert original choices to selected timezone
    filtered_choices <-
      original_choices[
    original_choices <= with_tz(lubridate::ymd_hms(paste0(format(lubridate::today(), "%Y-%m-%d"), " ", "08:00:00"), tz = "Australia/Brisbane"), tz)
      ]

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


  wind_info <- reactiveValues()
  wind_info$predmap <- NULL
  wind_info$selected_time <- NULL
  wind_info$selected_time_utc <- NULL
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

  # set default values for click
  input_coords <- reactiveValues()
  input_coords$long <- 145.0
  input_coords$lat <- -37.8
  observe({
    if (!is.null(input$smap_click)) {
      if (xy_in_aus(input$smap_click$lng, input$smap_click$lat)) {
        input_coords$long <- round(input$smap_click$lng, 3)
        input_coords$lat <- round(input$smap_click$lat, 3)
      }
    }
  })

  # add the small map
  output$smap <- renderLeaflet({
    isolate({
      leaflet(bound) %>%
        setView(lng = 135.51,
                lat = -25.98,
                zoom = 3) %>%
        addTiles() %>%
        addPolygons(fillOpacity = 0) %>%
        addMarkers(lng = input_coords$long, lat = input_coords$lat)
    })
  })
  # update the click and marker without changing zoom and reloading
  observeEvent(input$smap_click, {
    leafletProxy("smap") %>%
      clearMarkers() %>%
      addMarkers(lng = input$smap_click$lng,
                 lat = input$smap_click$lat)
  })
  # update the map if x and y changes
  listen_to_xy <- reactive({
    list(input$x, input$y)
  })
  # update the markers
  observeEvent(listen_to_xy(), {
    input_coords$long <- round(input$x, 3)
    input_coords$lat <- round(input$y, 3)
  })

  # update the inputs based on click
  observe({
    updateNumericInput(session, "x",
                       value = input_coords$long)
    updateNumericInput(session, "y",
                       value = input_coords$lat)
  })


  # show coordinates with click
  output$checklatlong <- renderText({
    if (!is.null(input$smap_click)) {
      if (!xy_in_aus(input$smap_click$lng, input$smap_click$lat)) {
        "Selected location is not in the current forecasting range!"
      } else {
        NULL
      }
    }
  })

  # run the simulation
  observeEvent(input$run, {
    wind_info$predmap <- {
      # Create a Progress object
      progress <- shiny::Progress$new(max = 10 * input$nforecast)
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
      coords = list(c(input_coords$long, input_coords$lat)),
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
          "Backwards projection initiated at %s %s %s | Duration: %s hours\nLongitude: %s  Latitude: %s",
          input$forec_date,
          input$forec_time,
          input$timezone,
          input$nforecast,
          input_coords$long,
          input_coords$lat
        )
    else
      wind_info$title_text <-
        sprintf(
          "Forecast initiated at %s %s %s | Duration: %s hours\nLongitude: %s  Latitude: %s",
          input$forec_date,
          input$forec_time,
          input$timezone,
          input$nforecast,
          input_coords$long,
          input_coords$lat
        )

  })


  output$prediction <- renderPlot({
    # plot(generate_plot())

    req(input$run)

    # make plot only react to the run button
    isolate({
      xt <- c(
        floor(input_coords$long) - 18,
        floor(input_coords$long) + 18,
        floor(input_coords$lat) - 15,
        floor(input_coords$lat) + 15
      )

      pt <-
        data.frame(long = input_coords$long, lat = input_coords$lat)

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
