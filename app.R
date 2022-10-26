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

ui <- shinyUI(
  navbarPage("Wind Forecast Tool v0.1",
             selected = "Simulation",
             # theme = shinytheme("yeti"),

             # Panel 1 -----------------------------------------------------------------
             tabPanel(
               "Simulation",

               column(
                 width = 4,

                 h5("Select meteorological forecast cycle"),
                 shiny::splitLayout(
                   # dateInput("forec_date",
                   #           label = NULL, #"Select meteorological forecast date",
                   #           value = lubridate::today() - 1,
                   #           min = "2022-05-22",
                   #           max = lubridate::today() - 1,
                   #           width = "100%"
                   # ),
                   dateInput("forec_date",
                             label = NULL, #"Select meteorological forecast date",
                             value = "2022-05-30",
                             min = "2022-05-22",
                             max = "2022-05-30",
                             width = "100%"
                   ),

                   selectizeInput(inputId = "forec_time",
                                  label = NULL, #"Select forecast start time",
                                  choices = c("00", "06", "12", "18"),
                                  options = list(dropdownParent = 'body',
                                                 # render = I("function() {onItemAdd('value', '$item'); }"),
                                                 create = 0),
                                  selected = "18")
                 ),


                 sliderTextInput(inputId = "nforecast",
                                 label = "Total run time (hours)",
                                 choices = seq(6, 48, 6),
                                 selected = 24,
                                 grid = TRUE
                 ),


                 sliderTextInput(inputId = "nsim",
                                 label = "Number of simulations",
                                 choices = seq(1, 30, 1),
                                 selected = 5,
                                 grid = TRUE
                 ),

                 selectInput(inputId = "level",
                             label = "Select atmospheric level",
                             choices = c("850mb"),
                             selected = "850mb",
                             width = "100%"
                 ),


                 shiny::splitLayout(
                   numericInput(inputId = "x",
                                label = "Longitude",
                                value = 145.0),
                   numericInput(inputId = "y",
                                label = "Latitude",
                                value = -37.8)
                 ),

                 # HTML("<br/>"),
                 # show selected region
                 span(textOutput("checklatlong"), style = "color:red"),
                 # add a leaflet map
                 leafletOutput("smap", height = 250),

               ),
               column(
                 width = 8,

                 HTML("<br/>"),
                 actionButton("run", "Run forecast"),
                 HTML("<br/>"),

                 plotOutput("prediction", height = "500px") %>%
                   withSpinner(color = "#428bca")# "#0dc5c1"

               )
             )

  )
)


server <- function(input, output, session){

  wind_info <- reactiveValues()
  wind_info$wind_path <- NULL
  wind_info$predmap <- NULL
  # get the wind data path using date and start time
  # /20220524/00/gfs_ugrd_850mb_20220524_t00z_f000
  observe({
    # wind_info$wind_path <- sprintf("%s/%s/%s",
    #                                apiurl,
    #                                format(as.Date(input$forec_date), "%Y%m%d"),
    #                                input$forec_time)
    wind_info$wind_path <- sprintf("wind-data/%s/%s",
                                   format(as.Date(input$forec_date), "%Y%m%d"),
                                   input$forec_time)
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
        setView(lng = 135.51, lat = -25.98, zoom = 3) %>%
        addTiles() %>%
        addPolygons(fillOpacity = 0) %>%
        addMarkers(lng = input_coords$long, lat = input_coords$lat)
    })
  })
  # update the click and marker without changing zoom and reloading
  observeEvent(input$smap_click, {
    leafletProxy("smap") %>%
      clearMarkers() %>%
      addMarkers(lng = input$smap_click$lng, lat = input$smap_click$lat)
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

    wind_info$predmap <- wind_sim(data_path = wind_info$wind_path,
                                  long = input_coords$long,
                                  lat = input_coords$lat,
                                  nforecast = input$nforecast,
                                  nsim = input$nsim,
                                  # fdate = format(as.Date(input$forec_date), "%Y%m%d"),
                                  # fhour = input$forec_time,
                                  atm_level = input$level)

  })


  output$prediction <- renderPlot({
    # plot(generate_plot())

    req(input$run)

    # make plot only react to the run button
    isolate({

      xt <- c(floor(input_coords$long) - 18,
              floor(input_coords$long) + 18,
              floor(input_coords$lat) - 15,
              floor(input_coords$lat) + 15)

      pt <- data.frame(long = input_coords$long, lat = input_coords$lat)

      if(!is.null(wind_info$predmap)){

        r_crop <- terra::crop(wind_info$predmap, xt)
        r_crop <- r_crop / global(r_crop, max, na.rm = TRUE)[1,1] * 100

        gplot(r_crop, maxpixels = 500000) +
          geom_tile(aes(fill = value), alpha = 0.8) +
          viridis::scale_fill_viridis(option = "D", direction = -1, na.value = NA) +
          geom_point(data = pt, aes(x = long, y = lat), inherit.aes = FALSE,
                     col = "red", alpha = 0.8, shape = "\u2605", size = 7) +
          geom_sf(data = st_crop(border, ext(xt)), inherit.aes = FALSE, fill = NA) +
          coord_sf(crs = 4326) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 13),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
          ) +
          labs(x = "Longitude", y = "Latitude", fill = "Frequency") +
          ggtitle(
            sprintf("Forecast initiated at %s %s:00 UTC | Duration: %s hours\nLongitude: %s  Latitude: %s",
                    input$forec_date, input$forec_time, input$nforecast,
                    input_coords$long, input_coords$lat)
          )
      }

      # save plot in home
      # ggsave(filename = "~/phenology.png", plot = generate_plot(), device = 'png')

    })

  })

}


shinyApp(ui, server)
