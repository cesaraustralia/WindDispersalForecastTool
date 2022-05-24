library(tidyverse)
library(shiny)
library(terra)
library(sf)

# read the wind functions
source("R/wind_scr.R")

ui <- shinyUI(
  navbarPage("Wind Forecasting",
             selected = "Forecasing",
             theme = "button.css",

             # Panel 1 -----------------------------------------------------------------
             tabPanel(
               "Forecasing",



             )

  )
)


server <- function(input, output){


}


shinyApp(ui, server)
