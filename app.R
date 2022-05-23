library(tidyverse)
library(shiny)
library(terra)
library(sf)


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
