library(shiny)
library(leaflet)
library(mongolite)
library(ggmap)
library(dplyr)
library(lubridate)
library(scales)
library(forcats)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visualisation des données Velib"),
  
  # Sidebar  
  sidebarLayout(
    sidebarPanel(
      textInput("adresse", "Adresse à saisir",
                value = "8 boulevard Saint-Michel, Paris"),
      sliderInput("distance", "Distance",value = 200, min = 0, max=500, step = 10),
      dateRangeInput("plage","Plage de dates")
    ),
    # Show plot
    mainPanel(
       leafletOutput("geocode"), 
       plotOutput("graphes_semaine_station_selectionnee")
    )
  )
))
