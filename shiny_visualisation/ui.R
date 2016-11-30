
library(shiny)

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
       leafletOutput("geocode")
    )
  )
))
