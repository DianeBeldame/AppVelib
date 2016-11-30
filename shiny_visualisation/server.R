library(shiny)
library(ggmap)
library(leaflet)
library(mongolite)

m <- mongo(collection = "velib",  url = paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@thinkr.fr/velib"),
           verbose = TRUE, options = ssl_options())

shinyServer(function(input, output) {
  output$geocode <- renderLeaflet({
    # geocode address
    coordinates <- reactive({as.data.frame(geocode(input$adresse))})
    # query and results in reactive
    x<-paste0('{ "loc" : { "$near" : { "$geometry" :  { "type" : "Point" , "coordinates" : [ ',coordinates()$lon, ' , ', coordinates()$lat,'] } ,  "$maxDistance" : ',input$distance,'}}}')
    res <- m$find(x)
    # print(coordinates())
    lamap <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      #  addMarkers(data = my.coordinates,lng=my.coordinates$lon, lat=my.coordinates$lat)
      addMarkers(data = res, lng=res$position$lng, lat=res
                 $position$lat)
  })
})

