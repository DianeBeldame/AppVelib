library(shiny)
library(ggmap)
library(leaflet)
library(mongolite)
library(dplyr)
library(ggmap)
library(leaflet)
library(mongolite)
library(dplyr)
library(lubridate)
library(scales)
library(forcats)

m <- mongo(collection = "velib",  url = paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@thinkr.fr/velib"),
           verbose = TRUE, options = ssl_options())

shinyServer(function(input, output) {
  
  liste_stations_proches <- reactive({coordinates <- reactive({as.data.frame(geocode(input$adresse))})
  # query and results in reactive
  x<-paste0('{ "loc" : { "$near" : { "$geometry" :  { "type" : "Point" , "coordinates" : [ ',coordinates()$lon, ' , ', coordinates()$lat,'] } ,  "$maxDistance" : ',input$distance,'}}}')
  m$find(x)
  })
  
  output$geocode <- renderLeaflet({
    res <- liste_stations_proches()
    lamap <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      #  addMarkers(data = my.coordinates,lng=my.coordinates$lon, lat=my.coordinates$lat)
      addMarkers(data = res, lng=res$position$lng, lat=res$position$lat,layerId = res$number)
  })
  
  get_data_station_cliquee <- reactive({
    
    res <- liste_stations_proches()
    # res$serie[input$geocode_marker_click$id]
    res$serie[[which(res$number == input$geocode_marker_click$id)]]%>% 
      mutate(last_update_parsed = ymd_hms(last_update_parsed)) %>%
      select(last_update_parsed, available_bikes) %>% 
    filter(last_update_parsed > input$plage[1],last_update_parsed < input$plage[2]) %>% # et ajout de variables utiles au plot
    mutate(heure = as.POSIXct(sprintf('%s %s:%s', Sys.Date(), 
                                      format(last_update_parsed, '%H:%M'), '00')),
           jour = fct_relevel(format(last_update_parsed,"%A"), c("lundi", "mardi", "mercredi", "jeudi", "vendredi","samedi")), 
           date = format(last_update_parsed, "%d%m"))
  })

output$graphes_semaine_station_selectionnee <- renderPlot({
  ggplot(data = get_data_station_cliquee(), aes(x=heure,
                                   y = available_bikes,
                                   group = date,
                                   col= jour))+ 
    geom_line()+
    facet_grid(jour~.)+
    scale_x_datetime(breaks=date_breaks('1 hour'), 
                     labels=date_format('%H:%M'))+ 
    theme(legend.position = "bottom")
})
})



