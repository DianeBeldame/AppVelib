# 1ere version d'une application VelibQuatuor
library(shiny)
library(leaflet)
library(ggmap)
library(mongolite)
library(ggplot2)

GetStationList <- function(m,addresse){
  # coordinates <- geocode(addresse)
  # x           <- paste0('{ "loc" : {
  #                           "$near" : {
  #                             "$geometry" :  {
  #                                   "type" : "Point" ,
  #                                   "coordinates" : [ ',coordinates$lon, ' , ', coordinates$lat,'] } ,
  #                                   "$maxDistance" : ',200,'
  #                             }
  #                           }
  #                       }')
  # output             <- unique(m$find(x))
  output             <- My.Single.Query.WithoutSerie(m,address    = addresse)
  output$proba <- runif(nrow(output))
  return(output)
}

ComputeColor <- function(proba){
  R   <- (1-proba)
  G   <- proba
  R[is.na(proba)] <- 0
  G[is.na(proba)] <- 0
  COL <- rgb(R, G, 0)
  print(COL)
  return(COL)
}

# On initialise la connexion à la base mongo et à la collection velibW
m      <- mongo(collection = "velibW", url = paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@thinkr.fr/velib"),verbose = TRUE, db = "velib")

# on construit l'object ui qui décrit l'interface web
ui <- navbarPage(title = "VelibQuatuor App",
                 tabPanel(title = "Main Application",
                          fluidRow(
                            column(4,
                                   textInput(inputId    = "adresse_depart", label = "Adresse de départ",value = "8 boulevard saint michel, Paris"),
                                   actionButton(inputId = "update_depart",  label = "refresh"),
                                   textInput(inputId    = "adresse_arrivee",label = "Adresse d'arrivée", value = "4, boulevard magenta, Paris"),
                                   actionButton(inputId = "update_arrivee", label = "refresh")
                                   ),
                            column(4,
                                   leafletOutput(outputId = "geocode_depart")
                                   ),
                            column(4,
                                   leafletOutput(outputId = "geocode_arrivee")
                                   )
                          ),
                          fluidRow(
                            column(4,
                                   wellPanel(title = "Date & heure du jour pour une simulation",
                                    dateInput(inputId = "date_du_jour",label = "Date du jour"),
                                    numericInput(inputId = "heure_du_jour",label = "heure du jour",min = 0,max = 23,step = 1,value = 12)
                                    ),
                                   wellPanel(title = "Prevision",
                                    numericInput(inputId = "à heure +",label = "prévision à : ",min = 1,max = 24,step = 1,value = 3)
                                   ),
                                   actionButton(inputId = "compute", label = "lance le calcule")
                                   ),
                            column(4,
                                   plotOutput(outputId = "plotStationDepart01"),
                                   plotOutput(outputId = "plotModelDepart")
                                  ),
                            column(4,
                                   plotOutput(outputId = "plotStationArrivee01"),
                                   plotOutput(outputId = "plotModelArrivee")
                                  )
                          )
                 ),
                 navbarMenu(title = "Data Explore",
                            tabPanel(title = "One Station One Week Day"
                            ),
                            tabPanel(title = "Chi Squared data"
                            )
                 )
         )




server <- function(input, output) {
  
  # on source le code FonctionUtiles2.R
  source("../FonctionsUtiles2.R", local = TRUE)
  
  # on effectue les requêtes pour l'affichage des stations
  rv <- reactiveValues()
    rv$liste_stations_proches_depart  <- GetStationList(m,"8 boulevard saint michel, Paris")
    rv$liste_stations_proches_arrivee <- GetStationList(m,"4, boulevard magenta, Paris")
    rv$donnees_depart                 <- NULL
    rv$donnees_arrivee                <- NULL
    rv$resultat_modelisation          <- NULL
    

  # mise à jour des données géographique du modèle lorsque l'on clique sur le bouton refresh
  observeEvent(input$update_depart, {
              rv$liste_stations_proches_depart  <- GetStationList(m,input$adresse_depart)})
  observeEvent(input$update_arrivee, {
              rv$liste_stations_proches_arrivee <- GetStationList(m,input$adresse_arrivee)
  })
  
  # affichage des map des stations de depart et d'arrivee
  output$geocode_depart <- renderLeaflet({
    res   <- rv$liste_stations_proches_depart
    lamap <- leaflet() %>%
      addTiles() %>%  
      addCircleMarkers(data = res, lng=res$lng, lat=res$lat,layerId = res$number, popup = paste0(as.character(round(res$proba*100,1)),"%"),color = ComputeColor(res$proba))
  })
  output$geocode_arrivee <- renderLeaflet({
    res   <- rv$liste_stations_proches_arrivee
    lamap <- leaflet() %>%
      addTiles() %>%  
      addCircleMarkers(data = res, lng=res$lng, lat=res$lat,layerId = res$number, popup = paste0(as.character(round(res$proba*100,1)),"%"),color = ComputeColor(res$proba))
  })  
  
  # mise à jour des données utiles pour le modèle lorsque l'on clique sur le bouton refresh
  observeEvent(input$update_depart, {
              rv$donnees_depart                <- My.Single.Query(m,
                                                                  address      = input$adresse_depart,
                                                                  hour         = 0:24,
                                                                  day          = c(weekdays(input$date_du_jour, abbreviate = FALSE),
                                                                                   weekdays(input$date_du_jour+1, abbreviate = FALSE)),
                                                                  date_start   = "2016/07/01",
                                                                  date_end     = "2016/12/31",
                                                                  max_distance = 200)
              })

  observeEvent(input$update_arrivee, {
              rv$donnees_arrivee               <- My.Single.Query(m,
                                                                  address      = input$adresse_arrivee,
                                                                  hour         = 0:24,
                                                                  day          = c(weekdays(input$date_du_jour, abbreviate = FALSE),
                                                                                   weekdays(input$date_du_jour+1, abbreviate = FALSE)),
                                                                  date_start   = "2016/07/01",
                                                                  date_end     = "2016/12/31",
                                                                  max_distance = 200)
  })

 # affichage des données requêtées
 output$plotStationDepart01 <- renderPlot({
   station_number <- rv$liste_stations_proches_depart$number[1]
   if (!is.null(rv$donnees_depart))
      ggplot(subset(rv$donnees_depart,number==station_number))+aes(x=time,y=available_bikes,color=summary)+geom_point(lwd=1)+facet_wrap(~dateday,nrow=3)+ggtitle(label = "Velibs disponibles à la station la plus proche")
   })  

 

  output$plotStationArrivee01 <- renderPlot({
    station_number <- rv$liste_stations_proches_arrivee$number[1]
    if (!is.null(rv$donnees_arrivee))
      ggplot(subset(rv$donnees_arrivee,number==station_number))+aes(x=time,y=available_bike_stands,color=summary)+geom_point(lwd=1)+facet_wrap(~dateday,nrow=3)+ggtitle(label = "places disponibles à la station la plus proche")
  })  

  
  
}


shinyApp(ui = ui, server = server)
