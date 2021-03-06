# 1ere version d'une application VelibQuatuor
 library(shiny)
 library(leaflet)
 library(ggmap)
 library(mongolite)
 library(ggplot2)

 #source("FonctionsUtiles2.R")

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
  return(output)
}

ComputeColor <- function(proba){
  R   <- (1-proba)
  G   <- proba
  R[is.na(proba)] <- 0
  G[is.na(proba)] <- 0
  COL <- rgb(R, G, 0)
  # print(COL)
  return(COL)
}

# On initialise la connexion à la base mongo et à la collection velibW
#m      <- mongo(collection = "velibW", url = paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@thinkr.fr/velib"),verbose = TRUE, db = "velib")
m      <- mongo(collection = "velib4",db = "Velib")
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
                                    dateInput(inputId = "date_du_jour",label = "Date du jour",value = as.Date("2016/10/13")),
                                    numericInput(inputId = "heure_du_jour",label = "heure du jour",min = 0,max = 23,step = 1,value = 8)
                                    ),
                                   wellPanel(title = "Prevision",
                                    numericInput(inputId = "prevision",label = "prévision à : ",min = 1,max = 24,step = 1,value = 20)
                                   ),
                                   selectInput(inputId = "selectModel",label = "choix du modèle",choices = c("Model01","Model02","Model03")),
                                   numericInput(inputId = "thresholdBikes",label = "Seuil nombre de vélib ",min = 1,max = 10,step = 1,value = 5),
                                   numericInput(inputId = "thresholdStands",label = "Seuil nombre de place ",min = 1,max = 10,step = 1,value = 5),
                                   actionButton(inputId = "compute", label = "lance le calcul"),
                                   uiOutput(outputId = "selectStationDepart"),
                                   uiOutput(outputId = "selectStationArrivee")
                                   ),
                            column(8,
                                   plotOutput(outputId = "plotStationDepart01"),
                                   plotOutput(outputId = "plotStationArrivee01")
                                   )
                            ),
                          fluidRow(    
                            column(4),
                            column(8,
                                   plotOutput(outputId = "plotModelDepart"),
                                   plotOutput(outputId = "plotModelArrivee")
                                  )
                          ))
                 ,
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
  
  temp_depart  <- GetStationList(m,"8 boulevard saint michel, paris")
  temp_arrivee <- GetStationList(m,"4 boulevard magenta, paris")
  rv$liste_stations_proches_depart  <- temp_depart
  rv$liste_stations_proches_arrivee <- temp_arrivee
  rv$donnees_depart                 <- My.Single.Query(m,
                                                       address      = "8 boulevard saint michel, paris",
                                                       hour         = 0:24,
                                                       day          = c(weekdays(as.Date("2016/10/13"), abbreviate = FALSE),
                                                                        weekdays(as.Date("2016/10/13")+1, abbreviate = FALSE)),
                                                       date_start   = "2016/07/01",
                                                       date_end     = "2016/12/31",
                                                       max_distance = 200)
  rv$donnees_arrivee               <- My.Single.Query(m,
                                                      address      = "4 boulevard magenta, paris",
                                                      hour         = 0:24,
                                                      day          = c(weekdays(as.Date("2016/10/13"), abbreviate = FALSE),
                                                                       weekdays(as.Date("2016/10/13")+1, abbreviate = FALSE)),
                                                      date_start   = "2016/07/01",
                                                      date_end     = "2016/12/31",
                                                      max_distance = 200)
  
  rv$resultat_modelisation_depart <- {
    temp             <- temp_depart
    temp$color_level <- 0
    temp$value       <- 0
    list(summary = temp, type="binomial")
  }
  
  rv$resultat_modelisation_arrivee <- {
    temp             <- temp_arrivee
    temp$color_level <- 0
    temp$value       <- 0
    list(summary = temp, type="binomial")
  }
  
  observeEvent(input$update_depart,{
    print("MAJ : liste_stations_proches_depart")
    rv$liste_stations_proches_depart  <- GetStationList(m,input$adresse_depart)

    print("MAJ : donnees_depart")
    rv$donnees_depart                 <- My.Single.Query(m,
                                                         address      = input$adresse_depart,
                                                         hour         = 0:24,
                                                         day          = c(weekdays(input$date_du_jour, abbreviate = FALSE),
                                                                          weekdays(input$date_du_jour+1, abbreviate = FALSE)),
                                                         date_start   = "2016/07/01",
                                                         date_end     = "2016/12/31",
                                                         max_distance = 200)
    rv$resultat_modelisation_depart <- {
      temp             <- rv$liste_stations_proches_depart
      temp$color_level <- 0
      temp$value       <- 0
      list(summary = temp, type="binomial")
    }
     })
    
  observeEvent(input$update_arrivee,{
    print("MAJ : liste_stations_proches_arrivee")
    rv$liste_stations_proches_arrivee <- GetStationList(m,input$adresse_arrivee)
    print("MAJ : donnees_arrivee")
    rv$donnees_arrivee               <- My.Single.Query(m,
                                                      address      = input$adresse_arrivee,
                                                      hour         = 0:24,
                                                      day          = c(weekdays(input$date_du_jour, abbreviate = FALSE),
                                                                       weekdays(input$date_du_jour+1, abbreviate = FALSE)),
                                                      date_start   = "2016/07/01",
                                                      date_end     = "2016/12/31",
                                                      max_distance = 200)
    rv$resultat_modelisation_arrivee <- {
      temp             <- rv$liste_stations_proches_arrivee
      temp$color_level <- 0
      temp$value       <- 0
      list(summary = temp, type="binomial")
    }
    
    })
  
  #choix des stations à afficher
  output$selectStationDepart <- renderUI({
    selectInput(inputId = "stationDepart",label = "Station au départ à afficher",choices = rv$liste_stations_proches_depart$number)
  })
  output$selectStationArrivee <- renderUI({
    selectInput(inputId = "stationArrivee",label = "Station à l'arrivée à afficher",choices = rv$liste_stations_proches_arrivee$number)
  })
  
  # on lance le calcul qui va mettre à jour les données lorsque l'on clique sur calcul
  observeEvent(input$compute,{
    print("MAJ : resultat_modelisation_depart")
    switch(input$selectModel,
           Model01={
              rv$resultat_modelisation_depart <- My.Model.01(rv$donnees_depart,rv$liste_stations_proches_depart,
                                                             input$date_du_jour,input$heure_du_jour,input$prevision,"bikes",type_model="binomial",
                                                             available_bikes_threshold=input$thresholdBikes)
           },
           Model02={
              rv$resultat_modelisation_depart <- My.Model.01(rv$donnees_depart,rv$liste_stations_proches_depart,input$date_du_jour,input$heure_du_jour,input$prevision,"bikes",type_model="poisson",
                                                             available_bikes_threshold=input$thresholdBikes)
           },
           Model03={
             # ajoutez votre modèle ici
           })
           
  })
  
  observeEvent(input$compute,{
    print("MAJ : resultat_modelisation_arrivee")
    switch(input$selectModel,
           Model01={
              rv$resultat_modelisation_arrivee <- My.Model.01(rv$donnees_arrivee,rv$liste_stations_proches_arrivee,input$date_du_jour,input$heure_du_jour,input$prevision,"stands",type_model="binomial",
                                                              available_stands_threshold=input$thresholdStands)
           },
           Model02={
             rv$resultat_modelisation_arrivee <- My.Model.01(rv$donnees_arrivee,rv$liste_stations_proches_arrivee,input$date_du_jour,input$heure_du_jour,input$prevision,"stands",type_model="poisson",
                                                             available_stands_threshold=input$thresholdStands)
           },
           Model03={
             # ajoutez votre modèle ici
           })
  })  

  # affichage des map des stations de depart et d'arrivee
  output$geocode_depart <- renderLeaflet({
    print("MAJ: geocode_depart")
    res         <- rv$resultat_modelisation_depart$summary
    model_type  <- rv$resultat_modelisation_depart$type
    if(model_type=="binomial"){
      # on affiche des pourcentages dans les popups
      lamap <- leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = res, lng=res$lng, lat=res$lat,layerId = res$number, popup = paste0(res$number," : ",as.character(round(res$value*100,1)),"%"),color = ComputeColor(res$color_level))
    }else{
      print(res)
      # on affiche des nombres dans les popups
      lamap <- leaflet() %>%
        addTiles() %>%  
        addCircleMarkers(data = res, lng=res$lng, lat=res$lat,layerId = res$number, popup = paste0(res$number," : ",res$value),color = ComputeColor(res$color_level))
    }
  })
  
  output$geocode_arrivee <- renderLeaflet({
    print("MAJ: geocode_arrivee")
    res         <- rv$resultat_modelisation_arrivee$summary
    model_type  <- rv$resultat_modelisation_arrivee$type
    if(model_type=="binomial"){
      # on affiche des pourcentages dans les popups
      lamap <- leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = res, lng=res$lng, lat=res$lat,layerId = res$number, popup = paste0(res$number," : ",as.character(round(res$value*100,1)),"%"),color = ComputeColor(res$color_level))
    }else{
      # on affiche des nombres dans les popups
      lamap <- leaflet() %>%
        addTiles() %>%  
        addCircleMarkers(data = res, lng=res$lng, lat=res$lat,layerId = res$number, popup = paste0(res$number," : ",res$value),color = ComputeColor(res$color_level))
    }
    
  })  
  
 
 # affichage des données requêtées
 output$plotStationDepart01 <- renderPlot({
   print("MAJ: plotStationDepart01")
   station_number <- input$stationDepart
   if(is.null(station_number)){
    station_number <- rv$liste_stations_proches_depart$number[1]
   }
   if (!is.null(rv$donnees_depart))
      ggplot(subset(rv$donnees_depart,number==station_number))+aes(x=time,y=available_bikes,color=summary)+geom_point(lwd=1)+facet_wrap(~dateday,nrow=3)+ggtitle(label = paste0("Velibs disponibles à la station ",station_number))
   })  

 

  output$plotStationArrivee01 <- renderPlot({
    print("MAJ: plotStationArrivee01")    
    
    station_number <- input$stationArrivee
    if(is.null(station_number)){
      station_number <- rv$liste_stations_proches_arrivee$number[1]
    }
    
    if (!is.null(rv$donnees_arrivee))
      ggplot(subset(rv$donnees_arrivee,number==station_number))+aes(x=time,y=available_bike_stands,color=summary)+geom_point(lwd=1)+facet_wrap(~dateday,nrow=3)+ggtitle(label = paste0("places disponibles à la station ",station_number))
  })  


  
  # # # on affiche la comparaison model mesure
  output$plotModelArrivee <- renderPlot(expr = {
    print("MAJ : plotModelArrivee")
    station_number <- input$stationArrivee
    if(is.null(station_number)){
      station_number <- rv$liste_stations_proches_arrivee$number[1]
    }
    i <- which(rv$liste_stations_proches_arrivee$number==station_number)
    
    if (!is.null(rv$resultat_modelisation_arrivee$data)){
      # uniquement s'il y a des données a tracer
      ggplot(rv$resultat_modelisation_arrivee$data[[i]]$data)+aes(x=time,y=fit,color=fit_type)+geom_point(lwd=1)+facet_wrap(~dateday,nrow=3)+ggtitle(label = paste0("comparaison données et fit station ",station_number))
    }
  })
  
  output$plotModelDepart <- renderPlot(expr = {
    print("MAJ : plotModelDepart")
    station_number <- input$stationDepart
    if(is.null(station_number)){
      station_number <- rv$liste_stations_proches_depart$number[1]
    }
    i <- which(rv$liste_stations_proches_depart$number==station_number)
    if (!is.null(rv$resultat_modelisation_depart$data)){
      # uniquement s'il y a des données a tracer
     ggplot(rv$resultat_modelisation_depart$data[[i]]$data)+aes(x=time,y=fit,color=fit_type)+geom_point(lwd=1)+facet_wrap(~dateday,nrow=3)+ggtitle(label = paste0("comparaison données et fit station ",station_number))
    }
  })


  
}


shinyApp(ui = ui, server = server)
