data_par_station <- function(m,num_station){
  require(mongolite)
  my_aggregate <- paste0('[
                         {"$match":{"number":',num_station,'}},
                         {"$unwind":"$serie"},
                         {"$project":{"_id":0,
                         "number" : 1,
                         "address" : 1,
                         "serie.number" :1,
                         "serie.hour" : 1,
                         "serie.status":1,
                         "serie.bike_stands":1,
                         "serie.available_bikes":1,
                         "serie.available_bike_stands":1,
                         "serie.day":1,
                         "serie.day_num":1,
                         "serie.month_num":1,
                         "serie.date":1,
                         "serie.summary":1,
                         "serie.precipIntensity":1,
                         "serie.temperature":1,
                         "serie.humidity":1,
                         "serie.minute":1,
                         "dist.calculated":1,
                         "dist.location":1,
                         "serie.windspeed":1,
                         "serie.visibility":1,
                         "serie.cloudcover":1
                         }}
  ]')

  out <- m$aggregate(my_aggregate)
  out$serie$address <- out$address
  out$serie$number <- out$number
  out <- out$serie
}


traitement_data_pr_modeling <- function(dataset, binairy=TRUE){
  require(data.table)
  require(dplyr)
  require(class)
  require(nnet)

  if(binairy == TRUE) {
    # Creer la variable binaire: 0 si aucun velib disponible, 1 si au moins 1 velib est disponible  
    dataset[,':='(bike_dispo = as.factor(ifelse(available_bikes == 0, 0, 1)))]
  } else {
    # Crée une variable categorielle: le nombre de velibe si <4, sinon on note "4+"
    # Je ne suis pas sûr que ça fonctionne pour faire une régression de type Poisson,
    # mais ce n'est peut être pas ce que tu veux faire.
    dataset[,':='(bike_dispo = as.factor(ifelse(available_bikes < 4, available_bikes, "4+")))]
  }
    
  # supprimer les stations fermes 
  setkey(dataset,status)
  dataset <- dataset[.("OPEN")]
  dataset[,status:=NULL]
  
# transferer la capacite de stations comme une facteur
  dataset$bike_stands <- as.factor(dataset$bike_stands)
  
# supprimer la minute, parce qu on va pas predire la nbr de velib ds minutes
  dataset[,minute:=NULL]
  
# cluster les info meteo
  data_meteo         <- dataset%>%
                        select(summary,precipIntensity,temperature,humidity,visibility)
  cluster_meteo      <- kmeans(data_meteo[,2:5,with=FALSE],centers=10) # pourquoi 10 groupes?
  data_meteo$cluster <- as.factor(cluster_meteo$cluster)
  data_meteo$summary <- as.factor(data_meteo$summary)
  data_meteo         <- data_meteo %>%
                        select(summary, cluster)
  dataset[,c("summary","precipIntensity","temperature", "humidity" ,"visibility")
             :=NULL]
  dataset            <- cbind(dataset,data_meteo)

# concatener address: pour en extraire les arrondissements
  dataset$address    <- tolower(dataset$address)
  dataset$c          <- unlist(lapply(dataset$address, concatene_add))
  dataset[,c("arr"):=as.factor(substr(address,c-4,c+1))]
  dataset[,c("c","address"):=NULL]

# supprimer la date
  dataset[,c("date"):=NULL]

# dummy hour: c'est un tableau avec 24 colonnes nommées h_1, h_2, ..., h_24
# qui contient des 0 et des 1. pour une observation donnée, si hour=23, seule 
# la colonne h_23 contiendra un 1, les autres sont à 0.
  col_nom <- c()
  for(hh in unique(dataset$hour)) {
    col_nom[hh] <- paste0("h_",hh)
  }
  dummy_heure           <- as.data.frame(class.ind(dataset$hour))
  colnames(dummy_heure) <- col_nom
  dummy_heure           <- as.data.frame(apply(dummy_heure,MARGIN=2, FUN = as.factor))

  # dummy week: meme chose avec les jours de la semaine
  # les colonnes sont nommées dans l'ordre suivant chez moi: "dimanche" "jeudi"    "lundi"    "mardi"    "mercredi" "samedi"   "vendredi"
  dummy_week <- as.data.frame(class.ind(dataset$day))
  dummy_week <- as.data.frame(apply(dummy_week,MARGIN = 2,FUN = as.factor))
  
  # dummy day: meme chose avec les jours dans le mois
  # les colonnes sont nommées D_1, D_2, ..., D_31
  col_nom <- c()
  for(hh in unique(dataset$day_num)) {
    col_nom[hh] <- paste0("D_",hh)
  }
  dummy_day <- as.data.frame(class.ind(dataset$day_num))
  colnames(dummy_day)<-col_nom  
  dummy_day <- as.data.frame(apply(dummy_day, MARGIN = 2, FUN = as.factor))
  
  # arr: meme chose avec les arrondissements
  col_nom <- c()
  for(hh in unique(dataset$arr)) {
    col_nom[hh] <- paste0("arr_",hh)
  }
  dummy_arr <- as.data.frame(class.ind(dataset$arr))
  colnames(dummy_arr)<-col_nom  
  dummy_arr <- as.data.frame(apply(dummy_arr, MARGIN = 2, FUN = as.factor))
  
# summary: le summary de le meteo
  dummy_summary <- as.data.frame(class.ind(dataset$summary))
  dummy_summary <- as.data.frame(apply(dummy_summary, MARGIN = 2, FUN = as.factor))

# type meteo: l'indice du cluster meteo calculé plus haut
  col_nom <- c()
  for(hh in unique(dataset$cluster)) {
    col_nom[hh] <- paste0("T_",hh)
  }
  dummy_meteo           <- as.data.frame(class.ind(dataset$cluster))
  colnames(dummy_meteo) <- col_nom  
  dummy_meteo           <- as.data.frame(apply(dummy_meteo, MARGIN = 2, FUN = as.factor))
  
# Jointure
  bike_dispo  <- dataset$bike_dispo
  bike_stands <- dataset$bike_stands
  number      <- as.factor(dataset$number)
  data_all    <- cbind(bike_dispo,  bike_stands,   number,
                       dummy_heure, dummy_week,    dummy_day,
                       dummy_arr,   dummy_summary, dummy_meteo)

}
  
concatene_add <- function(address){
  n <- length(gregexpr("\\d",address)[[1]])
  d <- gregexpr("\\d",address)[[1]][n]
  print(d-1)
}


station_approximation<-function(m,max_distance=200,address=NULL,nb_stations_max=6,limit_size=10,skip=0){
  
  require(mongolite)
  k<-1
  outputs <- list()
  num_station <- list()

    require(ggmap)
    my.coordinates <- as.data.frame(geocode(address))
    for(month_num in 7:12){
      # my.aggregate <- paste0('[
      #                        {"$geoNear" : {
      #                        "near": { "type": "Point", "coordinates": [',my.coordinates$lon, ' , ', my.coordinates$lat,'] },
      #                        "distanceField": "dist.calculated",
      #                        "maxDistance": ',max_distance,',
      #                        "includeLocs": "dist.location",
      #                        "query":{"month_num": ',month_num,'},
      #                        "num" : ',nb_stations_max,',
      #                        "spherical": true
      #                        }
      #                        },
      #                        { "$unwind": "$serie"},
      #                        { "$project" : {"_id": 0,
      #                        "number": 1, 
      #                        "serie.number" : 1}} 
      #                        ]') 
      
      # je propose cette aggregation puisque ici on ne souhaite que trouver le "number" des stations qui sont proches  de l'adresse indiqué
      # on n'a donc pas besoin de deplier ("unwind") serie.
      my.aggregate <- paste0('[
                             {"$geoNear" : {
                             "near": { "type": "Point", "coordinates": [',my.coordinates$lon, ' , ', my.coordinates$lat,'] },
                             "distanceField": "dist.calculated",
                             "maxDistance": ',max_distance,',
                             "includeLocs": "dist.location",
                             "query":{"month_num": ',month_num,'},
                             "num" : ',nb_stations_max,',
                             "spherical": true
    }
},
                             { "$project" : {"_id": 0,
                             "number": 1
                             }} 
                             ]') 
      
      output              <- m$aggregate(my.aggregate)
      if(nrow(output)>0)
        num_station[[k]] <- output$number
      k <- k + 1
    }
  num_station <- unique(unlist(num_station))  
  return(num_station)
}


En_To_Fr <- function(day){
  if (day == "Monday"){
    jour <- "lundi"
  } else {
    if (day == "Tuesday") {
      jour <- "mardi"
    } else {
      if (day == "Wednesday") {
        jour <- "mercredi"
      } else {
        if (day == "Thursday") {
          jour <- "jeudi"
        } else {
          if (day == "Friday") {
            jour <- "vendredi"
          } else {
            if (day == "Saturday") {
              jour <- "samedi"
            } else {
              if (day == "Sunday") {
                jour <- "dimanche"
              }
            }
          }
        }
      }
    }
  }
  return(jour)
}

read.Mongo<-function(collection, add_depart, heure_depart){
  data <- data.frame()
  list_station <- station_approximation(collection, add_depart)
  for(kk in 1:length(list_station)){
    data <- rbind(data, data_par_station(collection,list_station[kk]))
  }
  
  data <- as.data.table(data)
  data$hour<-as.factor(data$hour)
  levels(data$hour)[1] <- "24"
  
  return(data)
}



