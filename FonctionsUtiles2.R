My.Single.Query<-function(m,max_distance=200,address=NULL,day="lundi",nb_stations_max=6,date_start="2016/09/01",date_end="2016/10/01",hour=10,limit_size=10,skip=0){
  
  require(mongolite)
  k<-1
  outputs <- list()
  if(is.null(address)){
    for(month_num in 7:12){
      
      my.aggregate <- paste0('[
                             { "$match":{"month_num": ',month_num,'}},
                             { "$skip"  : ',skip,'},
                             { "$limit" : ',limit_size,'},
                             { "$unwind": "$serie"},
                             { "$match" : {"serie.day" : {"$in" :["',paste(day,collapse = '","'),'"]} ,
                             "serie.hour" : {"$in" : [',paste(hour,collapse = ","),']},
                             "serie.last_update" : {"$gte": ',as.integer(as.POSIXct(date_start, origin="1970-01-01", tz="Europe/Paris"))*1000,'},
                             "serie.last_update" : {"$lte": ',as.integer(as.POSIXct(date_end, origin="1970-01-01", tz="Europe/Paris"))*1000,'}}},
                             { "$project" : {"_id": 0, "number": 1, 
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
                             "serie.cloudcover":1}} 
                             ]') 
      
      output              <- m$aggregate(my.aggregate)
      if(nrow(output)>0)
        output$serie$number <- output$number
      outputs[[k]]           <- output$serie
      k <- k + 1
      
    }
  }
  else{
    require(ggmap)
    my.coordinates <- as.data.frame(geocode(address))
    for(month_num in 7:12){
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
                             { "$unwind": "$serie"},
                             { "$match" : {"serie.day" : {"$in" :["',paste(day,collapse = '","'),'"]} ,
                             "serie.hour" : {"$in" : [',paste(hour,collapse = ","),']},
                             "serie.last_update" : {"$gte": ',as.integer(as.POSIXct(date_start, origin="1970-01-01", tz="Europe/Paris"))*1000,'},
                             "serie.last_update" : {"$lte": ',as.integer(as.POSIXct(date_end, origin="1970-01-01", tz="Europe/Paris"))*1000,'}}},
                             { "$project" : {"_id": 0,
                             "number": 1,
                             "position":1,
                             "dist": 1,
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
                             "serie.precipProbability":1,
                             "serie.apparentTemperature":1,
                             "serie.temperature":1,
                             "serie.humidity":1,
                             "serie.minute":1,
                             "serie.windspeed":1,
                             "serie.visibility":1,
                             "serie.cloudcover":1}} 
                             ]') 
      
      output                <- m$aggregate(my.aggregate)
      if(nrow(output)>0)
        output$serie$number <- output$number
      outputs[[k]]          <- output$serie
      k <- k + 1
    }
  }
  if(k>0){outputs<-do.call(rbind,outputs)}
  outputs$number     <- factor(outputs$number)
  outputs$day_num    <- factor(outputs$day_num)
  outputs$month_num  <- factor(outputs$month_num)
  outputs$time       <- outputs$hour+outputs$minute/60
  
  
  outputs$dateday    <- as.factor(2016*10000 + as.numeric(levels(outputs$month_num))[outputs$month_num] *100 + as.numeric(levels(outputs$day_num))[outputs$day_num] )
  outputs$summary    <- as.factor(outputs$summary)
  
  return(outputs)
}


My.Single.Query.Single.Station<-function(m,station_number,day="lundi",nb_stations_max=6,date_start="2016/09/01",date_end="2016/10/01",hour=10,limit_size=10,skip=0){
  
  require(mongolite)
  k<-1
  outputs <- list()
  for(month_num in 7:12){
    my.aggregate <- paste0('[
                             { "$match":{"month_num": ',month_num,', "number" :{"$in" :[',paste0(station_number,collapse = ','),']}}},
                             { "$unwind": "$serie"},
                             { "$match" : {"serie.day" : {"$in" :["',paste(day,collapse = '","'),'"]} ,
                             "serie.hour" : {"$in" : [',paste(hour,collapse = ","),']},
                             "serie.summary" : {"$ne" : "null"},
                             "serie.last_update" : {"$gte": ',as.integer(as.POSIXct(date_start, origin="1970-01-01", tz="Europe/Paris"))*1000,'},
                             "serie.last_update" : {"$lte": ',as.integer(as.POSIXct(date_end, origin="1970-01-01", tz="Europe/Paris"))*1000,'}}},
                             { "$project" : {"_id": 0,
                             "number": 1,
                             "serie.hour" : 1,
                             "serie.status":1,
                             "serie.bike_stands":1,
                             "serie.available_bikes":1,
                             "serie.available_bike_stands":1,
                             "serie.day":1,
                             "serie.day_num":1,
                             "serie.month_num":1,
                             "serie.summary":{"$ifNull" : ["$serie.summary","null"]},
                             "serie.date":1,
                             "serie.precipIntensity":1,
                             "serie.precipProbability":1,
                             "serie.apparentTemperature":1,
                             "serie.temperature":1,
                             "serie.humidity":1,
                             "serie.minute":1,
                             "dist.location":1,
                             "serie.windspeed":1,
                             "serie.visibility":1,
                             "serie.cloudcover":1}} 
                             ]') 
    
    output              <- m$aggregate(my.aggregate)
    if(nrow(output)>0)
      output$serie$number <- output$number
    outputs[[k]]           <- output$serie
    k <- k + 1
  }
  if(k>0){outputs<-do.call(rbind,outputs)}
  outputs$number             <- factor(outputs$number)
  outputs$day_num            <- factor(outputs$day_num)
  outputs$month_num          <- factor(outputs$month_num)
  outputs$time               <- outputs$hour+outputs$minute/60
  
  outputs$dateday <- as.factor(2016*10000 + as.numeric(levels(outputs$month_num))[outputs$month_num] *100 + as.numeric(levels(outputs$day_num))[outputs$day_num] )
  outputs$summary <- as.factor(outputs$summary)
  return(outputs)
  
}

My.Single.Query.WithoutSerie<-function(m,max_distance=200,address=NULL,nb_stations_max=6){
  
  require(mongolite)
  k<-1
  outputs <- list()
  if(!is.null(address)){
    require(ggmap)
    my.coordinates <- as.data.frame(geocode(address))
    for(month_num in 7:12){
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
                             "number": 1,
                             "position":1,
                             "dist": 1}} 
                             ]') 
      
      output                <- m$aggregate(my.aggregate)
      output                <- cbind(number=output$number,output$position[,c("lat","lng")],distance=output$dist$calculated)
      if(nrow(output)>0)
          outputs[[k]]      <- output
      k <- k + 1
    }
    }
  if(k>0){outputs<-do.call(rbind,outputs)}
  outputs$number     <- factor(outputs$number)
  outputs <- outputs[!duplicated(outputs$number),]
  
  return(outputs)
  }


My.Model.01 <- function(my.data.set.input,liste_stations,today_date,today_hour,prevision,target_type,available_bikes_threshold = 5, available_stands_threshold = 5,type_model = "binomial"){
  # my.data.set    : data frame contenant les données utiles pour la modélisation et l'estimation de la performance du modèle
  # liste_stations : liste des stations classées de la plus proche à la plus lointaine
  # today_date     : la date du jour. Pour faire des simulations, on peut utiliser une date passée
  # today_hour     : l'heure H du jour. On connait toutes les données antérieures au jour today_date à l'heure today_hour
  # prevision      : on veut prévoir le nombre de velib à today_jour + prevision. prevision est donnée en heure
  # address        : l'adresse de départ ou d'arrivée
  # target_type    : "bikes" ou "stands" selon que l'on cherche un vélib ou un place libre
  #
  # exemple: on est le 28 décembre 2016 12h, on veut prédire le nombre de vélibs le lendemain à 10h, on entre
  #          donc:
  #          - today_date = 2016/12/28
  #          - today_hour = 12
  #          - prevision  = 22
  # 
  # les sorties doivent également respecter certaines règles pour s'intègrer dans l'app shiny
  # ces règles sont les suivantes:
  #
  
  today_date_num <- as.numeric(format(today_date, "%Y"))*1e4 + as.numeric(format(today_date, "%m"))*1e2 + as.numeric(format(today_date, "%d"))

  decalage                   <- today_hour+prevision
  decalage[decalage>23]      <- decalage[decalage>23]-24+100
  prevision_date_num         <- today_date_num*100+decalage
  
  # voici un exemple de modélisation:

  # # on requête les données
  # my.data.set             <- My.Single.Query(m,
  #                                            address    = address,
  #                                            day        = c(weekdays(today_date, abbreviate = FALSE),weekdays(input$today_date+1, abbreviate = FALSE)),
  #                                            date_start = "2016/07/01",
  #                                            date_end   = "2016/12/31",
  #                                            hour       = 0:23)
  # 
  # on construit un modele pour chaque station, indépendemment les unes des autres
    # on prepare la liste qui contiendra les resultats
  res <- vector("list", nrow(liste_stations))
  proba <- vector(length = nrow(liste_stations))
  for(i in 1:nrow(liste_stations)){
  
    # on selectionne les données correspondant a la i-ieme station mais uniquement les données antérieur à maintenant (today_date + today_hour)
    my.data.set                <- subset(my.data.set.input,number==liste_stations$number[i])
    my.data.set.daybefore      <- my.data.set
    
    # on calcule la moyenne des velibs par heure dans my.data.set.daybefore
    require(dplyr)
    my.data.set.daybefore      <- as.data.frame(my.data.set.daybefore %>% 
                                             group_by(number,dateday,hour) %>% 
                                             summarise(bike_stands=mean(bike_stands),
                                                       available_bike_stands=mean(available_bike_stands),
                                                       available_bikes=mean(available_bikes)))
    
    # on ajoute une colonne a my.data.set qui donne les dernieres données velibs connues, i.e. à H - X heures
    X                          <- prevision # c'est ici que l'on change le nombre d'heure séparant l'heure de la demande et l'heure de la prévision
    my.data.set$key            <- as.numeric(as.character(my.data.set$dateday))*100+as.numeric(my.data.set$hour)
    
    decalage                   <- as.numeric(my.data.set.daybefore$hour)+X
    decalage[decalage>23]      <- decalage[decalage>23]-24+100
    my.data.set.daybefore$key  <- as.numeric(as.character(my.data.set.daybefore$dateday))*100+decalage
    
    # on se focalise sur la prediction d'une station
    # on suppose la météo connue (il faudra utiliser la prédiction pour évaluer le modèle)
    my.data                    <- merge(my.data.set,subset(my.data.set.daybefore,number==my.data.set$number[1])[,-c(1,2,3)],by.x="key",by.y="key",suffixes = c("","_current"))
    
    # on ajoute une colonne a my.data.set qui donne les données velibs connues du meme jour une semaine avant
    my.data.set.daybefore$key  <- as.numeric(as.character(my.data.set.daybefore$dateday))*100+100+as.numeric(my.data.set.daybefore$hour)
    
    # on se focalise sur la prediction d'une station
    # on suppose la météo connue (il faudra utiliser la prédiction pour évaluer le modèle)
    my.data                    <- merge(my.data,subset(my.data.set.daybefore,number==my.data.set$number[1])[,-c(1,2,3)],by.x="key",by.y="key",suffixes = c("","_weekbefore"))
    
    
    # dans mon exemple, on a 286 observations pour 24 variables
    switch(target_type,
           stands = {
             # on veut modaliser la probabilité de trouver un velib (on estime qu'il faut qu'il y en ai au moins 4)
             my.data$stand_dispo                                                           <- 0
             my.data$stand_dispo[my.data$available_bike_stands>available_stands_threshold] <- 1
             my.data$stand_dispo                                                           <- factor(my.data$stand_dispo)
             my.data$hour                                                                  <- factor(my.data$hour)
             
             # on crée un ensemble de test et un ensemble d'apprentissage
             # on constitue un ensemble de test équilibré pour faciliter l'estimation de la performance du modèle
             my.data.pre                <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)<=today_date_num*100+today_hour)
             my.data.post               <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)>today_date_num*100+today_hour)
             ind_stand_dispo            <- which(my.data.pre$stand_dispo==1)
             ind_stand_nondispo         <- which(my.data.pre$stand_dispo==0)
             ind_1                      <- sample(ind_stand_dispo, length(ind_stand_dispo)/2)
             ind_0                      <- sample(ind_stand_nondispo, length(ind_stand_nondispo)/2)
             my.data.test               <- my.data.pre[c(ind_1,ind_0),]
             my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
             
             # on enleve les données postérieur à la date d'aujourd'hui
             
             table(my.data.test$stand_dispo)
             table(my.data.training$stand_dispo)
             
             # on applique une première modélisation
             
             lbw.for.bestglm   <- within(my.data.training, {
               y           <- stand_dispo  # stand_dispo into y
               stand_dispo  <- NULL        # Delete stand_dispo
             })
             my.data$fit       <- as.numeric(as.character(my.data$stand_dispo))
             my.data$fit_type  <- "actual"             
           },
           bikes={
            # on veut modaliser la probabilité de trouver un velib (on estime qu'il faut qu'il y en ai au moins 4)
            my.data$bike_dispo                                                    <- 0
            my.data$bike_dispo[my.data$available_bikes>available_bikes_threshold] <- 1
            my.data$bike_dispo                                                    <- factor(my.data$bike_dispo)
            my.data$hour                                                          <- factor(my.data$hour)
            
            # on crée un ensemble de test et un ensemble d'apprentissage
            # on constitue un ensemble de test équilibré pour faciliter l'estimation de la performance du modèle
            my.data.pre                <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)<=today_date_num*100+today_hour)
            my.data.post               <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)>today_date_num*100+today_hour)
            ind_bike_dispo             <- which(my.data.pre$bike_dispo==1)
            ind_bike_nondispo          <- which(my.data.pre$bike_dispo==0)
            ind_1                      <- sample(ind_bike_dispo, length(ind_bike_dispo)/2)
            ind_0                      <- sample(ind_bike_nondispo, length(ind_bike_nondispo)/2)
            my.data.test               <- my.data.pre[c(ind_1,ind_0),]
            my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
            
            # on enleve les données postérieur à la date d'aujourd'hui

            table(my.data.test$bike_dispo)
            table(my.data.training$bike_dispo)
            
            # on applique une première modélisation
            
            lbw.for.bestglm   <- within(my.data.training, {
                                      y           <- bike_dispo  # bike_dispo into y
                                      bike_dispo  <- NULL        # Delete bike_dispo
                              })
            my.data$fit       <- as.numeric(as.character(my.data$bike_dispo))
            my.data$fit_type  <- "actual"
        })
    
    ## Reorder variables
    require(bestglm)
    lbw.for.bestglm <-lbw.for.bestglm[, c("precipIntensity","apparentTemperature","humidity","visibility","available_bike_stands_current","available_bikes_current","bike_stands_weekbefore","available_bike_stands_weekbefore","available_bikes_weekbefore","y")]
    res.bestglm     <- bestglm(Xy = lbw.for.bestglm, family = binomial,IC = "BIC",method = "exhaustive")
    my.reg          <- res.bestglm$BestModel

    my.data.training$fit      <- predict(object = my.reg,newdata = my.data.training,type = "response")
    my.data.training$fit_type <- "training data"
    
    my.data.test$fit          <- predict(object = my.reg,newdata = my.data.test,type = "response")
    my.data.test$fit_type     <- "test data"
    
    my.data.post$fit          <- predict(object = my.reg,newdata = my.data.post,type = "response")
    my.data.post$fit_type     <- "post data"
    
    # on stocke les résultats
    resultat        <- list(
                          model = my.reg,
                          data  = rbind(my.data,my.data.training,my.data.test,my.data.post),
                          proba = mean(my.data.post$fit[my.data.post$key==prevision_date_num])
                        )
    
    res[[i]]          <- resultat
    proba[i]          <- resultat$proba
    
  }
  
  summary             <- liste_stations
  summary$color_level <- proba
  summary$value       <- proba
  
  return(list(summary = summary, type = type_model,data = res))
  

  
}