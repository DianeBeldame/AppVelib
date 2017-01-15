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
  value <- vector(length = nrow(liste_stations))
  value_norm<- vector(length = nrow(liste_stations))
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
             switch(type_model,
                    binomial={
                     my.data$stand_dispo                                                           <- 0
                     my.data$stand_dispo[my.data$available_bike_stands>available_stands_threshold] <- 1
                     my.data$stand_dispo                                                           <- factor(my.data$stand_dispo)
                    },
                    poisson={
                      my.data$stand_dispo                                                           <- my.data$available_bike_stands
                    })
             
             my.data$hour                                                                  <- factor(my.data$hour)
             
             # on crée un ensemble de test et un ensemble d'apprentissage
             # on constitue un ensemble de test équilibré pour faciliter l'estimation de la performance du modèle
             my.data.pre                <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)<=today_date_num*100+today_hour)
             my.data.post               <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)>today_date_num*100+today_hour)
             
             switch(type_model,
                    binomial={
                       ind_stand_dispo            <- which(my.data.pre$stand_dispo==1)
                       ind_stand_nondispo         <- which(my.data.pre$stand_dispo==0)
                       ind_1                      <- sample(ind_stand_dispo, length(ind_stand_dispo)/2)
                       ind_0                      <- sample(ind_stand_nondispo, length(ind_stand_nondispo)/2)
                       my.data.test               <- my.data.pre[c(ind_1,ind_0),]
                       my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
                       table(my.data.test$stand_dispo)
                       table(my.data.training$stand_dispo)
                       
                    },
                    poisson={
                       min_val <- min(my.data.pre$stand_dispo)  
                       max_val <- max(my.data.pre$stand_dispo)  
                       ind_stand_dispo            <- which(my.data.pre$stand_dispo>0.5*(min_val+max_val))
                       ind_stand_nondispo         <- which(my.data.pre$stand_dispo<=0.5*(min_val+max_val))
                       ind_1                      <- sample(ind_stand_dispo, length(ind_stand_dispo)/2)
                       ind_0                      <- sample(ind_stand_nondispo, length(ind_stand_nondispo)/2)
                       my.data.test               <- my.data.pre[c(ind_1,ind_0),]
                       my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
             })
             

             
             
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
            my.data$hour                                                          <- factor(my.data$hour)
            switch(type_model,
                   binomial={
                     my.data$bike_dispo                                                    <- 0
                     my.data$bike_dispo[my.data$available_bikes>available_bikes_threshold] <- 1
                     my.data$bike_dispo                                                    <- factor(my.data$bike_dispo)
                   },
                   poisson={
                     my.data$bike_dispo                                                    <- my.data$available_bikes
                   })
            
            my.data$hour                                                                  <- factor(my.data$hour)
            
            # on crée un ensemble de test et un ensemble d'apprentissage
            # on constitue un ensemble de test équilibré pour faciliter l'estimation de la performance du modèle
            my.data.pre                <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)<=today_date_num*100+today_hour)
            my.data.post               <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)>today_date_num*100+today_hour)


            switch(type_model,
                   binomial={
                     ind_bike_dispo             <- which(my.data.pre$bike_dispo==1)
                     ind_bike_nondispo          <- which(my.data.pre$bike_dispo==0)
                     ind_1                      <- sample(ind_bike_dispo, length(ind_bike_dispo)/2)
                     ind_0                      <- sample(ind_bike_nondispo, length(ind_bike_nondispo)/2)
                     my.data.test               <- my.data.pre[c(ind_1,ind_0),]
                     my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
                     table(my.data.test$bike_dispo)
                     table(my.data.training$bike_dispo)
                     
                     },
                   poisson={
                     min_val                    <- min(my.data.pre$bike_dispo)  
                     max_val                    <- max(my.data.pre$bike_dispo)  
                     ind_bike_dispo             <- which(my.data.pre$bike_dispo>0.5*(min_val+max_val))
                     ind_bike_nondispo          <- which(my.data.pre$bike_dispo<=0.5*(min_val+max_val))
                     ind_1                      <- sample(ind_bike_dispo, length(ind_bike_dispo)/2)
                     ind_0                      <- sample(ind_bike_nondispo, length(ind_bike_nondispo)/2)
                     my.data.test               <- my.data.pre[c(ind_1,ind_0),]
                     my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
                   })
            


            
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
    switch(type_model,
           binomial={
              res.bestglm     <- bestglm(Xy = lbw.for.bestglm, family = binomial,IC = "BIC",method = "exhaustive")
           },
           poisson={
             res.bestglm     <- bestglm(Xy = lbw.for.bestglm, family = poisson,IC = "BIC",method = "exhaustive")
           })
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
                          value = mean(my.data.post$fit[my.data.post$key==prevision_date_num])
                        )
    if(type_model=="poisson"){
      resultat$data$fit[resultat$data$fit>max_val] <- max_val
      resultat$data$fit[resultat$data$fit<0] <- 0
    }
    
    res[[i]]          <- resultat
    value[i]          <- resultat$value
    switch(type_model,
           binomial={value_norm[i]<- resultat$value},
           poisson ={
             switch(target_type,
                    bikes={
                      value_norm[i]<- resultat$value/available_bikes_threshold
                    },
                    stands={
                      value_norm[i]<- resultat$value/available_stands_threshold
                    })
             value[i] <- round(value[i],0)
           })
    
    
  }
  value_norm[value_norm>1] <- 1
  summary             <- liste_stations
  summary$color_level <- value_norm
  summary$value       <- value
  
  return(list(summary = summary, type = type_model,data = res))
  

  
}

My.Model.02 <- function(my.data.set.input,liste_stations,today_date,today_hour,prevision,target_type,available_bikes_threshold = 5, available_stands_threshold = 5,type_model = "binomial"){
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
  value <- vector(length = nrow(liste_stations))
  value_norm<- vector(length = nrow(liste_stations))
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
             switch(type_model,
                    binomial={
                      my.data$stand_dispo                                                           <- 0
                      my.data$stand_dispo[my.data$available_bike_stands>available_stands_threshold] <- 1
                      my.data$stand_dispo                                                           <- factor(my.data$stand_dispo)
                    },
                    poisson={
                      my.data$stand_dispo                                                           <- my.data$available_bike_stands
                    })
             
             my.data$hour                                                                  <- factor(my.data$hour)
             
             # on crée un ensemble de test et un ensemble d'apprentissage
             # on constitue un ensemble de test équilibré pour faciliter l'estimation de la performance du modèle
             my.data.pre                <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)<=today_date_num*100+today_hour)
             my.data.post               <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)>today_date_num*100+today_hour)
             
             switch(type_model,
                    binomial={
                      ind_stand_dispo            <- which(my.data.pre$stand_dispo==1)
                      ind_stand_nondispo         <- which(my.data.pre$stand_dispo==0)
                      ind_1                      <- sample(ind_stand_dispo, length(ind_stand_dispo)/2)
                      ind_0                      <- sample(ind_stand_nondispo, length(ind_stand_nondispo)/2)
                      my.data.test               <- my.data.pre[c(ind_1,ind_0),]
                      my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
                      table(my.data.test$stand_dispo)
                      table(my.data.training$stand_dispo)
                      
                    },
                    poisson={
                      min_val <- min(my.data.pre$stand_dispo)  
                      max_val <- max(my.data.pre$stand_dispo)  
                      ind_stand_dispo            <- which(my.data.pre$stand_dispo>0.5*(min_val+max_val))
                      ind_stand_nondispo         <- which(my.data.pre$stand_dispo<=0.5*(min_val+max_val))
                      ind_1                      <- sample(ind_stand_dispo, length(ind_stand_dispo)/2)
                      ind_0                      <- sample(ind_stand_nondispo, length(ind_stand_nondispo)/2)
                      my.data.test               <- my.data.pre[c(ind_1,ind_0),]
                      my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
                    })
             
             
             
             
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
             my.data$hour                                                          <- factor(my.data$hour)
             switch(type_model,
                    binomial={
                      my.data$bike_dispo                                                    <- 0
                      my.data$bike_dispo[my.data$available_bikes>available_bikes_threshold] <- 1
                      my.data$bike_dispo                                                    <- factor(my.data$bike_dispo)
                    },
                    poisson={
                      my.data$bike_dispo                                                    <- my.data$available_bikes
                    })
             
             my.data$hour                                                                  <- factor(my.data$hour)
             
             # on crée un ensemble de test et un ensemble d'apprentissage
             # on constitue un ensemble de test équilibré pour faciliter l'estimation de la performance du modèle
             my.data.pre                <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)<=today_date_num*100+today_hour)
             my.data.post               <- subset(my.data,as.numeric(as.character(dateday))*100+as.numeric(hour)>today_date_num*100+today_hour)
             
             
             switch(type_model,
                    binomial={
                      ind_bike_dispo             <- which(my.data.pre$bike_dispo==1)
                      ind_bike_nondispo          <- which(my.data.pre$bike_dispo==0)
                      ind_1                      <- sample(ind_bike_dispo, length(ind_bike_dispo)/2)
                      ind_0                      <- sample(ind_bike_nondispo, length(ind_bike_nondispo)/2)
                      my.data.test               <- my.data.pre[c(ind_1,ind_0),]
                      my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
                      table(my.data.test$bike_dispo)
                      table(my.data.training$bike_dispo)
                      
                    },
                    poisson={
                      min_val                    <- min(my.data.pre$bike_dispo)  
                      max_val                    <- max(my.data.pre$bike_dispo)  
                      ind_bike_dispo             <- which(my.data.pre$bike_dispo>0.5*(min_val+max_val))
                      ind_bike_nondispo          <- which(my.data.pre$bike_dispo<=0.5*(min_val+max_val))
                      ind_1                      <- sample(ind_bike_dispo, length(ind_bike_dispo)/2)
                      ind_0                      <- sample(ind_bike_nondispo, length(ind_bike_nondispo)/2)
                      my.data.test               <- my.data.pre[c(ind_1,ind_0),]
                      my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
                    })
             
             
             
             
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
    switch(type_model,
           binomial={
             res.bestglm     <- bestglm(Xy = lbw.for.bestglm, family = binomial,IC = "BIC",method = "exhaustive")
           },
           poisson={
             res.bestglm     <- bestglm(Xy = lbw.for.bestglm, family = poisson,IC = "BIC",method = "exhaustive")
           })
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
      value = mean(my.data.post$fit[my.data.post$key==prevision_date_num])
    )
    if(type_model=="poisson"){
      resultat$data$fit[resultat$data$fit>max_val] <- max_val
      resultat$data$fit[resultat$data$fit<0] <- 0
    }
    
    res[[i]]          <- resultat
    value[i]          <- resultat$value
    switch(type_model,
           binomial={value_norm[i]<- resultat$value},
           poisson ={
             switch(target_type,
                    bikes={
                      value_norm[i]<- resultat$value/available_bikes_threshold
                    },
                    stands={
                      value_norm[i]<- resultat$value/available_stands_threshold
                    })
             value[i] <- round(value[i],0)
           })
    
    
  }
  value_norm[value_norm>1] <- 1
  summary             <- liste_stations
  summary$color_level <- value_norm
  summary$value       <- value
  
  return(list(summary = summary, type = type_model,data = res))
  
  
  
}

AddPastDataSet <- function(my.data.set,X){
  
  #### On ajoute les données à H-X
  my.data.set.daybefore      <- my.data.set

  # moyennage par heure
  my.data.set                <- as.data.frame(my.data.set %>% 
                                                group_by(number,dateday,hour) %>% 
                                                mutate(bike_stands=mean(bike_stands),
                                                          available_bike_stands=mean(available_bike_stands),
                                                          available_bikes=mean(available_bikes)))
  
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
  
  # on merge
  my.data.set                <- merge(my.data.set,
                                      subset(my.data.set.daybefore,number==my.data.set$number[1])[,-c(1,2,3)],
                                      by.x="key",by.y="key",suffixes = c("","_minusH"))
  my.data.set$key <- NULL
  
  #### On ajoute les données à J-7
  my.data.set$key            <- as.numeric(as.character(my.data.set$dateday))*100+as.numeric(my.data.set$hour)
  my.data.set.daybefore$key  <- as.numeric(as.character(my.data.set.daybefore$dateday))*100+as.numeric(my.data.set.daybefore$hour)-700
  # on merge
  my.data.set                <- merge(my.data.set,
                                      subset(my.data.set.daybefore,number==my.data.set$number[1])[,-c(1,2,3)],
                                      by.x="key",by.y="key",suffixes = c("","_minusW"))
  my.data.set$key <- NULL
  my.data.set$minute <- NULL
  return(unique(my.data.set))
  
}

Velib.Initialisation <- function(mongo_connection = mongo(collection = "velib4",db = "Velib"), 
                                 address = "8, boulevard saint michel, paris",
                                 lat = NULL,
                                 lng = NULL,
                                 today_date = "2016/10/13",
                                 data_date_start = "2016/07/01",
                                 data_date_end   = "2016/12/31"
){
  
  today_date        <- as.Date(today_date)
  
  # calcul des jours de la semaine
  Week_day_j        <- weekdays(as.Date(today_date))
  week_day_j_plus_1 <- weekdays(as.Date(today_date)+1)
  
  my.data.set.input             <- My.Single.Query(mongo_connection,address    = address,
                                                   day        = c(Week_day_j,week_day_j_plus_1),
                                                   date_start = data_date_start,
                                                   date_end   = data_date_end,
                                                   hour       = 0:23)
  my.data.set.input$hour    <- factor(my.data.set.input$hour)
  my.data.set.input$dateday <- factor(my.data.set.input$dateday)
  
  
  liste_stations                <- My.Single.Query.WithoutSerie(mongo_connection,address    = address)
  
  return(list(today_date=today_date, week_day_j=Week_day_j,week_day_j_plus_1=week_day_j_plus_1,rawdata=my.data.set.input,liste_stations=liste_stations))
  
}

Velib.MiseEnFormeDesDonnees<-function(dataset, prevision){
  dataset$hour    <- factor(dataset$hour)
  dataset$dateday <- factor(dataset$dateday)
  
  # on splite par station
  data.set.per.stations <- split(dataset,dataset$number)
  
  # pour chaque station, on va ajouter en variable explicative le nombre de velibs moyen par heur à "prevision" heures plus tot, et 7 jours plus tôt
  
  data.set.per.stations <- lapply(data.set.per.stations,FUN=AddPastDataSet,prevision)
  
  data.set.per.stations <- do.call(rbind,data.set.per.stations)
  
  return(data.set.per.stations)
  
}

ComputePrevisionDateNum<-function(today_date,today_hour,prevision=0){
  today_date_num <- as.numeric(format(today_date, "%Y"))*1e4 + as.numeric(format(today_date, "%m"))*1e2 + as.numeric(format(today_date, "%d"))
  
  decalage                   <- today_hour+prevision
  decalage[decalage>23]      <- decalage[decalage>23]-24+100
  prevision_date_num         <- today_date_num*100 + decalage
  return(prevision_date_num)  
}


Velib.SeparationDesDonnees<-function(data,today_date,today_hour){
  today_date_num             <- ComputePrevisionDateNum(today_date = as.Date(today_date), today_hour = today_hour)
  my.data.pre                <- subset(data,
                                       as.numeric(as.character(dateday))*100+as.numeric(hour)<=today_date_num)
  my.data.post               <- subset(data,
                                       as.numeric(as.character(dateday))*100+as.numeric(hour)>today_date_num)
  return(list(data.pre = my.data.pre,
              data.post=my.data.post))
  
}

Velib.Modelisation<-function(data,target_type="bikes",threshold=5,model_family="binomial",ratio=0.5,symmetric=FALSE,model_type="rf"){
  
  data$status <- factor(data$status)
  data$day <- factor(data$day)
  
  # On definit Y
  switch(target_type,
         bikes={
           Y<-data$available_bikes
         },
         stands={
           Y<-data$available_stands
         })
  
  switch(model_family,
         binomial={
           Y <- ifelse(Y>threshold,1,0)
         },
         poisson={
           # do nothing
         })
  
  # On supprime les champs à prédire
  data$available_bike_stands <- NULL
  data$available_bikes       <- NULL
  data$time <- NULL
  data$date_day       <- NULL
  
  # On contruit un ensemble de test et un ensemble d'apprentissage
  
  if(symmetric){
    switch(model_family,
           binomial={
             ind_0 <- which(Y==0)
             ind_1 <- which(Y==1)
             if(table(Y)[[1]]>table(Y)[[2]]){
               # on a plus de 0 que de 1
               ind_1_ <- sample(ind_1, length(ind_1)*ratio)
               ind_0_ <- sample(ind_0, length(ind_1)*ratio)
             }else{
               ind_1_ <- sample(ind_1, length(ind_0)*ratio)
               ind_0_ <- sample(ind_0, length(ind_0)*ratio)
             }
             ind_training     <- c(ind_0_,ind_1_)
           },
           poisson={
             min_val    <- min(Y)  
             max_val    <- max(Y)  
             ind_0      <- which(Y>0.5*(min_val+max_val))
             ind_1      <- which(Y<=0.5*(min_val+max_val))
             if(length(ind_0)>length(ind_1)){
               # on a plus de 0 que de 1
               ind_1_ <- sample(ind_1, length(ind_1)*ratio)
               ind_0_ <- sample(ind_0, length(ind_1)*ratio)
             }else{
               ind_1_ <- sample(ind_1, length(ind_0)*ratio)
               ind_0_ <- sample(ind_0, length(ind_0)*ratio)
             }
             ind_training     <- c(ind_0_,ind_1_)
           })
  }else{
    ind_training              <- sample(1:nrow(data), nrow(data)*ratio)
  }
  
  my.data.test      <- cbind(data[-ind_training,],Y=Y[-ind_training])
  my.data.training  <- cbind(data[ind_training,],Y=Y[ind_training])
  my.data.training$Y <- factor(my.data.training$Y)
  # modelisation
  switch(model_family,
         binomial={
           switch(model_type,
                  ridge={
                    require(glmnet)
                    reg.cvridge <- cv.glmnet(data.matrix(my.data.training[,-which(names(my.data.pre)=="Y")]),my.data.training[,which(names(my.data.pre)=="Y")],alpha=0,nfolds = 10,
                                             family="binomial",lambda = 10^(seq(-6,0,length.out = 100)))
                    plot(reg.cvridge)
                    reg.cvridge$lambda.min
                    # prediction sur l'ensemble d'apprentissage
                    pred_ridge <- predict(reg.cvridge,data.matrix(data.matrix(my.data.training[,-which(names(my.data.pre)=="Y")])),s = reg.cvridge$lambda.min,type="class")
                    print(table(my.data.training$Y,pred_rf))
                    # prediction sur l'ensemble de test
                    pred_ridge <- predict(reg.cvridge,data.matrix(data.matrix(my.data.test[,-which(names(my.data.pre)=="Y")])),s = reg.cvridge$lambda.min,type="class")
                    print(table(my.data.test$Y,pred_ridge))
                    
                  },
                  rf={
                    require(randomForest)
                    
                    reg.rf             <- randomForest(Y ~ ., data=my.data.training, importance=TRUE,proximity=TRUE)
                    # prediction sur l'ensemble d'apprentissage
                    pred_rf            <- predict(reg.rf,newdata = my.data.training,type="class")
                    print(table(my.data.training$Y,pred_rf))
                    # prediction sur l'ensemble de test
                    pred_rf            <- predict(reg.rf,newdata = my.data.test,type="class")
                    print(table(my.data.test$Y,pred_rf))
                    return(list(model=reg.rf,ind_training=ind_training,fitted_values=pred_rf,model_family=model_family,model_type=model_type))
                    
                  })
         },
         poisson={})

}
