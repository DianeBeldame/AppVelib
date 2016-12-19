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
<<<<<<< HEAD
                             { "$match" : {"serie.day" : {"$in" :["',paste(day,collapse = '","'),'"]} ,
                             "serie.hour" : {"$in" : [',paste(hour,collapse = ","),']},
=======
                             { "$match" : {"serie.day" : "',day,'" ,
                             "serie.hour" : ',hour,',
>>>>>>> 442ed247a39d9c03582fb2738f186f8be41466f5
                             "serie.last_update" : {"$gte": ',as.integer(as.POSIXct(date_start, origin="1970-01-01", tz="GMT"))*1000,'},
                             "serie.last_update" : {"$lte": ',as.integer(as.POSIXct(date_end, origin="1970-01-01", tz="GMT"))*1000,'}}},
                             { "$project" : {"_id": 0, "number": 1, 
                             "serie.hour" : 1,
<<<<<<< HEAD
                             "serie.status":1,
                             "serie.bike_stands":1,
                             "serie.available_bikes":1,
                             "serie.available_bike_stands":1,
                             "serie.day":1,
=======
                             "serie.available_bikes":1,
                             "serie.available_bike_stands":1,
>>>>>>> 442ed247a39d9c03582fb2738f186f8be41466f5
                             "serie.day_num":1,
                             "serie.month_num":1,
                             "serie.date":1,
                             "serie.summary":1,
                             "serie.precipIntensity":1,
                             "serie.temperature":1,
                             "serie.humidity":1,
<<<<<<< HEAD
                             "serie.minute":1,
                             "dist.calculated":1,
                             "dist.location":1,
=======
>>>>>>> 442ed247a39d9c03582fb2738f186f8be41466f5
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
<<<<<<< HEAD
                             { "$match" : {"serie.day" : {"$in" :["',paste(day,collapse = '","'),'"]} ,
                             "serie.hour" : {"$in" : [',paste(hour,collapse = ","),']},
                             "serie.last_update" : {"$gte": ',as.integer(as.POSIXct(date_start, origin="1970-01-01", tz="GMT"))*1000,'},
                             "serie.last_update" : {"$lte": ',as.integer(as.POSIXct(date_end, origin="1970-01-01", tz="GMT"))*1000,'}}},
                             { "$project" : {"_id": 0,
                             "number": 1, 
                             "serie.hour" : 1,
                             "serie.status":1,
                             "serie.bike_stands":1,
                             "serie.available_bikes":1,
                             "serie.available_bike_stands":1,
                             "serie.day":1,
=======
                             { "$match" : {"serie.day" : "',day,'" ,
                             "serie.hour" : ',hour,',
                             "serie.last_update" : {"$gte": ',as.integer(as.POSIXct(date_start, origin="1970-01-01", tz="GMT"))*1000,'},
                             "serie.last_update" : {"$lte": ',as.integer(as.POSIXct(date_end, origin="1970-01-01", tz="GMT"))*1000,'}}},
                             { "$project" : {"_id": 0, "number": 1, 
                             "serie.hour" : 1,
                             "serie.available_bikes":1,
                             "serie.available_bike_stands":1,
>>>>>>> 442ed247a39d9c03582fb2738f186f8be41466f5
                             "serie.day_num":1,
                             "serie.month_num":1,
                             "serie.date":1,
                             "serie.summary":1,
<<<<<<< HEAD
                             "serie.precipIntensity":1,"serie.precipProbability":1,"serie.apparentTemperature":1,
                             "serie.temperature":1,
                             "serie.humidity":1,
                             "serie.minute":1,
=======
                             "serie.precipIntensity":1,
                             "serie.temperature":1,
                             "serie.humidity":1,
                             "serie.minute":1,
                             "dist.calculated":1,
>>>>>>> 442ed247a39d9c03582fb2738f186f8be41466f5
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
  if(k>0){outputs<-do.call(rbind,outputs)}
  outputs$number       <- factor(outputs$number)
  outputs$day_num       <- factor(outputs$day_num)
  outputs$month_num       <- factor(outputs$month_num)
  return(outputs)
}


