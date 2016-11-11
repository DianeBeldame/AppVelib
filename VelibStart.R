require(mongolite)
require(rjson)
require(dplyr)

DecauxKey <- "da879af595184f071c181408b837b7da636f924f"
DecauxContractName <- "Paris"
UrlDecaux <- function(decaux,key) {
  if (grepl('\\?',decaux, perl = TRUE)) {
    delim <- '&'
  }
  else {
    delim <- '?'
  }
  sprintf("https://api.jcdecaux.com/vls/v1/%s%sapiKey=%s",decaux,delim,key)
}
GetJsonDecaux <- function(decaux, key = DecauxKey) {
  jsonlite::fromJSON(UrlDecaux(decaux,key), flatten = TRUE)
}
stations_df <- GetJsonDecaux(sprintf("stations?contract=%s",
                                  DecauxContractName))

str(stations_df)



require(mongolite)
stations <- mongo(db = "db1",  # db1 à db9
                  collection ="stations",  # le nom que vous voulez 
                  url = "mongodb://cepe-hd-e.ensae.fr:27017/")
stations$count()
stations$drop()
stations$insert(stations_df)
stations$count()

sts <- stations$find()

require(rjson)

locs = apply(stations_df, 1, function(x) { 
  lat = as.numeric(x[12])
  lon = as.numeric(x[13])
  loc=list( type="Point", coordinates=unname(c(lat, lon)))
})

stations_df$loc <- locs


for (i in 1:nrow(stations_df)) {
  loc = paste0( '{ "loc" : ', rjson::toJSON(stations_df[i,]$loc[[1]]), '}')
  print (loc)
  print(paste0('{"number":' ,stations_df[i,1],'}'))
    stations$update(query = paste0('{"number":' ,stations_df[i,1],'}'), 
                    update = paste0('{ "$set" :' , loc , '}'))
}



stations$index('{ "loc" : "2dsphere" }')
stations$index()

require(ggmap)
addr <- geocode("1 rue du Louvre Paris")

## find closest stations to 1 rue du Louvre in Paris


stations$find(paste0('{"number":' ,i[1],'}'))
stations$find('{"number":33006}')



geoloc <- '{
     "loc": {
        "$nearSphere": {
           "$geometry": {
              "type" : "Point",
              "coordinates" : [%s, %s]
           },
           "$minDistance": %s,
           "$maxDistance": %s
        }
     }
   }'


getFromIndex <- function(lon, lat, 
                         minDistance = 1000,  maxDistance = 5000) {
  l <- list("loc" = list("$nearSphere" = list(
    "$geometry" = list("type" = "Point","coordinates" = as.vector(c(lon, lat)),
                       "$minDistance"= minDistance,"$maxDistance"= maxDistance)
  )
  ))
  
  df <- stations$find(jsonlite::toJSON(l, auto_unbox = T))

  return(df)
}

options(digits=9)
getFromIndex(addr[1,2], addr[1,1], minDistance = 100, 
             maxDistance = 500)

### En dur 
sts <- stations$find(  
'{
     "loc": {
        "$nearSphere": {
           "$geometry": {
              "type" : "Point",
              "coordinates" : [ 48.86102, 2.33587 ]
           },
           "$minDistance": 100,
           "$maxDistance": 500
        }
     }
   }'
) 

req_loc <- sprintf(geoloc,addr[1, 1], addr[1,2], minDistance = 1000,  maxDistance = 5000)
stations$find(req_loc)


stations$find(jsonlite::toJSON(l, auto_unbox = FALSE))


##### Leaflet 

# https://rstudio.github.io/leaflet/
require(leaflet)

stations_df$popup <-  paste0("<b>", stations_df$address, 
                             "</b><br><b> Available bikes : </b>", stations_df$available_bikes)

col <- ifelse(stations_df$status == "OPEN", "green", "red")

map <- leaflet(data = stations_df) %>% 
  addTiles() %>% 
  addCircles(~ position.lng, ~ position.lat, 
             popup = ~popup, 
             radius = ~bike_stands,
             color = col,
             stroke = TRUE, fillOpacity = 0.75) %>%
  addLegend(title = "Status", labels = c("OPEN", "CLOSE"), colors = c("green", "red"))

map

# avec un clustering

map <- leaflet(data = stations_df) %>% 
  addTiles() %>% 
  addMarkers(~ position.lng, ~ position.lat,
             popup = ~ popup, 
             clusterOptions = markerClusterOptions()) %>%
  addLegend(title = "Status", labels = c("OPEN", "CLOSE"), colors = c("green", "red"))

map




