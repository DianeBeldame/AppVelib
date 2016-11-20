# Script used to insert regularly weather data on paris

library(jsonlite)
library(mongolite)

#Paris latitude & longitude
lat <- 48.866667
lon <- 2.333333
# Darksky api key
api_key           <- my.env$api_key_darksky
#retrieve weather for the current datetime
current_weather  <- jsonlite::fromJSON(sprintf("https://api.darksky.net/forecast/%s/%f,%f", api_key, lat, lon))

m <- mongo(collection = "weather", db = "velibs")
m$insert(current_weather)

q('yes')