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
current_weather_local_time <- as.POSIXct(current_weather$currently$time, origin="1970-01-01", tz=current_weather$timezone)

dbName <- "velib"
if (!is.null(my.env$user_mongo) && !is.null(my.env$pwd_mongo)) {
  mongo_url <- paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@localhost/", dbName)
} else {
  mongo_url <- paste0("mongodb://localhost/", dbName)
}
print(paste0("Inserting weather data at ", current_weather_local_time, "(local time) in ", mongo_url))
m <- mongo(collection = "weather", db = dbName, url = mongo_url)
m$insert(current_weather)

q('yes')