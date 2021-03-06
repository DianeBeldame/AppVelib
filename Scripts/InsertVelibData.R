# Script used to insert regularly weather data on paris

library(jsonlite)
library(mongolite)

#Paris latitude & longitude
lat <- 48.866667
lon <- 2.333333

#retrieve weather for the current datetime
current_weather  <- jsonlite::fromJSON(sprintf("https://api.darksky.net/forecast/%s/%f,%f", my.env$api_key_darksky, lat, lon))
current_weather_local_time <- as.POSIXct(current_weather$currently$time, origin="1970-01-01", tz=current_weather$timezone)

#retrieve jcdecaux velib data
contract <- "Paris"
data_jcdecaux = as.data.frame(fromJSON(sprintf("https://api.jcdecaux.com/vls/v1/stations?contract=%s&apiKey=%s", contract, my.env$api_key_jcdecaux), flatten = TRUE))

#
# Add date helper fields
#
data_jcdecaux$last_update_parsed <- as.POSIXct(data_jcdecaux$last_update/1000, origin="1970-01-01", tz=current_weather$timezone)
#retrieve day
data_jcdecaux$day <- weekdays(data_jcdecaux$last_update_parsed, abbreviate = TRUE)
# retrieve hour
data_jcdecaux$hour <- as.numeric(format(data_jcdecaux$last_update_parsed, "%H"))

#
# Add weather fields
#
data_jcdecaux$weather <- current_weather$currently$summary
data_jcdecaux$weather_1h <- current_weather$hourly$data$summary[2]

#
# Clean unecessary fields
# 
data_jcdecaux$name <- NULL
data_jcdecaux$address <- NULL
data_jcdecaux$banking <- NULL
data_jcdecaux$position.lng <- NULL
data_jcdecaux$position.lat <- NULL
data_jcdecaux$contract_name <- NULL
data_jcdecaux$bonus <- NULL
data_jcdecaux$last_update_parsed <- NULL

#
# Insert into the database
# 
dbName <- "velib"
collectionNameTemplate <- "velib_%i"
collectionNameAll <- "velib_all"
if (!is.null(my.env$user_mongo) && !is.null(my.env$pwd_mongo)) {
  mongo_url <- paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@localhost/", dbName)
} else {
  mongo_url <- paste0("mongodb://localhost/", dbName)
}

# Number of ignored record (already inserted)
# Use an object, otherwise not incremented in the lapply loop
CreateCounter <- function(curr.count) {
  list(
    increment = function(amount) {
      curr.count <<- curr.count + amount
    },
    value = function() {
      return(curr.count)
    }
  )
}
nIgnore <- CreateCounter(0)

m_all <- mongo(collection = collectionNameAll, db = dbName, url = mongo_url)
lapply(split(data_jcdecaux, data_jcdecaux$number), function(x)
{
  m <- mongo(collection = sprintf(collectionNameTemplate, x$number), db = dbName, url = mongo_url)
  
  match <- sprintf('{"last_update" : %f}', x$last_update)
  out <- m$find(match)
  if (nrow(out) > 0) {
    #print(paste0("Already existing record with timestamp ", x$last_update, " for station ", x$number))
    # Increment the counter of ignored record
    nIgnore$increment(1)
  }
  else {
    # Insert in the separated database
    m$insert(x)
    # Insert in the grouped database
    m_all$insert(x)
  }
})

print(paste0(nIgnore$value(), " record ingored because already existing in the database"))


q('yes')