library(jsonlite)
library(mongolite)
library(lubridate)

#save date at the begining
ptm <- proc.time()

#Paris latitude & longitude
lat <- 48.866667
lon <- 2.333333
#Paris timezone
timezone <- "Europe/Paris"
# Darksky api ky
api_key           <- my.env$api_key_darksky

# The weather is retrieved for every 60 minutes
weather_frequency_minutes <- 30

weather_data <- list()
nweather_data <- 1

weather_api_call <- 0
weather_api_call_limit <- 10000

weather_api_start <- ymd_hms("2016-07-01 00:00:00", tz = timezone)
weather_api_end <- now()
# For test
#weather_api_end <- ymd_hms("2016-07-01 02:00:00", tz = timezone)
weather_api_current <- weather_api_start

# total number of calls needed
total_calls <- trunc(as.numeric(difftime(weather_api_end, weather_api_start, units="mins")) / weather_frequency_minutes)

# Use UTC time to check if the number of api call has been reset
weather_api_call_time <- with_tz(Sys.time(), tz="UTC")
weather_api_call_date_day <- day(weather_api_call_time)
while (weather_api_current <= weather_api_end) {
  
  current_time <- with_tz(Sys.time(), tz="UTC")
  current_date_day <- day(current_time)
  if (current_date_day != weather_api_call_date_day) {
    paste0("Current day is ", current_date_day, " and weather api call day is ", weather_api_call_date_day, " -> We can reset the number of api calls")
    # reset the counter and the date
    weather_api_call <- 0
    weather_api_call_date_day <- current_date_day
  }
  
  #
  # Retrieve historical weather
  #
  weather_url <- sprintf("https://api.darksky.net/forecast/%s/%f,%f,%i", api_key, lat, lon, as.integer(weather_api_current))
  historic_weather  <- jsonlite::fromJSON(weather_url)
  weather_data[[nweather_data]] <- historic_weather
  
  weather_api_call <- weather_api_call + 1
  # To be sure that the date is the same
  historic_weather_local_time <- as.POSIXct(historic_weather$currently$time, origin="1970-01-01", tz=timezone)
  
  if (nweather_data %% 25 == 0)
  {
    percent <- nweather_data * 100 / total_calls
    print(paste0(now(), ": ", nweather_data, " collected (", percent , "%)"))
  }
  
  if (weather_api_call >= weather_api_call_limit)
  {
    # Compute the number of seconds until next day
    next_date <- date(current_time) + days(1)
    # add 120 seconds to be sure
    diff <- as.integer(as.numeric(difftime(next_date, current_time, units="secs"))+120)
    
    print(paste0(now(), ": Number of api calls has been exceeded (", nweather_data ," collected) wait for ", diff, " seconds"))
    Sys.sleep(diff)
    print(paste0(now(), ": Resume after waiting"))
  }
  
  # go to next time
  weather_api_current <- weather_api_current + minutes(weather_frequency_minutes)
  nweather_data <- nweather_data + 1
} 

#
# Insert into the database
# 
dbName <- "velib"
collectionName <- "weather"
if (!is.null(my.env$user_mongo) && !is.null(my.env$pwd_mongo)) {
  mongo_url <- paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@localhost/", dbName)
} else {
  mongo_url <- paste0("mongodb://localhost/", dbName)
}
m <- mongo(collection = collectionName, db = dbName, url = mongo_url)
m$insert(weather_data)

elapsedTime <- proc.time() - ptm
sprintf("Elapsed time: %f seconds", elapsedTime[3])

