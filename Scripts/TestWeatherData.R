library(jsonlite)
library(dplyr)
library(ggplot2)

#Paris latitude & longitude
lat <- 48.866667
lon <- 2.333333
# Darksky api ky
api_key           <- "2d470adbb6c72c844cfe527f775be230"
#retrieve weather for the current datetime
current_weather  <- jsonlite::fromJSON(sprintf("https://api.darksky.net/forecast/%s/%f,%f", api_key, lat, lon))

current_weather_fact <- current_weather$currently$summary
current_weather_local_time <- as.POSIXct(current_weather$currently$time, origin="1970-01-01", tz=current_weather$timezone)

# Get the time & summary for the hour before and the next hour
weather_first_hour_time <- as.POSIXct(current_weather$hourly$data$time[1], origin="1970-01-01", tz=current_weather$timezone)
weather_first_hour_fact <- current_weather$hourly$data$summary[1]
weather_next_hour_time <- as.POSIXct(current_weather$hourly$data$time[2], origin="1970-01-01", tz=current_weather$timezone)
weather_next_hour_fact <- current_weather$hourly$data$summary[1]

# Get historic weather for a specific date
historic_date_unix = as.numeric(as.POSIXct("2016-11-13", format="%Y-%m-%d", tz=current_weather$timezone))
historic_weather  <- jsonlite::fromJSON(sprintf("https://api.darksky.net/forecast/%s/%f,%f,%i", api_key, lat, lon, historic_date_unix))
historic_weather_first_hour_time <- as.POSIXct(historic_weather$hourly$data$time[1], origin="1970-01-01", tz=historic_weather$timezone)
historic_weather_first_hour_fact <- historic_weather$hourly$data$summary[1]
historic_weather_second_hour_time <- as.POSIXct(historic_weather$hourly$data$time[2], origin="1970-01-01", tz=historic_weather$timezone)
historic_weather_second_hour_fact <- historic_weather$hourly$data$summary[2]