library(jsonlite)
library(dplyr)
library(RJSONIO)
library(ggplot2)


inputFile <- "data/data_all_Paris.jjson_2016-10-01-1475295963"
con  <- file(inputFile, open = "r")
data <- list()
i <- 0
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  
  line_data <- jsonlite::fromJSON(oneLine)
  data <- rbind(data, line_data)
  i <- i +1
  if (i %% 100 == 0)
  {
    print(i)
  }
} 

close(con)

# Parse date fields from posix
# TODO: which timezone should we use?
data$download_date_parsed <- as.POSIXct(data$download_date[1], origin="1970-01-01", tz="GMT")
data$last_update_parsed <- as.POSIXct(data$last_update[1]/1000, origin="1970-01-01", tz="GMT")
#retrieve day
data$day <- weekdays(data$last_update_parsed, abbreviate = TRUE)
#retrieve hour
data$day <- weekdays(data$last_update_parsed, abbreviate = TRUE)
# retrieve hour
data$hour <- as.numeric(format(data$last_update_parsed, "%H"))

# Plot the evolution of available_bikes
data_901 <- filter(data, number == 901)
plot(x = data_901$last_update, y = data_901$available_bikes, type = "l")

# Plot 
ggplot(data_901)+geom_point(aes(x=hour, y=available_bikes, col=day))
