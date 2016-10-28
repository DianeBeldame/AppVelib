library(jsonlite)
library(dplyr)
library(RJSONIO)


inputFile <- "data_all_Paris.jjson_2016-09-01-1472703936"
con  <- file(inputFile, open = "r")
data <- list()
i <- 0
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  
  line_data <- jsonlite::fromJSON(oneLine)
  data <- rbind(data, line_data)
  i <- i +1
  if (i %% 100 == 0)
  {
    i
  }
} 

close(con)

