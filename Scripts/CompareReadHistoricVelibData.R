library(jsonlite)

inputFile <- "data/data_all_Paris.jjson_2016-10-01-1475295963"

# Read line by line
ptm <- proc.time()
con  <- file(inputFile, open = "r")
data <- list()
i <- 0
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  
  line_data <- jsonlite::fromJSON(oneLine)
  
  # Bind with previous line
  data <- rbind(data, line_data)
  i <- i +1
  
  if (i %% 1000 == 0)
  {
    print(i)
  }
} 
close(con)
elapsedTime <- proc.time() - ptm
# On thibaut's computer, it took 781s = 13min
print(sprintf("Elapsed time for line by line: %f seconds", elapsedTime[3]))

# Read all lines at once and use lapply
#save date at the begining
ptm <- proc.time()
con  <- file(inputFile, open = "r")
data_tmp  <- lapply(readLines(con), jsonlite::fromJSON, flatten = TRUE)
close(con)
data2 <- rbind(data_tmp[[1]][FALSE,], do.call(rbind, data_tmp))
elapsedTime <- proc.time() - ptm
# On thibaut's computer, it took 148s = 2m28s -> 5 times faster
print(sprintf("Elapsed time for lapply: %f seconds", elapsedTime[3]))


