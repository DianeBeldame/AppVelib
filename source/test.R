
library(jsonlite)
data    <- fromJSON(sprintf("[%s]", paste(readLines(raw_data),collapse=",")))
#sqdfsqdf sq

con <- file("data_all_Paris.jjson_2016-09-01-1472703936.gz", "r")


my.data <- jsonlite::fromJSON(readLines(con,n=1))
close(con)
