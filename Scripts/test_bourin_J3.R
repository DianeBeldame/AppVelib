require(mongolite)

# ouvrir la connexion avec la base distante : 
# m  <-mongo(collection = "velib",  url = paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@thinkr.fr/velib"),verbose = TRUE, db = "velib")
m  <-mongo(collection = "velib",  url = paste0("mongodb://diane:gaia314@localhost/velib"),verbose = TRUE, db = "velib")



files <- list.files(path="./data",pattern = "*.gz")[3]
for(j in 1:length(files)){
  inputFile <- paste0("data/",files[j])
  compteur  <- 0
  con       <- file(inputFile, open = "r")
  # pour moi: update stopped at i=172 & compteur=1064
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    line_data <- jsonlite::fromJSON(oneLine)
    print(compteur)
    if (compteur %% 100 == 0)
    {
      print(paste0(files[j]," : ",compteur," lines sent"))
    }
    
    
    if ( TRUE ){
      for(i in 1:nrow(line_data)){
        
        my.query  <- paste0('{ "number" : ', line_data$number[i],'}')
        tmp       <- line_data[i,c("bike_stands","last_update","available_bike_stands","available_bikes","status","download_date")]
        tmp$last_update_parsed   <- as.POSIXct(line_data$last_update[i]/1000, origin="1970-01-01", tz="GMT")
        tmp$download_date_parsed <- as.POSIXct(line_data$download_date[i], origin="1970-01-01", tz="GMT")
        #retrieve day
        tmp$day   <- weekdays(tmp$last_update_parsed, abbreviate = TRUE)
        # retrieve hour
        tmp$hour  <- as.numeric(format(tmp$last_update_parsed, "%H"))
        # retrive minutes
        tmp$minute  <- as.numeric(format(tmp$last_update_parsed, "%M"))
        output    <- m$update(query=my.query,update = paste0('{"$push": {"serie" :{ "last_update_parsed" : "',tmp$last_update_parsed,'" ,"day" : "',tmp$day,'" ,"hour" : ',tmp$hour,' ,"minute" : ',tmp$minute,' ,"bike_stands" : ',tmp$bike_stands,', "last_update" : ',tmp$last_update,', "available_bike_stands" : ',tmp$available_bike_stands,', "available_bikes" : ',tmp$available_bikes,', "status" : "',tmp$status,'", "download_date" : ',tmp$download_date,', "download_date_parsed" : "',tmp$download_date_parsed,'"}}}'),upsert = TRUE)
        
        
      }
    }
    compteur  <- compteur + 1
  }
}
close(con)
