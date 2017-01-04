
library(RCurl) 
library(stringr)

#Monthes to select
monthes <- (7:12)

#download path
download_path <- "data/"

#City to download
city <- "Paris"

# Browse the directory
base_url <- "http://vlsstats.ifsttar.fr/rawdata/RawData/RawData_OLD/"
filenames_webpage = getURL(base_url, dirlistonly = TRUE)

# Retrieve paris filenames from webpage
filename_pattern <- paste0("data_all_", city, ".jjson_(\\d{4})-(\\d{2})-(\\d{2})-(\\d{10}).gz");
pos = gregexpr(filename_pattern, filenames_webpage)
filenames <- lapply(pos[[1]], function(x){
  substring(filenames_webpage, x, x+44)
})
#Remove duplicate files
filenames <- unique(filenames)

# Download the Raw data files
lapply(filenames, function(filename) {
  
  filename_match <- str_match(filename, filename_pattern)
  if (filename_match[3] %in% monthes) {
    download.file(paste0(base_url,filename), paste0(download_path,filename))
  }
})