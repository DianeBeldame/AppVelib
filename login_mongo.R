library(mongolite)

# en local depuis Rstudio

mongo(collection = "velib",  url = "mongodb://user:mdp@localhost/velib",
      verbose = TRUE, options = ssl_options())$count()
