library(ggmap)
library(leaflet)
library(mongolite)
library(dplyr)
library(lubridate)
library(scales)
library(forcats)

m <- mongo(collection = "velib",  url = paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@thinkr.fr/velib"),
           verbose = TRUE, options = ssl_options())


# Warning : ne pas essayer d'afficher le résultat d'une requête MongoDB
# dans la console => affichage mal géré == R session aborted


# part 1 de l'app Shiny : afficher les stations les plus proches ====
# pour une distance donnée ====

# reprise du code de Denis : 
address <- "8 boulevard saint michel, paris"
my.coordinates <- as.data.frame(geocode(address))
distance <- 200 # en metres, à faire varier
# query des stations les plus proches sur l'ancienne base : 
x<-paste0('{ "loc" : { "$near" : { "$geometry" :  { "type" : "Point" , "coordinates" : [ ',my.coordinates$lon, ' , ', my.coordinates$lat,'] } ,  "$maxDistance" : ',distance,'}}}')
output <- m$find(x)


# la map : 
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  #  addMarkers(data = my.coordinates,lng=my.coordinates$lon, lat=my.coordinates$lat)
  addMarkers(data = output, lng=output$position$lng, lat=output$position$lat)

# => partie déjà ajoutée à l'app


# part 2 : récupérer une station et afficher ses courbes de disponibilités
# pour chaque jour de la semaine : 
# TODO in Shiny : associer le mouse clic à la selection d'un marqueur leaflet

# sélection d'un marker sur la map : 
names(output$serie[[1]])
class(output$serie)

# construction de dataset, conversion des dates, drop de variales useless : 
dataset <- output$serie[[1]] %>% mutate(
  last_update_parsed = ymd_hms(last_update_parsed)) %>%
  select(last_update_parsed, available_bikes)

# filtre des données sur la plage de dates choisies par l'utilisateur : 
donnee_filter <- filter(dataset, last_update_parsed > "2016-07-28",
       last_update_parsed < "2016-09-07") %>% # et ajout de variables utiles au plot
  mutate(heure = as.POSIXct(sprintf('%s %s:%s', Sys.Date(), 
                                    format(last_update_parsed, '%H:%M'), '00')),
         jour = fct_relevel(format(last_update_parsed,"%A"), c("lundi", "mardi", "mercredi", "jeudi", "vendredi","samedi")), 
         date = format(last_update_parsed, "%d%m"))

# summary(donnee_filter)
# head(donnee_filter)

# réalisation du plot correspondant : 
ggplot(data = donnee_filter, 
       aes(x=heure, 
       y = available_bikes,
       group = date,
       col= jour))+ 
  geom_line()+
  facet_grid(jour~.)+
  scale_x_datetime(breaks=date_breaks('1 hour'), 
                   labels=date_format('%H:%M'))+ 
  theme(legend.position = "bottom")


