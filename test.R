# On initialise la connexion à la base mongo et à la collection velibW
require(mongolite)
m      <- mongo(collection = "velibW", url = paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@thinkr.fr/velib"),verbose = TRUE, db = "velib")

my.data.set             <- My.Single.Query(m,
                                           address    = "8, boulevard saint michel, paris",
                                           day        = c("jeudi","vendredi"),
                                           date_start = "2016/07/01",
                                           date_end   = "2016/12/31",
                                           hour       = 0:23)


liste_stations          <- My.Single.Query.WithoutSerie(m,
                                           address    = "8, boulevard saint michel, paris")

temp <- My.Model.01(my.data.set,liste_stations,as.Date("2016/10/13"),8,3,"bikes")

my.data.set2             <- My.Single.Query(m,
                                           address    = "4, boulevard magenta, paris",
                                           day        = c("jeudi","vendredi"),
                                           date_start = "2016/07/01",
                                           date_end   = "2016/12/31",
                                           hour       = 0:23)

liste_stations2          <- My.Single.Query.WithoutSerie(m,
                                                        address    = "4, boulevard magenta, paris")

temp2 <- My.Model.01(my.data.set2,liste_stations2,as.Date("2016/10/13"),8,3,"stands")
ggplot(temp2$data[[1]]$data)+aes(x=time,y=fit,color=fit_type)+geom_point(lwd=1)+facet_wrap(~dateday,nrow=3)+ggtitle(label = "comparaison données et fit")
