## initialisation
source("FonctionsUtiles2.R")
source(".Rprofile")
require(mongolite)
m      <- mongo(collection = "velibW", url = paste0("mongodb://",my.env$user_mongo,":",my.env$pwd_mongo,"@thinkr.fr/velib"),verbose = TRUE, db = "velib")
m      <- mongo(collection = "velib4",db = "Velib")
## données d'entrées: 88, rue de la vilette
#my.data.set.input             <- My.Single.Query(m,address    = "88, rue de la vilette",
my.data.set.input             <- My.Single.Query(m,address    = "8, boulevard saint michel, paris",
                                                   day        = c("jeudi","vendredi"),
                                                   date_start = "2016/07/01",
                                                   date_end   = "2016/12/31",
                                                   hour       = 0:23)


liste_stations                <- My.Single.Query.WithoutSerie(m,address    = "8, boulevard saint michel, paris")

today_date                    <- as.Date("2016/10/13")
today_hour                    <- 8
prevision                     <- 3
target_type                   <- "stands"
available_bikes_threshold     <- 5
available_stands_threshold    <- 5
type_model                    <- "binomial"

## mise en forme des données

ComputePrevisionDateNum<-function(today_date,today_hour,prevision=0){
  today_date_num <- as.numeric(format(today_date, "%Y"))*1e4 + as.numeric(format(today_date, "%m"))*1e2 + as.numeric(format(today_date, "%d"))
  
  decalage                   <- today_hour+prevision
  decalage[decalage>23]      <- decalage[decalage>23]-24+100
  prevision_date_num         <- today_date_num*100 + decalage
  return(prevision_date_num)  
}

today_date_num     <- ComputePrevisionDateNum(today_date = today_date, today_hour = today_hour,prevision = 0)
prevision_date_num <- ComputePrevisionDateNum(today_date = today_date, today_hour = today_hour,prevision = prevision)
prevision_date_num

my.data.set.input$hour    <- factor(my.data.set.input$hour)
my.data.set.input$dateday <- factor(my.data.set.input$dateday)

# on splite par station
data.set.per.stations <- split(my.data.set.input,my.data.set.input$number)

# pour chaque station, on va ajouter en variable explicative le nombre de velibs moyen par heur à "prevision" heures plus tot, et 7 jours plus tôt

data.set.per.stations <- lapply(data.set.per.stations,FUN=AddPastDataSet,prevision)

data.set.per.stations <- do.call(rbind,data.set.per.stations)

data.set.per.stations$bike_dispo  <- ifelse(data.set.per.stations$available_bikes>2,1,0)
data.set.per.stations$stand_dispo <- ifelse(data.set.per.stations$available_bike_stands>1,1,0)

# affichage des données

require(ggplot2)
ggplot(data.set.per.stations)+aes(x=hour,y=stand_dispo)+geom_point()

## modélisation
# ensemble d'apprentissage
my.data.pre                <- subset(data.set.per.stations,
                                     as.numeric(as.character(dateday))*100+as.numeric(hour)<=today_date_num)
my.data.post               <- subset(data.set.per.stations,
                                     as.numeric(as.character(dateday))*100+as.numeric(hour)>today_date_num)


switch(type_model,
       binomial={
         ind_stand_dispo                   <- which(my.data.pre$bike_dispo==1)
         ind_stand_nondispo                <- which(my.data.pre$bike_dispo==0)
         ind_1                             <- sample(ind_stand_dispo, length(ind_stand_nondispo)/4)
         ind_0                             <- sample(ind_stand_nondispo, length(ind_stand_nondispo)/4)
         my.data.test                      <- my.data.pre[c(ind_1,ind_0),]
         my.data.training                  <- my.data.pre[-c(ind_1,ind_0),]
         table(my.data.test$bike_dispo)
         table(my.data.training$bike_dispo)
         
         
         
       },
       poisson={
         min_val <- min(my.data.pre$stand_dispo)  
         max_val <- max(my.data.pre$stand_dispo)  
         ind_stand_dispo            <- which(my.data.pre$bike_dispo>0.5*(min_val+max_val))
         ind_stand_nondispo         <- which(my.data.pre$bike_dispo<=0.5*(min_val+max_val))
         ind_1                      <- sample(ind_stand_dispo, length(ind_stand_dispo)/2)
         ind_0                      <- sample(ind_stand_nondispo, length(ind_stand_nondispo)/2)
         my.data.test               <- my.data.pre[c(ind_1,ind_0),]
         my.data.training           <- my.data.pre[-c(ind_1,ind_0),]
       })


# reg lasso
# status"," bike_stands"," available_bike_stands"," available_bikes"," hour"," day"," day_num",
# "month_num"," date","summary","precipIntensity","precipProbability","temperature",
# "apparentTemperature","humidity","visibility","number","time","dateday","bike_stands_minusH",
# "available_bike_stands_minusH","available_bikes_minusH","bike_stands_minusW",
# "available_bike_stands_minusW","available_bikes_minusW","bike_dispo","stand_dispo"
X_label <- c("hour","summary","precipIntensity","precipProbability","temperature",
             "apparentTemperature","humidity","visibility","number","bike_stands_minusH",
             "available_bike_stands_minusH","available_bikes_minusH","bike_stands_minusW",
             "available_bike_stands_minusW","available_bikes_minusW")
Y_label <- "bike_dispo"



require(glmnet)
reg.cvridge <- cv.glmnet(data.matrix(my.data.training[,X_label]),my.data.training[,Y_label],alpha=0.1,nfolds = 10,
                         family="binomial",lambda = 10^(seq(-6,0,length.out = 100)))
plot(reg.cvridge)
reg.cvridge$lambda.min
pred_ridge <- predict(reg.cvridge,data.matrix(my.data.test[,X_label]),s = reg.cvridge$lambda.min,type="class")
table(my.data.test$bike_dispo,pred_ridge)

# à faire:
# lasso, ridge, elastic net
# randomForest
require(randomForest)
my.data.training$bike_dispo <- factor(my.data.training$bike_dispo)
reg.rf  <- randomForest(bike_dispo ~ ., data=my.data.training[,c(X_label,Y_label)], importance=TRUE,
                        proximity=TRUE)
pred_rf <- predict(reg.rf,my.data.training[,X_label],type="class")
table(my.data.training$bike_dispo,pred_rf)
pred_rf <- predict(reg.rf,my.data.test[,X_label],type="class")
table(my.data.test$bike_dispo,pred_rf)

X_label2 <- c("hour","summary","bike_dispo")

donclassif<-model.matrix(as.formula(paste0("~",paste(X_label2,collapse="+"))),data=my.data.pre)

mafon <- function(X,k=3){sort(X,dec=FALSE)[k]}
DD    <- dist(donclassif)
ppl   <- apply(as.matrix(DD),1,mafon)
plot(sort(ppl,dec=TRUE),type="h",xlim=c(0,30))


cldb <- dbscan::dbscan(donclassif,eps=.4)
plot(donclassif,col=cldb$cluster+1,pch=cldb$cluster)
table(cldb$cluster,my.data.pre$bike_dispo)
