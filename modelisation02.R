source("my_function.R")
library(mongolite)
library(data.table)
library(randomForest)
library(pROC)
library(DMwR)
library(ggplot2)


# Connexion à la collection velibW
collection_m <- mongo(collection="velibW"
                      ,url="mongodb://toto:MDPtoto314@thinkr.fr/velib"
                      ,db="velib")

# Recherche des stations a proximites d'une adresse donnée (ici le 8 boulevard satin michel, paris)
data_velib    <- data.frame()
list_station  <- station_approximation(collection_m,address= "8 boulevard saint michel, paris")
# for(kk in 1:length(list_station)){
#   data_velib  <- rbind(data_velib, data_par_station(collection_m,list_station[kk]))
# }
data_velib    <-  vector("list",length(list_station))
for(kk in 1:length(list_station)){
  data_velib[[kk]]  <-data_par_station(collection_m,list_station[kk])
}
data_velib <- do.call(rbind,data_velib)

# Conversion de data_velib en data table
data_velib                 <- as.data.table(data_velib)
data_velib$hour            <- as.factor(data_velib$hour)
levels(data_velib$hour)[1] <- "24"
data_all                   <- traitement_data_pr_modeling(data_velib,binairy=TRUE)

# creer la base d'app et la base test  70%/30%
ind   <- sample(1:2,nrow(data_all),prob=c(0.7,0.3),replace=TRUE)
train <- data_all[ind==1,]
test  <- data_all[ind==2,]

# j'imagine que c'est pour balancer les TRUE et FALSE sur l'ensemble d'apprentissage
train_balance <- SMOTE(bike_dispo~.,train,perc.over=2000,perc.under=110)

# modelise par la forest aleatoire
rf_velib      <- randomForest(train_balance[,2:99], train_balance[,1],ntree=100,mtry=17)
summary(rf_velib)
rf_velib$importance
pred_rf       <- predict(rf_velib, test,type="response")
mat_conf      <- table(test[,1],pred_rf)
sum(diag(mat_conf)/sum(mat_conf))
mat_conf

pred_rf       <- predict(rf_velib, test, type = "prob")
auc           <- roc(test$bike_dispo,pred_rf[,2])
plot(auc,ylim=c(0,1),print.thres=TRUE,main=paste('AUC',round(auc$auc[[1]],2)))
result.coords <- coords(auc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)
mat_conf2     <- table(test[,1],pred_rf[,2]>result.coords["threshold"])

###########modele pour les nbr de velib##########
ggplot(data=data_velib, aes(available_bikes)) + geom_bar()

table(data_velib$available_bikes)


