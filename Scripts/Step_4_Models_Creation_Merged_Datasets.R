### Packages
pacman::p_load(caret,lubridate,dplyr,tidyr,skimr,ggplot2,scales,fracdiff,imputeTS,
               edeaR,doParallel,urca,forecast,anytime,reshape,randomForest,class,e1071,
               rstudioapi,plotly,ggplot2)


### Path
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")

source("Scripts/Step_3_Test_Val_Creation.R")

#### Uni_ID Models ####
## try to find the troll in the dataset

 # cluster <- makeCluster(3)
 # registerDoParallel(cluster)
 #  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)

# floor and Building
# Variables <- grep("WAP",colnames(train),value = TRUE)
# 
# RFFIT_Uni_TD <- randomForest(y=  train$Uni_ID_POS, x = train[,Variables])
# 
# KNNFIT_Uni_TD <- train(y = train$Uni_ID_POS,
#                     x = train[,Variables],
#                     method = "knn", trControl = fitControl)
# 
# saveRDS(RFFIT_Uni_TD, file="Models/RFFIT_Uni_TD.rds")
# saveRDS(KNNFIT_Uni_TD, file="Models/KNNFIT_Uni_TD.rds")

RFFIT_Uni_TD <- readRDS("Models/RFFIT_Uni_TD.rds")
KNNFIT_Uni_TD <- readRDS("Models/KNNFIT_Uni_TD.rds")

#### RF- UNI ####

##RF
#predicting values 
pred_RFFIT_Val_Uni<-predict(RFFIT_Uni_TD, test)
pred_RFFIT_Train_Uni <- predict(KNNFIT_Uni_TD, train) 

#post for unique ID variable 
pred.metric_RFFIT_Val_Uni <- postResample(pred_RFFIT_Val_Uni, test$Uni_ID_POS)
pred.metric_RFFIT_Train_Uni <- postResample(pred_RFFIT_Train_Uni, train$Uni_ID_POS)

#Separate Floor and building for Validation Set PostResample
pred_RFFIT_Val_Uni <- as.data.frame(pred_RFFIT_Val_Uni)
pred_RFFIT_Val_B_F <- pred_RFFIT_Val_Uni %>% separate(1,into = c("BUILDINGID","FLOOR"), sep = "_")
pred_RFFIT_Val_B_F$FLOOR <- as.ordered(pred_RFFIT_Val_B_F$FLOOR)
pred_RFFIT_Val_B_F$BUILDINGID <- as.factor(pred_RFFIT_Val_B_F$BUILDINGID)

#post for Validation separate
pred.metric_RFFIT_Val_F <- postResample(pred_RFFIT_Val_B_F$FLOOR,test$FLOOR)
pred.metric_RFFIT_Val_B <- postResample(pred_RFFIT_Val_B_F$BUILDINGID,test$BUILDINGID)

#Confusion Matrix
confusionMatrix(pred_RFFIT_Val_B_F$FLOOR,test$FLOOR)
confusionMatrix(pred_RFFIT_Val_B_F$BUILDINGID,test$BUILDINGID)

#Separate Floor and building for Training Set PostResample
pred_RFFIT_Train_Uni <- as.data.frame(pred_RFFIT_Train_Uni)
pred_RFFIT_Train_B_F <- pred_RFFIT_Train_Uni %>% separate(1,into = c("BUILDINGID","FLOOR"), sep = "_")
pred_RFFIT_Train_B_F$FLOOR <- as.ordered(pred_RFFIT_Train_B_F$FLOOR)
pred_RFFIT_Train_B_F$BUILDINGID <- as.factor(pred_RFFIT_Train_B_F$BUILDINGID)

#post for Training separate
pred.metric_RFFIT_Train_F <- postResample(pred_RFFIT_Train_B_F$FLOOR,train$FLOOR)
pred.metric_RFFIT_Train_B <- postResample(pred_RFFIT_Train_B_F$BUILDINGID,train$BUILDINGID)


#### KNN - UNI ####

#predicting values 
pred_KNNFIT_Val_Uni<-predict(KNNFIT_Uni_TD, test)
pred_KNNFIT_Train_Uni <- predict(KNNFIT_Uni_TD, train) 

#post for unique ID variable 
pred.metric_KNNFIT_Val_Uni <- postResample(pred_KNNFIT_Val_Uni, test$Uni_ID_POS)
pred.metric_KNNFIT_Train_Uni <- postResample(pred_KNNFIT_Train_Uni, train$Uni_ID_POS)

#Separate Floor and building for Training Set PostResample
pred_KNNFIT_Train_Uni <- as.data.frame(pred_KNNFIT_Train_Uni)
pred_KNNFIT_Train_B_F <- pred_KNNFIT_Train_Uni %>% separate(1,into = c("BUILDINGID","FLOOR"), sep = "_")
pred_KNNFIT_Train_B_F$FLOOR <- as.ordered(pred_KNNFIT_Train_B_F$FLOOR)
pred_KNNFIT_Train_B_F$BUILDINGID <- as.factor(pred_KNNFIT_Train_B_F$BUILDINGID)

#post Training
pred.metric_KNNFIT_Train_F <- postResample(pred_KNNFIT_Train_B_F$FLOOR,train$FLOOR)
pred.metric_KNNFIT_Train_B <- postResample(pred_KNNFIT_Train_B_F$BUILDINGID,train$BUILDINGID)

#Separate Floor and building for Validation Set PostResample
pred_KNNFIT_Val_Uni <- as.data.frame(pred_KNNFIT_Val_Uni)
pred_KNNFIT_Val_B_F <- pred_KNNFIT_Val_Uni %>% separate(1,into = c("BUILDINGID","FLOOR"), sep = "_")
pred_KNNFIT_Val_B_F$FLOOR <- as.ordered(pred_KNNFIT_Val_B_F$FLOOR)
pred_KNNFIT_Val_B_F$BUILDINGID <- as.factor(pred_KNNFIT_Val_B_F$BUILDINGID)

#post Validation
pred.metric_KNNFIT_Val_F <- postResample(pred_KNNFIT_Val_B_F$FLOOR,test$FLOOR)
pred.metric_KNNFIT_Val_B <- postResample(pred_KNNFIT_Val_B_F$BUILDINGID,test$BUILDINGID)


#### Longitude Models ####

#names of the variables for the training model
 #Variables_L <- c(Variables,"BUILDINGID")
# 
# #RF
 # RFFIT_Lon_TD <- randomForest(y=  train$LONGITUDE, x = train[,Variables_L])
 # saveRDS(RFFIT_Lon_TD, file="Models/RFFIT_Lon_TD.rds")

RFFIT_Lon_TD <- readRDS("Models/RFFIT_Lon_TD.rds")

pred_RFFIT_Val_Lon <-  predict(RFFIT_Lon_TD,test)
pred_RRFIT_Test_Lon <- predict(RFFIT_Lon_TD,train)

#Post resample RFFIT_Lon
pred.metric_RFFIT_Val_Lon <- postResample(pred_RFFIT_Val_Lon,test$LONGITUDE)
pred.metric_RFFIT_Train_Lon <- postResample(pred_RRFIT_Test_Lon,train$LONGITUDE)

#KNNFIT
 # KNNFIT_Lon_TD <- train(y = train$LONGITUDE,
 #                     x = train[,Variables_L],
 #                     method = "knn", trControl = fitControl)
 # 
 # saveRDS(KNNFIT_Lon_TD, file="Models/KNNFIT_Lon_TD.rds")
KNNFIT_Lon_TD <- readRDS("Models/KNNFIT_Lon_TD.rds")

pred_KNNFIT_Val_Lon <-  predict(KNNFIT_Lon_TD,test)
pred_KNNFIT_Train_Lon <- predict(KNNFIT_Lon_TD,train)

#Post resample KNNFIT_Lon
pred.metric_KNNFIT_Val_Lon <- postResample(pred_KNNFIT_Val_Lon,test$LONGITUDE)
pred.metric_KNNFIT_Train_Lon <- postResample(pred_KNNFIT_Train_Lon,train$LONGITUDE)


#### Latitude Models ####

## RF
# RFFIT_Lat_TD <- randomForest(y=  train$LATITUDE,
#                           x = train[,Variables_L])
# 
# saveRDS(RFFIT_Lat, file="Models/RFFIT_Lat_TD.rds")

RFFIT_Lat_TD <- readRDS("Models/RFFIT_Lat_TD.rds")

pred_RFFIT_Val_Lat <-  predict(RFFIT_Lat_TD,test)
pred_RRFIT_Train_Lat <- predict(RFFIT_Lat_TD,train)

#Post resample RFFIT_Lat
pred.metric_RFFIT_Val_Lat <- postResample(pred_RFFIT_Val_Lat,test$LATITUDE)
pred.metric_RFFIT_Train_Lat <- postResample(pred_RRFIT_Train_Lat,train$LATITUDE)


# KNN
# KNNFIT_Lat_TD <- train(y = train$LATITUDE,
#                     x = train[,Variables_L],
#                     method = "knn", trControl = fitControl)
# 
# saveRDS(KNNFIT_Lat_TD, file="Models/KNNFIT_Lat_TD.rds")
KNNFIT_Lat_TD <- readRDS("Models/KNNFIT_Lat_TD.rds")

pred_KNNFIT_Val_Lat <-  predict(KNNFIT_Lat_TD,test)
pred_KNNFIT_Train_Lat <- predict(KNNFIT_Lat_TD,train)

#Post resample KNNFIT_Lon
pred.metric_KNNFIT_Val_Lat <- postResample(pred_KNNFIT_Val_Lat,test$LATITUDE)
pred.metric_KNNFIT_Train_Lat <- postResample(pred_KNNFIT_Train_Lat,train$LATITUDE)

# stopCluster(cluster)