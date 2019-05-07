### Packages
pacman::p_load(caret,lubridate,dplyr,tidyr,skimr,ggplot2,scales,fracdiff,imputeTS,
               edeaR,doParallel,urca,forecast,anytime,reshape,randomForest,class,e1071,
               rstudioapi,plotly,ggplot2)


### Path
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")

#Import Test Dataset 

testData <- read.csv("DataSets/testData.csv")

#Have the same data manipulation as in the training set for the WAP
#Grep function to get the WAP columns
WAP_Test <- grep("WAP",colnames(testData),value = TRUE)

#Model better perform with absolute values
testData[,WAP_Test] <- abs(testData[,WAP_Test])

#Replace value of 100 bpm with -120 in both Validation and Training Data.
testData[testData == 100] <- 120


#### Uni_ID ####

#Load RF Model to predict Building and Floor
RFFIT_Uni_TD <- readRDS("Models/RFFIT_Uni_TD.rds")

#predicting values 
pred_RFFIT_Test_Uni<-predict(RFFIT_Uni_TD, testData)

#Separate Building and Floor prediction and add them to the Test Set
#Separate Floor and building for Validation Set PostResample
pred_RFFIT_Test_Uni <- as.data.frame(pred_RFFIT_Test_Uni)
pred_RFFIT_Test_B_F <- pred_RFFIT_Test_Uni %>% separate(1,into = c("BUILDINGID","FLOOR"), sep = "_")
testData$FLOOR <- as.ordered(pred_RFFIT_Test_B_F$FLOOR)
testData$BUILDINGID <- as.factor(pred_RFFIT_Test_B_F$BUILDINGID)


#### Longitude #### 
#Load RF models for Longitude
RFFIT_Lon_Test <- readRDS("Models/RFFIT_Lon_TD.rds")

#Predict Values
testData$LONGITUDE <-  predict(RFFIT_Lon_Test,testData)


#### Latitude ####
#Load RF models for Longitude
RFFIT_Lat_Test <- readRDS("Models/RFFIT_Lat_TD.rds")

#Predict Values
testData$LATITUDE <-  predict(RFFIT_Lat_Test,testData)


#### Plot Predictions ####
Plot_Test_3D <- plot_ly(testData, x =~LONGITUDE, y =~LATITUDE, z=~FLOOR) %>%
  add_markers()


#### Submission File ####
#Create New Data frame for submission
SUB <- testData %>% select(LATITUDE,LONGITUDE,FLOOR)

#Write csv file
write.csv(SUB, file = "DataSets/SUBMISSION_Martin.csv")