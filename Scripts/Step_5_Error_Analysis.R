### Packages
pacman::p_load(caret,lubridate,dplyr,tidyr,skimr,ggplot2,scales,fracdiff,imputeTS,
               edeaR,doParallel,urca,forecast,anytime,reshape,randomForest,class,e1071,
               rstudioapi,plotly,ggplot2)


### Path
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")

source("Scripts/Step_4_Models_Creation_Merged_Datasets.R")


### Error Analysis for RF ###
#Error Matrix RF

#creating a new column
test$BUILDINGIDp<-pred_RFFIT_Val_B_F$BUILDINGID
test$FLOORp <- pred_RFFIT_Val_B_F$FLOOR
test$Uni_ID_POSp <- pred_RFFIT_Val_Uni$pred_RFFIT_Val_Uni

#rows of elements where there is a differnece between prediciton and actual value
#which function returns row numbers when comparing two colums (== equal, != unequal)
pred.metric_RFFIT_Val_F <- postResample(pred_RFFIT_Val_B_F$FLOOR,test$FLOOR)

#Create vectors to select errors
ErrorRF_B <- which(test$BUILDINGIDp != test$BUILDINGID)
ErrorRF_F <- which(test$FLOORp != test$FLOOR)
ErrorRF_Uni <- which(test$Uni_ID_POS != test$Uni_ID_POSp)

test$ER_RF_B <- 0
test$ER_RF_F <- 0
test$ER_RF_Uni <- 0

#transform in 1 the to get the errors
test$ER_RF_B[ErrorRF_B] <- 1
test$ER_RF_F[ErrorRF_F] <- 1
test$ER_RF_Uni[ErrorRF_Uni] <- 1

#factorize results
test$ER_RF_B <- as.factor(test$ER_RF_B)
test$ER_RF_F <- as.factor(test$ER_RF_F)
test$ER_RF_Uni <- as.factor(test$ER_RF_Uni)

# Uni ID prediction against actual observation
# esquisse::esquisser()

Plot_P_VS_R_UOBs <- ggplot(data = test, aes(x = Uni_ID_POS, y = Uni_ID_POSp)) +
  geom_jitter(aes(colour=ER_RF_Uni)) +
  theme_minimal() +
  ggtitle("Predicted Values VS Actual Values") +
  theme(plot.title = element_text(hjust = 0.5))

#Number of observations per floor and per building
Sample_distribution <- ggplot(data = Sample_Total) +
  aes(x = BUILDINGID, y = FLOOR) +
  geom_jitter(size = 0.1) +
  theme_minimal() +
  ggtitle("Observations per Floor per Building Sample") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bin2d(bins = 30)

#How sample data is distributed
Plot_Sample_3D <- plot_ly(Sample_Total, x =~LONGITUDE, y =~LATITUDE, z=~FLOOR, color = ~Which_DT,
                         colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers()

##Plot different color for different class of errors 

#Add Prediction of Longitude and Latitude to the test Set
test$LONGITUDEp <- pred_RFFIT_Val_Lon
test$LATITUDEp <- pred_RFFIT_Val_Lat

#Plot residuals KNN and RF
Error_DT <- test
Error_DT$Res_LAT_RFFIT <- pred_RFFIT_Val_Lat - test$LATITUDE
Error_DT$Res_LAT_KNNFIT <- pred_KNNFIT_Val_Lat - test$LATITUDE
Error_DT$Res_LON_RFFIT <- pred_RFFIT_Val_Lon - test$LONGITUDE
Error_DT$Res_LON_KNNFIT <- pred_KNNFIT_Val_Lon - test$LONGITUDE

#plot residuals Lat
attach(Error_DT)
par(mfrow=c(1,2)) 
hist(Res_LAT_RFFIT)
hist(Res_LAT_KNNFIT)

#plot residuals Lon
attach(Error_DT)
par(mfrow=c(1,2)) 
hist(Res_LON_RFFIT)
hist(Res_LON_KNNFIT)

# Plot Prediction
Plot_P_3D <- plot_ly(test, x =~LONGITUDEp, y =~LATITUDEp, z=~FLOORp) %>%
  add_markers()