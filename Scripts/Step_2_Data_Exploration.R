# Data Exploration

# Martin Albaek

#29/4/2019

### Packages

pacman::p_load(caret,lubridate,dplyr,tidyr,skimr,ggplot2,scales,fracdiff,imputeTS,
               edeaR,doParallel,urca,forecast,anytime,reshape,randomForest,class,e1071,
               rstudioapi,plotly)


#esquisse::esquisser()

### Path
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")

##Load script
source("Scripts/Step_1_Data_Preparation.R")

#### Initial Data Preparation for graphing ####


#### Data Exploration ####

#Grep function to get the WAP columns
WAP_Train <- grep("WAP",colnames(Train_Data),value = TRUE)
WAP_Val <- grep("WAP",colnames(Val_Data),value = TRUE)

#melt function to create dataframe for count rows uniwue values, transform the values in ID
ID <- colnames(select(Train_Data, -contains("WAP")))
Plot_Train <- melt(Train_Data,id.vars = ID)

Uni_WAP <- Plot_Train %>% group_by(variable,value) %>% summarise(count = n())  
Dstr_Uni_WAP <- Uni_WAP %>% group_by(variable) %>% summarise(Count = n()) 
plot(Uni_WAP$variable,Uni_WAP$count)

#Check distribution of observations of signals
Plot_Train_No100 <- Plot_Train %>% filter(value <100)
hist(Plot_Train_No100$value)
summary(Plot_Train_No100$value)
boxplot(Plot_Train_No100$value)

OutVals = boxplot(Plot_Train_No100$value, plot=FALSE)$out
summary(OutVals)

#add how many time did the WAP get noticed  
SEE <- Plot_Train_No100 %>% group_by(variable) %>% summarise(count = n())
Plot_Train_No100 <-  merge(Plot_Train_No100,SEE,all.x = TRUE)

#different distribution and outliers per observations

#create bins to better visualize results
Plot_Train$Intensity <- 
  ifelse(Plot_Train$value < quantile(Plot_Train$value, 0.25) - 3*IQR(Plot_Train$value),"Outliers",
         ifelse(Plot_Train$value < quantile(Plot_Train$value, 0.25) - 1.5*IQR(Plot_Train$value),"Very Good Signal",
                ifelse(Plot_Train$value < quantile(Plot_Train$value, 0.5),"Good Signal",
                       ifelse(Plot_Train$value < quantile(Plot_Train$value, 0.75),"Bad Signal",
                               ifelse(Plot_Train$value < quantile(Plot_Train$value, 0.75) + 1.5*IQR(Plot_Train$value),"Very Bad Signal",
                                      ifelse(Plot_Train$value < quantile(Plot_Train$value, 0.75) + 3*IQR(Plot_Train$value),"Outliers", "Useless"))))))


#merge
Train_Data$Which_DT <- "T"
Val_Data$Which_DT <- "V"
Total_DT <- rbind(Train_Data,Val_Data)

#### plot different datasets ####

#esquisse::esquisser()

Plot_Total_3D <- plot_ly(Total_DT, x =~LONGITUDE, y =~LATITUDE, z=~FLOOR, color = ~Which_DT,
                         colors = c('#BF382A', '#0C4B8E')) %>%
                         add_markers()

#Plot 3D with the the number of observation per WAP for Training and Validation
Plot_Train_3D <- plot_ly(Train_Data, x =~LONGITUDE, y =~LATITUDE, z=~FLOOR,
                         marker = list(color = ~Diff_Obs, colorscale = c("Portland"), showscale = TRUE)) %>%
                          add_markers()
  

Plot_Val_3D <- plot_ly(Val_Data, x =~LONGITUDE, y =~LATITUDE, z=~FLOOR,
                         marker = list(color = ~Diff_Obs, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
                        add_markers()

#Number of observations per floor and per building
Exploration_distribution  <- ggplot(data = Train_Data) +
  aes(x = BUILDINGID, y = FLOOR) +
  geom_jitter(size = 0.1) +
  theme_minimal()+
  ggtitle("Count Floor Building")+
  geom_bin2d(bins = 30)