# Preparing data set

# Martin Albaek

#16/4/2019

### Packages ###
pacman::p_load(caret,lubridate,dplyr,tidyr,skimr,ggplot2,scales,fracdiff,imputeTS,
               edeaR,doParallel,urca,forecast,anytime,reshape,randomForest,class,e1071,
               rstudioapi,plotly)

### Path
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")

trainingData <- read.csv("DataSets/trainingData.csv")
validationData <- read.csv("DataSets/validationData.csv")

Val_Data <- validationData
Train_Data <- trainingData

#### Initial Data Preparation for graphing ####

#Grep function to get the WAP columns
WAP_Train <- grep("WAP",colnames(Train_Data),value = TRUE)
WAP_Val <- grep("WAP",colnames(Val_Data),value = TRUE)

#Model better perform with absolute values
Train_Data[,WAP_Train] <- abs(Train_Data[,WAP_Train])
Val_Data[,WAP_Val] <- abs(Val_Data[,WAP_Val])

#Replace value of 100 bpm with -120 in both Validation and Training Data.
Train_Data[Train_Data == 100] <- 120
Val_Data[Val_Data == 100] <- 120

#drop Columns that have zero variance for validation set and Training Set
ZV <- nearZeroVar(Train_Data, freqCut = 100/0) 
Train_Data[,ZV] <- NULL
ZV <- nearZeroVar(Val_Data, freqCut = 100/0) 
Val_Data[,ZV] <- NULL

#keep columns in the training set that are meaningful in the validation set
Same_Col <- intersect(colnames(Train_Data),colnames(Val_Data))
Val_Data <- Val_Data[,Same_Col]
Train_Data <- Train_Data[,Same_Col]

#check for missing values
sum(is.na(Train_Data))

#Set right format for date variable
Train_Data$TIMESTAMP <- as.POSIXct(Train_Data$TIMESTAMP, origin = "1970-01-01")
Val_Data$TIMESTAMP <- as.POSIXct(Val_Data$TIMESTAMP, origin = "1970-01-01")

#Create unique identifier
Train_Data <- Train_Data %>% unite(Uni_ID_POS, BUILDINGID, FLOOR, sep="_", remove = FALSE)
Train_Data$Uni_ID_POS <- as.factor(Train_Data$Uni_ID_POS)

Val_Data <- Val_Data %>% unite(Uni_ID_POS, BUILDINGID, FLOOR, sep="_", remove = FALSE)
Val_Data$Uni_ID_POS <- as.factor(Val_Data$Uni_ID_POS)

#change variable types
Train_Data$FLOOR <- ordered(Train_Data$FLOOR)
Train_Data$BUILDINGID <- as.factor(Train_Data$BUILDINGID)
Val_Data$FLOOR <- ordered(Val_Data$FLOOR)
Val_Data$BUILDINGID <- as.factor(Val_Data$BUILDINGID)

ID <- colnames(select(Train_Data, -contains("WAP")))
str(Train_Data[,ID])

#Unique observation per row
Train_Data$Diff_Obs <- apply(Train_Data[,grep("WAP",colnames(Train_Data),value = TRUE)]
                             , 1, function(x)sum(x<120))

Val_Data$Diff_Obs <- apply(Val_Data[,grep("WAP",colnames(Val_Data),value = TRUE)]
                           , 1, function(x)sum(x<120))

#Remove duplicates
Val_Data <- unique(Val_Data)
Train_Data <- unique(Train_Data)