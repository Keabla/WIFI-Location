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

source("Scripts/Step_2_Data_Exploration.R")

#Grep function to get the WAP columns
WAP_Train <- grep("WAP",colnames(Train_Data),value = TRUE)
WAP_Val <- grep("WAP",colnames(Val_Data),value = TRUE)


#keep columns in the training set that are meaningful in the validation set
Same_Col <- intersect(colnames(Train_Data),colnames(Val_Data))
Val_Data <- Val_Data[,Same_Col]
Train_Data <- Train_Data[,Same_Col]

#check for missing values
sum(is.na(Train_Data))

#Drop variables that are not useful for our analysis as they are not useful for our analysis
Train_Data$TIMESTAMP <- NULL

#Drop PhoneID as there are 
length(unique(Train_Data$PHONEID)) == length(unique(Val_Data$PHONEID))
Train_Data$PHONEID <- NULL

ID <- colnames(select(Train_Data, -contains("WAP")))
str(Train_Data[,ID])

#Drop any row that has less than 4 unique observations, as one of them is -120 bmp (Absence of Signal) and
#the rest we need at least three observations to create a model because of True range multilateration 
Train_Data <- Train_Data %>% filter(Diff_Obs >3)
Train_Data <- Train_Data %>% mutate(id_row = row_number())
Val_Data <- Val_Data %>% filter(Diff_Obs > 3)

#Make sure that the two sets have the same columns
Same_Col <- intersect(colnames(Train_Data),colnames(Val_Data))
Train_Data <- Train_Data[,Same_Col]
Val_Data <- Val_Data[,Same_Col]

#### Merge Datasets ####

#Merge the two dataset
Total_DT <- rbind(Train_Data,Val_Data)
Total_DT$N_Row <- seq.int(nrow(Total_DT))

#Sample of Total Dataset Separated
Count_Table <- Total_DT %>% group_by(Uni_ID_POS) %>% summarise(count=n())
N_Sampling <- min(Count_Table$count)
Sample_Total <- Total_DT %>% group_by(Uni_ID_POS) %>% sample_n(N_Sampling)
Count_Table <- Sample_Total %>% group_by(Uni_ID_POS) %>% summarise(count=n())

#Divide total Data Set into Training and Validation Set, with 
# the sample set being the training set and the rest validation
Which_Row <- Total_DT$N_Row
Which_No_Row <- Sample_Total$N_Row
Which_Row <- Which_Row[-Which_No_Row] 
No_Sample_DT <- Total_DT[Which_Row,]

nrow(Total_DT)
nrow(Sample_Total)
nrow(No_Sample_DT)

## 75% of the sample size
#smp_size <- floor(0.75 * nrow(Sample_Total))

## set the seed to make your partition reproducible
#set.seed(123)
# train_ind <- sample(seq_len(nrow(Sample_Total)), size = smp_size)

# train <- Sample_Total[train_ind, ]
# test <- Sample_Total[-train_ind, ]

train <- Sample_Total
test <- No_Sample_DT

#check strings for no WAP columns
ID <- colnames(select(train, -contains("WAP")))
str(train[,ID])
str(test[,ID])