setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow") 
df <- read.csv("CpG_236BorutaPy_onAll.csv", header = T)
#df <- df  %>% select(-c(1)) #Remove PATNO
output <- data.frame(matrix(ncol=2,nrow=0))

set.seed(1)
train <- createDataPartition(df [,"Age"], p=0.8, list = F) 
data_train <- df[train,]
data_test <- df[-train,]
control <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
Top2_Model <- train (Age~., data = data_train, method = 'enet',trControl = control)
print(Top2_Model)
pred <- predict(Top2_Model, data_test, method = "response")

data_test$PredAge = pred
output = data_test [,c('Age','PredAge')] # Selecting Age & Pred Age

cor(output$Age, output$PredAge, method = "pearson") # 0.9397128

write.csv(output, "temp_Merged_BorutaPy_Age_enet_10FCV.csv", row.names = F)

###### Saving the model
saveRDS(Top2_Model, file = "Model_Top2B_enet_MergedData.RData")
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_MergedData.RData") 



###### New Data Set for Testing the model accuracy



###  AD - cb  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE134379")
NewData <- vroom("GSE134379_Top2b_cb_AD.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.6321707

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'GSE134379_cb_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE134379 cb AD [r = 0.6321707]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




###  AD - mtg  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE134379")
NewData <- vroom("GSE134379_Age_MTG_AD_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # -0.0666204

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
#write.csv(Age_Compare, 'GSE134379_mtg_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE134379 mtg AD [r = -0.0666204]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))



###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE76105")
NewData <- vroom("GSE76105_AD_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.6073925


OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
#write.csv(Age_Compare, 'GSE76105_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE76105 AD [r = 0.6073925]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))



###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE125895")
NewData <- vroom("GSE125895_AD_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.7774201


OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
#write.csv(Age_Compare, 'GSE125895_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE125895 AD [r = 0.7774201]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))





###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE66351")
NewData <- vroom("GSE66351_AD_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.6597235


OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
#write.csv(Age_Compare, 'GSE66351_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE66351 AD [r = 0.6597235]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))





###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE109627")
NewData <- vroom("GSE109627_AD_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.5228891

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
#write.csv(Age_Compare, 'GSE109627_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE109627 AD [r = 0.5228891]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE80970")
NewData <- vroom("GSE80970_AD_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.6229957

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'GSE80970_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE80970 AD [r = 0.6229957]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))





###  MS  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE40360")
NewData <- vroom("GSE40360_MS_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.7866513

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'GSE40360_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE40360 MS [r = 0.7866513]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))





###  Autism  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE53162")
NewData <- vroom("GSE53162_Auti_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.8645099

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'GSE53162_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE53162 Autism [r = 0.8645099]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




###  Schizophrenia  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE61380")
NewData <- vroom("GSE61380_Schiz_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.3748943

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'GSE61380_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE61380 Schizophrenia [r = 0.3748943]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




###  Schizophrenia  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE61431")
NewData <- vroom("GSE61431_Schiz_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.8647929

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'GSE61431_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE61431 Schizophrenia [r = 0.8647929]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))





###  Schizophrenia  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE89702")
NewData <- vroom("GSE89702_Top2b_Schiz_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.8590632

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'GSE89702_Schiz_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE89702 Schizophrenia [r = 0.8590632]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




###  Schizophrenia  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE74193")
NewData <- vroom("GSE74193_Top2b_Schiz.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.9012765

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'GSE74193_Schiz_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE74193 Schizophrenia [r = 0.9012765]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




###  HIV  ###
setwd ("Z:\\Project_Top2B\\April_2023\\GSE59457")
NewData <- vroom("GSE59457_HIV_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.7314591


OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'GSE59457_HIV_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - GSE59457 HIV [r = 0.7314591]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))





###  PD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\GSE151355")
NewData <- vroom("GSE151355_PD_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 


OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'GSE151355_PD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(x = OriginalAge)) +
  geom_line(aes(y = OriginalAge, color = "Original Age"), size = 2) +
  geom_line(aes(y = PredictAge, color = "Predicted Age"), size = 2) + 
  theme_minimal_hgrid() + 
  labs(x = "Age", y = "Age", color = " Age") + 
  ggtitle("Age Comparison Plot - GSE151355 PD") +
  scale_x_continuous(breaks = seq(10, 100, by = 5)) +
  scale_y_continuous(breaks = seq(10, 100, by = 5)) +
  scale_color_manual(values = c("blue", "red"), labels = c("Original Age", "Predicted Age")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 2), 
        panel.grid.major.x = element_line(color = "grey", linewidth = 0.2, linetype = 3),
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.2, linetype = 3),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        legend.position = c(0.80, 0.18))  # Set the position of the legend (x, y)





###  PD 850K Blood ###
setwd ("Z:\\Project_Top2B\\Clocks_Analysis\\PD_PPMI")
NewData <- vroom("PPMI_Top2B.csv")
NewData_P = NewData[which(NewData$APPRDX == 1), ]
NewData_H = NewData[which(NewData$APPRDX == 2), ]

##  both patient and healthy
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs

SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.4856152


##  only patient
head(NewData_P[1:4,1:9])
NewData <- NewData_P  %>% select(-c(1))
CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.4839876


OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'PD_PPMI_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(x = OriginalAge)) +
  geom_line(aes(y = OriginalAge, color = "Original Age"), size = 2) +
  geom_line(aes(y = PredictAge, color = "Predicted Age"), size = 2) + 
  theme_minimal_hgrid() + 
  labs(x = "Age", y = "Age", color = " Age") + 
  ggtitle("Age Comparison Plot - PD PPMI") +
  scale_x_continuous(breaks = seq(10, 100, by = 5)) +
  scale_y_continuous(breaks = seq(10, 500, by = 30)) +
  scale_color_manual(values = c("blue", "red"), labels = c("Original Age", "Predicted Age")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 2), 
        panel.grid.major.x = element_line(color = "grey", linewidth = 0.2, linetype = 3),
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.2, linetype = 3),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        legend.position = c(0.08, 0.88))  # Set the position of the legend (x, y)





##  only healthy
head(NewData_H[1:4,1:9])
NewData <- NewData_H  %>% select(-c(1))
CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
SelectCpGs <- SelectCpGsRaw %>% select(-OriginalAge) #Remove Age before prediction
PredictAge <- predict(TheModel, newdata = SelectCpGs) #Using MODEL: TheModel
PredictAge <- as.data.frame(PredictAge) # Putting in one dataframe
SelectCpGs$OriginalAge <- SelectCpGsRaw$OriginalAge # Adding Original Age again
SelectCpGs$PredictAge <- PredictAge # Adding Predicted Age
dim(SelectCpGs)
dim(PredictAge)
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.5296042







