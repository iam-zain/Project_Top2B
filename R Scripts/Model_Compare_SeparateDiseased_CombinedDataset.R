setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow") 
df <- read.csv("CpG_236BorutaPy_onAll.csv", header = T)
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_MergedData.RData") 

###### New Data Set for Testing the model accuracy



###  Diseased - All  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Diseased_All")
NewData <- vroom("Merged_Top2B_DiseasedMean1.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
#NewData <- NewData  %>% select(-c(1))
sum(is.na(NewData))

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
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.9108337

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'MergedDiseased_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - Diseased [r = 0.9108337]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))






###  Schiz   ###
setwd ("Z:\\Project_Top2B\\April_2023\\diseasewise\\schiz")
NewData <- vroom("Merged_Schiz_Top2B_AgeMean.csv")
sum(is.na(NewData))
NewData <- na.omit(NewData) # Removing NAs

head(NewData[1:4,1:9])
# Drop 1st (serial number)column
#NewData <- NewData  %>% select(-c(1))

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
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.845626

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
#write.csv(Age_Compare, 'GSE134379_cb_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - Schizophrenia [r = 0.845626]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))





###  AD   ###
setwd ("Z:\\Project_Top2B\\April_2023\\diseasewise\\AD")
NewData <- vroom("Merged_AD_Top2B_AgeMean.csv")
sum(is.na(NewData))
NewData <- na.omit(NewData) # Removing NAs

head(NewData[1:4,1:9])
# Drop 1st (serial number)column
#NewData <- NewData  %>% select(-c(1))

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
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.6352856

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'Merged_AD_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - AD [r = 0.6352856]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))


