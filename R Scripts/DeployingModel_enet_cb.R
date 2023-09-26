setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\cerebellum")
df <- read.csv("CpG_49BorutaPy_onAll_cb.csv", header = T)
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

cor(output$Age, output$PredAge, method = "pearson") # 0.8806811

write.csv(output, "BorutaPy_Age_enet_10FCV_cb.csv", row.names = F)

###### Saving the model
saveRDS(Top2_Model, file = "Model_Top2B_enet_cb.RData")
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_cb.RData") 



###### New Data Set for Testing the model accuracy



###  Diseased - cb  ###
setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\cerebellum\\Diseased")
NewData <- vroom("Merged_Top2B_Cerebellum_DiseasedMean.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
#NewData <- NewData  %>% select(-c(1))
sum(is.na(NewData))

CpgNames = colnames(df[,2:50]) # Selecting Column names from our model
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
cor(SelectCpGs$OriginalAge, SelectCpGs$PredictAge, method = "pearson") #Checking correlation # 0.780312

OriginalAge = SelectCpGs$OriginalAge
PredictAge = SelectCpGs$PredictAge
Age_Compare = data.frame(OriginalAge, PredictAge)
Age_Compare = Age_Compare[order(Age_Compare$OriginalAge), ]
write.csv(Age_Compare, 'Merged_cb_Diseased_AgeCompare.csv', row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(OriginalAge, PredictAge)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'OriginalAge') +
  labs(x = "Chronological Age", y= "Predicted Age\n") + 
  ggtitle("Age Comparison Plot - cb [r = 0.780312]") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 5)) +
  scale_y_continuous (breaks = seq(0, 120, by = 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))


