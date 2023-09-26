setwd ("Z:\\Project_Top2B\\April_2023\\Horvath")
df <- read.csv("Merged_Horvath_Healthy_Mean.csv", header = T)
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

cor(output$Age, output$PredAge, method = "pearson") # 0.9763849

#write.csv(output, "BorutaPy_Age_enet_10FCV.csv", row.names = F)

###### Saving the model
saveRDS(Top2_Model, file = "Model_Horvath_enet.RData")
# Loading the model
TheModel <- readRDS("Model_Horvath_enet.RData") 






###  Healthy  ###
NewData = df
head(NewData[1:4,1:9])

CpgNames = colnames(df[,2:354]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.33)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predct_CorrelationETC_Healthy_Horvath.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_Healthy_Horvath.csv', row.names = F)





### Diseased - Schizophrenia ###
NewData <- vroom("GSE74193_Schiz_Horvath_Age.csv")

CpgNames <- colnames(df[, 2:354])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predict_CorrelationETC_ModelHorvath_Schizophrenia.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_ModelHorvath_Schizophrenia.csv', row.names = F)








### Diseased - AD ###
NewData <- vroom("AD_Horvath_Age.csv")
NewData <- NewData  %>% select(-c(1))

CpgNames <- colnames(df[, 2:354])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predict_CorrelationETC_ModelHorvath_AD.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_ModelHorvath_AD.csv', row.names = F)









### Diseased - All ###
setwd ('Z:\\Project_Top2B\\April_2023\\Horvath\\Diseased')
NewData <- vroom("Merged_Horvath_Diseased_Mean.csv")

CpgNames <- colnames(df[, 2:354])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predict_CorrelationETC_ModelHorvath_DiseasedAll.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_ModelHorvath_DiseasedAll.csv', row.names = F)



















###################  Our Model  ########################
setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow")
df <- read.csv("CpG_236BorutaPy_onAll.csv", header = T)
TheModel <- readRDS("Model_Top2B_enet_MergedData.RData") 



### Diseased - Schizophrenia ###
setwd ("Z:\\Project_Top2B\\April_2023\\diseasewise\\schiz")
NewData <- vroom("GSE74193_236CpG_Schiz_Age.csv")

CpgNames <- colnames(df[, 2:237])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predict_CorrelationETC_OurModel_DataSetHorvath_Schizophrenia.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_OurModel_DataSetHorvath_Schizophrenia.csv', row.names = F)








### Diseased - AD ###
setwd ("Z:\\Project_Top2B\\April_2023\\diseasewise\\AD")
NewData <- vroom("Merged_236_AD_Age_Mean.csv")

CpgNames <- colnames(df[, 2:237])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predict_CorrelationETC_OurModel_DataSetHorvath_AD.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_OurModel_DataSetHorvath_AD.csv', row.names = F)







### Diseased - All ###
setwd ("Z:\\Project_Top2B\\April_2023\\Horvath\\From_OurModel")
NewData <- vroom("Merged_236_Diseased_Mean.csv")

CpgNames <- colnames(df[, 2:237])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predict_CorrelationETC_OurModel_DataSetHorvath_All.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_OurModel_DataSetHorvath_All.csv', row.names = F)





### Healthy - All ###
setwd ("Z:\\Project_Top2B\\April_2023\\Horvath\\Healthy_236")
NewData <- vroom("Merged_236_Healthy_Mean.csv")

CpgNames <- colnames(df[, 2:237])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predict_CorrelationETC_OurModel_DataSetHorvath_Healthy.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_OurModel_DataSetHorvath_Healthy.csv', row.names = F)

