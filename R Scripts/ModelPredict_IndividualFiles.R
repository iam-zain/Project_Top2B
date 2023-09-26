setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow") 
df <- read.csv("CpG_236BorutaPy_onAll.csv", header = T)
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_MergedData.RData") 


###### New Data Set for Testing the model accuracy



###  AD - cb  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE134379")
NewData <- vroom("GSE134379_Top2b_cb_AD.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE134379.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE134379.csv', row.names = F)




###  AD - mtg  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE134379")
NewData <- vroom("GSE134379_Age_MTG_AD_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_mtgGSE134379.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_mgGSE134379.csv', row.names = F)



###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE76105")
NewData <- vroom("GSE76105_AD_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE76105.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE76105.csv', row.names = F)



###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE125895")
NewData <- vroom("GSE125895_AD_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE125895.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE125895.csv', row.names = F)





###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE66351")
NewData <- vroom("GSE66351_AD_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE66351.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE66351.csv', row.names = F)





###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE109627")
NewData <- vroom("GSE109627_AD_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE109627.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE109627.csv', row.names = F)




###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE80970")
NewData <- vroom("GSE80970_AD_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE80970.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE80970.csv', row.names = F)





###  AD  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE105109")
NewData <- vroom("GSE105109_AD_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE105109.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE105109.csv', row.names = F)




###  MS  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE40360")
NewData <- vroom("GSE40360_MS_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE40360.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE40360.csv', row.names = F)






###  Autism  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE53162")
NewData <- vroom("GSE53162_Auti_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE53162.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE53162.csv', row.names = F)





###  Schizophrenia  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE61380")
NewData <- vroom("GSE61380_Schiz_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE61380.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE61380.csv', row.names = F)





###  Schizophrenia  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE61431")
NewData <- vroom("GSE61431_Schiz_Top2b.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE61431.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE61431.csv', row.names = F)






###  Schizophrenia  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE89702")
NewData <- vroom("GSE89702_Top2b_Schiz_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE89702.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE89702.csv', row.names = F)





###  Schizophrenia  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE74193")
NewData <- vroom("GSE74193_Top2b_Schiz.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE74193.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE74193.csv', row.names = F)




###  HIV  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE59457")
NewData <- vroom("GSE59457_HIV_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE59457.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE59457.csv', row.names = F)






###  HIV  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE67748")
NewData <- vroom("GSE67748_HIV_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE67748.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE67748.csv', row.names = F)







###  HIV  ###
setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\GSE67749")
NewData <- vroom("GSE67749_HIV_Top2b_Age.csv")
head(NewData[1:4,1:9])
# Drop 1st (serial number)column
NewData <- NewData  %>% select(-c(1))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
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


write.csv(results_df, 'BS_Predct_CorrelationETC_GSE67749.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_GSE67749.csv', row.names = F)

